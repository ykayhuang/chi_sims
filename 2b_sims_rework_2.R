#this is the rewrite part


#Sys.setlocale("LC_ALL", "English")
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(gridExtra)
library(tidyr)
library(data.table)
library(ff)
library(plyr)
#
pro_tested <- c("dair","mst","ndvi") #changed
#
air <- read.table("2_GIS_output/201904/fc_to_near_airport_ft.txt",sep=",",header=T)
grn <- read.table("2_GIS_output/201904/ndvi_300m.txt",sep=",",header=T)
mst <- read.table("2_GIS_output/201904/fc500mst201904.csv",sep=",",header=T)

mst2 <-aggregate(mst$t, by=list(mst$ORIG_FID), FUN=sum, na.rm=TRUE)%>%  `colnames<-`(c("oid","mst"))
prd2 <-aggregate(prd$F11, by=list(prd$OBJECTID), FUN=sum, na.rm=TRUE)%>%  `colnames<-`(c("oid","prd"))

clean_df <- cbind(air$OBJECTID,air$NEAR_DIST*0.3048,grn$MEAN) %>% 
  as.data.frame %>%  `colnames<-`(c("oid", "dair", "ndvi"))

clean_df2 <- merge(clean_df,mst2,by="oid",all=T) 
clean_df2[is.na(clean_df2)] <- 0

df2 <- clean_df2[,-1]
cor(df2,method="spearman")

cor.test(df2$ndvi,df2$dair)

a32air <- read.table("2_GIS_output/201904/a32_to_near_airport.csv",sep=",",header=T) #in m
a32grn <- read.table("2_GIS_output/201904/a32_ndvi300.csv",sep=",",header=T)
a32mst <- read.table("2_GIS_output/201904/a32mst500m.csv",sep=",",header=T)
a32mst2 <-aggregate(a32mst$t, by=list(a32mst$ORIG_FID), FUN=sum, na.rm=TRUE)%>%  `colnames<-`(c("oid","mst"))

a32df <- cbind(a32grn$OBJECTID,a32air$NEAR_DIST,a32grn$MEAN) %>% 
  as.data.frame %>%  `colnames<-`(c("oid", "dair", "ndvi")) %>% merge(.,a32mst2,by="oid",all=T) 
a32df[is.na(a32df)] <- 0

#a111
t111air <- read.table("2_GIS_output/201904/a111_near_airport.csv",sep=",",header=T) #in m
t111air$oid<-c(1:111)
t111air2 <- subset(t111air,select=c("oid","NEAR_DIST"))%>%  `colnames<-`(c("oid","dair"))
t111grn <- read.table("2_GIS_output/201904/a111_ndvi300.csv",sep=",",header=T) %>% subset(.,select=c("OBJECTID_1","MEAN"))%>%  `colnames<-`(c("oid","ndvi"))

t111mst <- read.table("2_GIS_output/201904/a111_mst500.csv",sep=",",header=T)
t111mst2 <-aggregate(t111mst$t, by=list(t111mst$ORIG_FID), FUN=sum, na.rm=TRUE)%>%  `colnames<-`(c("oid","mst"))

t111df <- merge(t111grn,t111air2,by="oid",all=T) %>% merge(.,t111mst2,by="oid",all=T) 
t111df <- subset(t111df,ndvi!="NA")
t111df[is.na(t111df)] <- 0


#rbind
a32df$c <- 1
clean_df2$c <- 0
t111df$c <- 2

df3 <- rbind(clean_df2,a32df,t111df)

ggplot(df3, aes(sample = dair,color=factor(df3$c))) +  stat_qq() 
ggplot(df3, aes(sample = ndvi,color=factor(df3$c))) +  stat_qq() 
ggplot(df3, aes(sample = mst,color=factor(df3$c))) +  stat_qq() 

##and then I will put the simulation in 
#but before that, I have to modifed a bit
#first,take the orginal, then combine with Air*
af40 <- fread( paste0("4_r_output/sims_201904/sims_result_",40,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note),ln=loop.count+4000) %>% as.data.frame() %>%  #keep some var
  transmute(oid=oid,dair=dair,mst=mst,ndvi=ndvi,loop.count=loop.count,cond.note=cond.note,ln=ln)

bf40 <- fread( paste0("4_r_output/sims_fixed_201904/sims_result_",40,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note),ln=loop.count+4000) %>% as.data.frame() %>% #keep some var
                                          .[which(str_detect(.$cond.note,"airport")),] %>% #keep condition with AirB (this is fixed) in it
  transmute(oid=oid,dair=dair,mst=mst,ndvi=ndvi,loop.count=loop.count,cond.note=cond.note,ln=ln)%>% #note that airB is fixed
                                            mutate(cond.note=ifelse(.$cond.note=="bi_airport_5k","af",
                                                                    ifelse(.$cond.note=="bi_airport_5k + bi_ndvi_300","af+n",
                                                                           ifelse(.$cond.note=="bi_mst_500 + bi_airport_5k","af+m",NA
                                                                    ))))

af80 <- fread( paste0("4_r_output/sims_201904/sims_result_",80,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note),ln=loop.count+8000) %>% as.data.frame()%>% #keep some var
                                          transmute(oid=oid,dair=dair,mst=mst,ndvi=ndvi,loop.count=loop.count,cond.note=cond.note,ln=ln)

bf80 <- fread( paste0("4_r_output/sims_fixed_201904/sims_result_",80,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note),ln=loop.count+8000) %>% as.data.frame() %>% #keep some var
                                          .[which(str_detect(.$cond.note,"airport")),] %>% #keep condition with AirB (this is fixed) in it
  transmute(oid=oid,dair=dair,mst=mst,ndvi=ndvi,loop.count=loop.count,cond.note=cond.note,ln=ln)%>% #note that airB is fixed
  mutate(cond.note=ifelse(.$cond.note=="bi_airport_5k","af",
                          ifelse(.$cond.note=="bi_airport_5k + bi_ndvi_300","af+n",
                                 ifelse(.$cond.note=="bi_mst_500 + bi_airport_5k","af+m",NA
                                 ))))
#merge
af1 <- rbind(af40,af80,bf40,bf80)
af1$cond2 <- factor(af1$cond.note, levels = c("all random","bi_airport_5k","af","bi_mst_500","bi_ndvi_300",
                                              "bi_mst_500 + bi_airport_5k","af+m","bi_airport_5k + bi_ndvi_300","af+n",
                                              "bi_mst_500 + bi_ndvi_300")) #sort the seq that i want, based on original varialbe's value
levels(af1$cond2) <- c("None (simple random)","Air_B","Air_B*","MS_B","NDVI_B",
                       "Air_B + MS_B","Air_B* + MS_B",
                       "Air_B + NDVI_B","Air_B* + NDVI_B","MS_B + NDVI_B") 

af2 <- 
df3 <- rbind(clean_df2,a32df,t111df)

ggplot(df3, aes(sample = dair,color=factor(df3$c))) +  stat_qq() 
ggplot(df3, aes(sample = ndvi,color=factor(df3$c))) +  stat_qq() 
ggplot(df3, aes(sample = mst,color=factor(df3$c))) +  stat_qq() 

F_make_the_qq_grid <- function(sel_var){
  for (i in 1:length(levels(af1$cond2) ) ){
    pick_cond <- levels(af1$cond2) [i]
    df5 <- af1 %>% .[which(.$cond2==pick_cond),]
    mf1 <- df5[,which(colnames(df5)%in%c(sel_var,"ln"))] %>% as.matrix(.) %>% 
      rbind(.,  cbind(clean_df2[,which(colnames(clean_df2)==sel_var)],19999),
            cbind(a32df[,which(colnames(a32df)==sel_var)],29999),
            cbind(t111df[,which(colnames(t111df)==sel_var)],39999))%>% as.data.frame()
    colnames(mf1) <- c("gvar","loop.count")
    mf1$gg <- ifelse(mf1$loop.count==19999,1,  #1=true city
                     ifelse(mf1$loop.count==29999,2, #2=32 current aot
                            ifelse(mf1$loop.count==39999,3,0))) #4=111 furture aot (include 32)
    mf1 <- subset(mf1,gg!=3) #not put in the furture AOT
    p <- qplot(data=mf1, sample=gvar, color = as.factor(loop.count),alpha = as.factor(loop.count),
               size=as.factor(loop.count))
    p <- p+theme_bw()+scale_color_manual(values=c( rep("black",100),rep("green",100),"red","blue"),guide = F )   #red for gg=1, #blue for gg=2/3
    p <- p+scale_alpha_manual(values=c( rep(0.3,100),rep(0.2,100),0.5,0.5),guide = F )
    p <- p+scale_size_manual(values=c(rep(1,200),1.2,2),guide=F) #somehow not working
    p <- p + labs(y=NULL,title=pick_cond)
    rlist[[i]] <- p
  }
  g <- do.call("grid.arrange", c(rlist, ncol=5,top=sel_var))
}

rlist <- list()
for (j in 1:length(pro_tested)){
  ggsave(file=paste0("4_r_output/figure_201904/figure22_",j,".png"), F_make_the_qq_grid(pro_tested[j]),width = 8,height = 5)
}