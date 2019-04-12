#this is the part that carry out after the committee meeting
#two task: 1. 
#          2. comparing distribution 
###DO NOT RUN THE SIMS!!!
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
##setting up the test variables #then I can just change this one
pro_tested <- c("dist_airport_m","mst_500m","ind_750m","ndvi_300m") #changed

#compare distribution
#make a figure first

#figure 6. make the figure showing distribution
# library(car)
# qqPlot(df2$dist_airport_m)
af40 <- fread( paste0("4_r_output/sims_result_",40,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note),ln=loop.count+4000) %>% as.data.frame()

af80 <- fread( paste0("4_r_output/sims_result_",80,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note),ln=loop.count+8000) %>% as.data.frame()

# # testing
# # pick_cond <- "all random"
# # df5 <- rbind(af40,af80) %>% .[which(.$cond.note==pick_cond),]
# mf1 <- cbind(df5$dist_airport_m,df5$mst_500m,df5$ind_750m,df5$ndvi_300m,df5$ln) %>%
#         rbind(.,cbind(df2$dist_airport_m,df2$mst_500m,df2$ind_750m,df2$ndvi_300m,9999)) %>% as.data.frame()
# colnames(mf1) <- c("dist_airport_m","mst_500m","ind_750m","ndvi_300m","loop.count")
# mf1$gg <- ifelse(mf1$loop.count==9999,1,0)
#  p<-qplot(sample = dist_airport_m, data = mf1, color=as.factor(loop.count))
#  p
# # p<-qplot(sample = ndvi_300m, data = mf1, color=as.factor(loop.count))
# # p
# p <- ggplot(mf1, aes(dist_airport_m, color = as.factor(loop.count),alpha = as.factor(loop.count),
#                      size=as.factor(gg))) + stat_ecdf(pad = FALSE)
# p <- p+theme_bw()+scale_color_manual(values=c( rep("black",100),rep("green",100),"red"),guide = F )
# p <- p+scale_alpha_manual(values=c( rep(0.3,100),rep(0.2,100),1),guide = F )
# p <- p+scale_size_manual(values=c(rep(1,200),3),guide=F) #somehow not working
# p <- p + labs(y="dist_airport_m",title="cond:all random/n=40",subtitle="red:true distribution in the city")
# p

#
rlist <- list()
af1 <- rbind(af40,af80)
af1$cond2 <- factor(af1$cond.note, levels = c("all random","bi_airport_5k","bi_mst_500","bi_ind_750","bi_ndvi_300",
                                                                 "bi_mst_500 + bi_airport_5k","bi_ind_750 + bi_airport_5k","bi_airport_5k + bi_ndvi_300",
                                                                 "bi_mst_500 + bi_ind_750", "bi_mst_500 + bi_ndvi_300",
                                                                 "bi_ind_750 + bi_ndvi_300")) #sort the seq that i want, based on original varialbe's value
levels(af1$cond2) <- c("None (simple random)","Air_B","MS_B","IND_B","NDVI_B",
                                   "Air_B + MS_B","Air_B + IND_B","Air_B + NDVI_B",
                                   "MS_B + IND_B","MS_B + NDVI_B",
                                   "IND_B + NDVI_B") 
F_make_the_big_grid <- function(sel_var){
for (i in 1:length(levels(af1$cond2) ) ){
pick_cond <- levels(af1$cond2) [i]
df5 <- af1 %>% .[which(.$cond2==pick_cond),]
mf1 <- df5[,which(colnames(df5)%in%c(sel_var,"ln"))] %>% as.matrix(.) %>% 
        rbind(.,  cbind(df2[,which(colnames(df2)==sel_var)],9999))%>% as.data.frame()
colnames(mf1) <- c("gvar","loop.count")
mf1$gg <- ifelse(mf1$loop.count==9999,1,0)
p <- ggplot(mf1, aes(gvar, color = as.factor(loop.count),alpha = as.factor(loop.count),
                     size=as.factor(gg))) + stat_ecdf(pad = FALSE)
p <- p+theme_bw()+scale_color_manual(values=c( rep("black",100),rep("green",100),"red"),guide = F )
p <- p+scale_alpha_manual(values=c( rep(0.3,100),rep(0.2,100),1),guide = F )
p <- p+scale_size_manual(values=c(rep(1,200),3),guide=F) #somehow not working
p <- p + labs(x = NULL,y="CDF",title=pick_cond)
rlist[[i]] <- p
}
g <- do.call("grid.arrange", c(rlist, ncol=4,top=sel_var))
}
#g <- grid.arrange(unlist(rlist), ncol=4)
for (j in 1:length(pro_tested)){
ggsave(file=paste0("4_r_output/figure/figure7_",j,".png"), F_make_the_big_grid(pro_tested[j]),width = 10,height = 6)
}
#
#for (i in unique(df3$cond.note)){print(i)}
#unused plot
# df5 <- df3 %>% .[which(.$cond.note==pick_cond),]
# mf2 <- subset(df5,loop.count==1)
# x <- mf2$dist_airport_m
# y <- df2$dist_airport_m
# 
# ks.test(x,y)
# 
# plot(ecdf(x = x), main = "ECDF of x and y")
# lines(ecdf(x = y), col = 2)

#part 7. make the qq plot
#
rlist <- list()
af1 <- rbind(af40,af80)
af1$cond2 <- factor(af1$cond.note, levels = c("all random","bi_airport_5k","bi_mst_500","bi_ind_750","bi_ndvi_300",
                                              "bi_mst_500 + bi_airport_5k","bi_ind_750 + bi_airport_5k","bi_airport_5k + bi_ndvi_300",
                                              "bi_mst_500 + bi_ind_750", "bi_mst_500 + bi_ndvi_300",
                                              "bi_ind_750 + bi_ndvi_300")) #sort the seq that i want, based on original varialbe's value
levels(af1$cond2) <- c("None (simple random)","Air_B","MS_B","IND_B","NDVI_B",
                       "Air_B + MS_B","Air_B + IND_B","Air_B + NDVI_B",
                       "MS_B + IND_B","MS_B + NDVI_B",
                       "IND_B + NDVI_B") 
F_make_the_qq_grid <- function(sel_var){
  for (i in 1:length(levels(af1$cond2) ) ){
    pick_cond <- levels(af1$cond2) [i]
    df5 <- af1 %>% .[which(.$cond2==pick_cond),]
    mf1 <- df5[,which(colnames(df5)%in%c(sel_var,"ln"))] %>% as.matrix(.) %>% 
      rbind(.,  cbind(df2[,which(colnames(df2)==sel_var)],9999))%>% as.data.frame()
    colnames(mf1) <- c("gvar","loop.count")
    mf1$gg <- ifelse(mf1$loop.count==9999,1,0)
    p <- qplot(data=mf1, sample=gvar, color = as.factor(loop.count),alpha = as.factor(loop.count),
               size=as.factor(gg))
    p <- p+theme_bw()+scale_color_manual(values=c( rep("black",100),rep("green",100),"red"),guide = F )
    p <- p+scale_alpha_manual(values=c( rep(0.3,100),rep(0.2,100),1),guide = F )
    p <- p+scale_size_manual(values=c(rep(1,200),3),guide=F) #somehow not working
    p <- p + labs(y=NULL,title=pick_cond)
    rlist[[i]] <- p
  }
  g <- do.call("grid.arrange", c(rlist, ncol=4,top=sel_var))
}
#g <- grid.arrange(unlist(rlist), ncol=4)
for (j in 1:length(pro_tested)){
  ggsave(file=paste0("4_r_output/figure/figure8_",j,".png"), F_make_the_qq_grid(pro_tested[j]),width = 10,height = 6)
}

#ks test (Kolmogorov-Smirnov)
F_ks_test <- function(pick_cond){
for (i in 1:100){
  kf2 <- kf1 %>% .[which(.$cond2==pick_cond),] %>% subset(.,loop.count==i)
  ks_p <- rbind(ks_p, c(pick_cond,i,
                        ks.test(kf2[,"dist_airport_m"],df2[,"dist_airport_m"])$p.value,
                        ks.test(kf2[,"mst_500m"],df2[,"mst_500m"])$p.value,
                        ks.test(kf2[,"ind_750m"],df2[,"ind_750m"])$p.value,
                        ks.test(kf2[,"ndvi_300m"],df2[,"ndvi_300m"])$p.value))
}
  ks_s <- rbind(ks_s, c(pick_cond,thisn,
                        table(as.numeric(ks_p[,3])<=0.05)["TRUE"],
                        table(as.numeric(ks_p[,3])<=0.05/100)["TRUE"],
                        table(as.numeric(ks_p[,4])<=0.05)["TRUE"],
                        table(as.numeric(ks_p[,4])<=0.05/100)["TRUE"],
                        table(as.numeric(ks_p[,5])<=0.05)["TRUE"],
                        table(as.numeric(ks_p[,5])<=0.05/100)["TRUE"],
                        table(as.numeric(ks_p[,6])<=0.05)["TRUE"],
                        table(as.numeric(ks_p[,6])<=0.05/100)["TRUE"]))
  ks_s
}
#read the correspnding file by loop from 40 to 80 (temp)
ks_test_sum <- NULL
for (d in 5:25){
  thisn <- d*4
  print(thisn)##
  df3 <- fread( paste0("4_r_output/sims_result_",thisn,".csv"),sep=",",header=T) %>% 
    mutate(grp=paste(n_profiles,"ep:",cond.note)) %>% as.data.frame()
  df3$cond_loop <- paste(df3$cond.note,df3$loop.count,sep="-")
  kf1 <- df3
  kf1$cond2 <- factor(kf1$cond.note, levels = c("all random","bi_airport_5k","bi_mst_500","bi_ind_750","bi_ndvi_300",
                                                "bi_mst_500 + bi_airport_5k","bi_ind_750 + bi_airport_5k","bi_airport_5k + bi_ndvi_300",
                                                "bi_mst_500 + bi_ind_750", "bi_mst_500 + bi_ndvi_300",
                                                "bi_ind_750 + bi_ndvi_300")) #sort the seq that i want, based on original varialbe's value
  levels(kf1$cond2) <- c("None (simple random)","Air_B","MS_B","IND_B","NDVI_B",
                         "Air_B + MS_B","Air_B + IND_B","Air_B + NDVI_B",
                         "MS_B + IND_B","MS_B + NDVI_B",
                         "IND_B + NDVI_B") 
  ks_p <- NULL
  ks_s <- NULL
  ks_test_sum <- rbind(ks_test_sum,
                       lapply(levels(kf1$cond2),F_ks_test) %>% do.call(rbind,.) %>% as.data.frame()  %>%`colnames<-`(c("cond","thisn","air_e","air_00e","ms_e","ms_00e","ind_e","ind_00e","ndvi_e","ndvi_00e"))
  )
}  

ks_test_sum <- sapply(ks_test_sum, as.character) #because some are factor
ks_test_sum[is.na(ks_test_sum)] <- ""
write.table(ks_test_sum,file="ks_test.csv",sep=",",row.names = F)

kf1 <- subset(mf1,loop.count==4001)

ks.test(kf1$gvar,df2$ind_750m)$p.value