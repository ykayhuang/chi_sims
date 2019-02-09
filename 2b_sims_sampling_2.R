#mod: 20190123

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

##from 2b..._1
df3 <- longdf
thisn <- 40
#df3 <- fread("4_r_output/sims_result_80.csv",sep=",",header=T) %>% 
#  mutate(grp=paste(n_profiles,"ep:",cond.note)) %>% as.data.frame()
df3 <- fread( paste0("4_r_output/sims_result_",thisn,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note)) %>% as.data.frame()
df3$cond_loop <- paste(df3$cond.note,df3$loop.count,sep="-")
#cut to 2
df3 <- subset(df3,n_profiles<3)
##setting up the test variables #then I can just change this one
pro_tested <- c("dist_airport_m","mst_500m","ind_750m","ndvi_300m") #changed

#to get the "true value"
raw_big <- read.table("2_GIS_output/gis_output_clean.csv",sep=",",header=T)
df2 <- raw_big
#fill NA with 0 (only for the one should have values)
df2$ind_500m <- ifelse(is.na(df2$ind_500m),0,df2$ind_500m)
df2$ind_750m <- ifelse(is.na(df2$ind_750m),0,df2$ind_750m)
df2$mst_500m <- ifelse(is.na(df2$mst_500m),0,df2$mst_500m)
df2$mst_750m <- ifelse(is.na(df2$mst_750m),0,df2$mst_750m)

#get the 0,5,25,50,75,95,100,mean,sd,iqr, and make a data frame
df_truevs <- cbind( sapply(df2, min,na.rm=T),
                     sapply(df2, quantile,probs=.5,na.rm=T),
                     sapply(df2, quantile,probs=.25,na.rm=T),
                     sapply(df2, quantile,probs=.50,na.rm=T),
                     sapply(df2, quantile,probs=.75,na.rm=T),
                     sapply(df2, quantile,probs=.95,na.rm=T),
                     sapply(df2, max,na.rm=T),sapply(df2, mean,na.rm=T),sapply(df2, sd,na.rm=T)) %>% 
                  as.data.frame()%>%  `colnames<-`(c("min","p5","q1","p50","q3","p95","max","mean","sd")) %>% 
                  mutate(iqr=q3-q1) %>%  `rownames<-`(colnames(df2)) %>% .[-1,]

#try plot something
#simple
#p <- ggplot(tf_p, aes(grp,dist_airport_m,color=factor(pros))) + geom_boxplot()
#p

#1 . with funcions with saving
fun_drawplot_iqr <- function(tf_var){
tf_p <- df4_iqr #for ploting
tfv_25 <- tf_p[which(tf_p$pros==0),tf_var] %>% quantile(probs=.25,na.rm=T) %>% as.numeric
tfv_75 <- tf_p[which(tf_p$pros==0),tf_var] %>% quantile(probs=.75,na.rm=T) %>% as.numeric
tfv_min <- tf_p[which(tf_p$pros==0),tf_var] %>% min(na.rm=T) %>% as.numeric
tfv_max <- tf_p[which(tf_p$pros==0),tf_var] %>% max(na.rm=T) %>% as.numeric
tfv_true_iqr <- (df2[,tf_var] %>% quantile(probs=.75,na.rm=T) %>% as.numeric)-(df2[,tf_var] %>% quantile(probs=.25,na.rm=T) %>% as.numeric)
p <- ggplot(tf_p, aes(grp,tf_p[,tf_var],fill=factor(pros))) + geom_boxplot()
#add big 25,75 line
p <- p + geom_hline(yintercept=tfv_25, linetype="dashed", 
                    color = "red")
p <- p + geom_hline(yintercept=tfv_75, linetype="dashed", 
                    color = "red")
p <- p + geom_hline(yintercept=tfv_min, linetype="solid", 
                     color = "red")
p <- p + geom_hline(yintercept=tfv_max, linetype="solid", 
                    color = "red")
p <- p + geom_hline(yintercept=tfv_true_iqr, linetype="solid", size=2,
                    color = "blue")
p <- p+labs(y = paste("The variable:",tf_var),x="group (by differemnt exposure profiles)",
            fill="groups (by how many exposure profiles)",
            title=paste("Boxplots show IQR for", tf_var, "in 500 simulations by groups"),
            subtitle="red dashed lines show the IQR of the variable when all random, and red solid lines shows the range of the variable when all random")
p <- p+theme_bw()+coord_flip() #+ theme(legend.position="bottom")
p
ggsave(paste0("4_r_output/1_",tf_var,"_IQR.png"),p,width=12)
}
colnames(df4_iqr)
fun_drawplot_iqr("dist_airport_m")
fun_drawplot_iqr("ndvi_300m")
fun_drawplot_iqr("ind_750m")
fun_drawplot_iqr("mst_500m")
#

#2. make a bar chart across different exposure profiles
fun_drawplot_bar_byv <- function(p_var){
df5 <- df3 %>% .[,c(p_var,"grp")] %>%`colnames<-`(c("x","grp"))
df5$inq <- ifelse(df5$x<=df_truevs[p_var,"q1"],1,
            ifelse(df5$x<=df_truevs[p_var,"p50"],2,
             ifelse(df5$x<=df_truevs[p_var,"q3"],3,4)))
#dummies = model.matrix(~factor(df5$inq))
df5$q1 <- (df5$inq==1)
df5$q2 <- (df5$inq==2)
df5$q3 <- (df5$inq==3)
df5$q4 <- (df5$inq==4)
z <- aggregate(df5[,c("q1","q2","q3","q4")],by=list(df5$grp),mean,na.rm=T) %>% mutate(cq2=q1+q2) %>%
        mutate(cq3=cq2+q3) %>% mutate(cq4=cq3+q4)
z_long <- gather(z, cond, mp, q1:q4, factor_key=TRUE)
z_long$cond <- factor(z_long$cond, levels = c("q4","q3","q2","q1"))
p <- ggplot(z_long,aes(x=factor(Group.1),y=as.numeric(mp),fill=factor(cond))) +geom_bar(stat = "identity")
#ref: http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html
#plyr
z_long <- ddply(z_long, .(Group.1),
                     transform, pos = cumsum(mp) - (0.5 * mp)) 

p <- p + geom_text(data=z_long, aes(x=factor(Group.1),y=pos,label = paste0( round(mp*100,1),"%")), size=3)
#p <- p + geom_hline(yintercept=c(0.25,0.5,0.75), linetype="dashed", color = "black")
if(str_detect(p_var,"ind")){
  p <- p + geom_hline(yintercept=c(0.557,0.75), linetype="dashed", color = "black")
}else{
  p <- p + geom_hline(yintercept=c(0.25,0.5,0.75), linetype="dashed", color = "black")
}
p <- p <- p+labs(y = paste("cumulative percentage of",p_var,"in each four quartile"),x="group (by different exposure profiles)",
                 fill="color by quartile",
                 title=paste("Bar chart shows how many", p_var, "is in different quartiles in 500 simulations by different exposure profiles"),
                 subtitle="black dashed lines show the 0.25/0.50/0.75 in cumulative percentage")
p <- p+theme_bw()+coord_flip()
p
ggsave(paste0("4_r_output/2_",p_var,"_bar.png"),p,width=12)
}
fun_drawplot_bar_byv("dist_airport_m")
fun_drawplot_bar_byv("ndvi_300m")
fun_drawplot_bar_byv("mst_500m")
fun_drawplot_bar_byv("ind_750m")

#3. draw by ep, within exposure profiiles
fun_drawplot_bar_byep <- function(pick_cond,pnn){
df5 <- df3 %>% .[which(.$cond.note==pick_cond),] 
df5 <- df5[,c("oid",pro_tested,"loop.count","cond.note","n_profiles","cond_loop","grp")]
get3q <- function(var){
  c(df_truevs[var,"q1"],df_truevs[var,"p50"],df_truevs[var,"q3"])
}
df6 <- cbind(df5,t(rep(NA,length(pro_tested))))
colnames(df6)[which(colnames(df6)==1):dim(df6)[2]] <- paste0(pro_tested,"_q")
for(v in pro_tested){
df6[,paste0(v,"_q")] <- ifelse(df6[,v]<=get3q(v)[1],1,
                         ifelse(df6[,v]<=get3q(v)[2],2,
                                ifelse(df6[,v]<=get3q(v)[3],3,4)))
df6 <- cbind(df6,t(rep(NA,4)))
colnames(df6)[which(colnames(df6)==1):dim(df6)[2]] <- paste0(v,"_q",1:4)
#dummies
df6[,paste0(v,"_q1")] <- df6[,paste0(v,"_q")]==1
df6[,paste0(v,"_q2")] <- df6[,paste0(v,"_q")]==2
df6[,paste0(v,"_q3")] <- df6[,paste0(v,"_q")]==3
df6[,paste0(v,"_q4")] <- df6[,paste0(v,"_q")]==4
}
#dummies = model.matrix(~factor(df5$inq))
z <- aggregate(df6[,c(str_which(colnames(df6),"_q1"),
                      str_which(colnames(df6),"_q2"),
                      str_which(colnames(df6),"_q3"),
                      str_which(colnames(df6),"_q4"))],by=list(df6$grp),mean,na.rm=T) %>%.[,-1] %>%t
z2 <- cbind(str_sub(rownames(z), end= -4),as.numeric(z),str_sub(rownames(z), start= -2)) %>% as.data.frame%>%`colnames<-`(c("va","mp","qr"))
z2$mp <- as.numeric(as.character(z2$mp))
z2$qr <- factor(z2$qr, levels = c("q4","q3","q2","q1"))
#
p <- ggplot(z2,aes(x=factor(va),y=(mp),fill=factor(qr))) +geom_bar(stat = "identity")
#plyr
z2 <- ddply(z2, .(va),  transform, pos = cumsum(mp) - (0.5 * mp)) 

p <- p + geom_text(data=z2, aes(x=factor(va),y=pos,label = paste0( round(mp*100,1),"%")), size=3)
p <- p + geom_hline(yintercept=c(0.25,0.5,0.75), linetype="dashed", 
                    color = "black")
p <- p <- p+labs(y = paste("cumulative percentage of variable in each four quartiles"),x="exposure profiles",
                 fill="color by quartile",
                 title=paste("Bar chart shows the composition of exposure profiles by different quartiles in 500 simulations under the condition of",pick_cond),
                 subtitle="black dashed lines show the 0.25/0.50/0.75 in cumulative percentage")
p <- p+theme_bw()+coord_flip()
p
ggsave(paste0("4_r_output/3_vars_bar",pnn,".png"),p,width=12)
}
#fun_drawplot_bar_byep("all random",1)

ep_test_list <- table(df3$grp) %>% names() %>% .[which(str_sub(table(df3$grp) %>% names,end=1) %in%c("0","1","2"))]
for(i in 1:length(ep_test_list)){
  fun_drawplot_bar_byep(str_sub(ep_test_list, start= 7)[i],i)  
}