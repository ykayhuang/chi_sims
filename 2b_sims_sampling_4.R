#mod:20190102: p4,p5 change n into percent
#
#set up n/variable for the analysis // BTW, need to load the df_truevs (2b....sampling_1)
thisn <- 80
##setting up the test variables #then I can just change this one
pro_tested <- c("dist_airport_m","mst_500m","ind_750m","ndvi_300m") #changed

#read the correspnding file
df3 <- fread( paste0("4_r_output/sims_result_",thisn,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note)) %>% as.data.frame()
df3$cond_loop <- paste(df3$cond.note,df3$loop.count,sep="-")
#cut to 2
df3 <- subset(df3,n_profiles<3)
#run the rest

#4. draw the aggreated bar chart to show how the classification (or miscla) across 100 runs
fun_drawmis_agg_bar <- function(pick_cond,sel_var){
  df5 <- df3 %>% .[which(.$cond.note==pick_cond),] 
  
  #
  df5 <- df5[,c("oid",pro_tested,"loop.count","cond.note","n_profiles","cond_loop","grp")]
  get3q <- function(var){
    c(df_truevs[var,"q1"],df_truevs[var,"p50"],df_truevs[var,"q3"])
  }#get3q: obatin the 25,50,75th p from the matrix named df_truevs
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
  zb2 <- aggregate(df6[,c(str_which(colnames(df6),"_q1"),
                        str_which(colnames(df6),"_q2"),
                        str_which(colnames(df6),"_q3"),
                        str_which(colnames(df6),"_q4"))],by=list(df6$grp,df6$loop.count),mean,na.rm=T) %>%.[,-c(1,2)] %>%t
  
  z3 <- cbind(rep(str_sub(rownames(zb2), end= -4),100),
                    rep(str_sub(rownames(zb2), start= -2),100),
                    as.vector(zb2),
                    rep(1:100,each=16))%>% as.data.frame%>%`colnames<-`(c("va","qr","mp","loopc"))
  
  z3$mp <- as.numeric(as.character(z3$mp))
  z3$qr <- factor(z3$qr, levels = c("q4","q3","q2","q1"))
  
  z2 <- subset(z3,va==sel_var)
  #
  p <- ggplot(z2,aes(x=factor(loopc),y=(mp),fill=factor(qr))) +geom_bar(stat = "identity")
  #plyr
  #z2 <- ddply(z2, .(va),  transform, pos = cumsum(mp) - (0.5 * mp))  / adjust to the middle
  #p <- p + geom_text(data=z2, aes(x=factor(va),y=pos,label = paste0( round(mp*100,1),"%")), size=3)
  p <- p + geom_hline(yintercept=c(0.25,0.5,0.75), linetype="dashed", 
                      color = "black")
  p <- p <- p+labs(y = paste("cumulative percentage of variable in each four quartiles"),x="exposure profiles",
                   fill="color by quartile",
                   title=paste("Bar chart shows the composition of exposure profiles by different quartiles in 500 simulations under the condition of",pick_cond),
                   subtitle="black dashed lines show the 0.25/0.50/0.75 in cumulative percentage")
  p <- p+theme_bw()+coord_flip()
  p
}
  #ggsave(paste0("4_r_output/3_vars_bar",pnn,".png"),p,width=12)

fun_drawmis_agg_bar("bi_airport_5k","dist_airport_m")

ep_test_list <- table(df3$grp) %>% names() %>% .[which(str_sub(table(df3$grp) %>% names,end=1) %in%c("0","1","2"))]
for(i in 1:length(ep_test_list)){
  p <- fun_drawmis_agg_bar(str_sub(ep_test_list, start= 7)[i],pro_tested[1])  
  p <- ggsave(paste0("4_r_output/4_vars_miss",pro_tested[1],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=12)
  p <- fun_drawmis_agg_bar(str_sub(ep_test_list, start= 7)[i],pro_tested[2])  
  p <- ggsave(paste0("4_r_output/4_vars_miss",pro_tested[2],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=12)
  p <- fun_drawmis_agg_bar(str_sub(ep_test_list, start= 7)[i],pro_tested[3])  
  p <- ggsave(paste0("4_r_output/4_vars_miss",pro_tested[3],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=12)
  p <- fun_drawmis_agg_bar(str_sub(ep_test_list, start= 7)[i],pro_tested[4])  
  p <- ggsave(paste0("4_r_output/4_vars_miss",pro_tested[4],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=12)
  #p <- fun_drawmis_agg_bar(str_sub(ep_test_list, start= 7)[i],pro_tested[5])  
  #p <- ggsave(paste0("4_r_output/4_vars_miss",pro_tested[5],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=12)
  print(i)
}

#5. tell how many misclassified (abs difference between given value of each)
fun_count_misclass <- function(pick_cond,sel_var){
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
                        str_which(colnames(df6),"_q4"))],by=list(df6$loop.count),mean,na.rm=T) %>%.[,-1] %>%t
  #nacountbyloop <- aggregate(tcm_500m~loop.count, data=df6, function(x) {sum(is.na(x))}, na.action = NULL) %>% .[,2]
    
  tz1 <- z[rownames(z) %>% str_detect(.,sel_var) %>% which(.==T),] %>% t %>% as.data.frame()
  if(str_detect(sel_var,"ind")){
    tz2 <- tz1*thisn
    tz3 <- cbind(abs(tz2[,1]-round(thisn*.556)),abs(tz2[,3]-round(thisn*.194)),abs(tz2[,4]-thisn/4)) %>% rowSums()
    out <- tz3/2
  }else if(str_detect(sel_var,"tcm")){
    rightn <- thisn-nacountbyloop
    tz2 <- tz1*rightn
    tz3 <- cbind(abs(tz2[,1]-round(rightn/4,1)),abs(tz2[,2]-round(rightn/4,1)),abs(tz2[,3]-round(rightn/4,1)),abs(tz2[,4]-round(rightn/4,1)))%>% rowSums()
    out <- tz3/2
  }else if(str_detect(sel_var,"mst_500")){ #need to rearange the prob for the q1 and q2 
    tz2 <- tz1*thisn
    tz3 <- cbind(abs(tz2[,1]-round(thisn*0.47,1)),abs(tz2[,2]-round(thisn*0.03,1)),abs(tz2[,3]-round(thisn/4,1)),abs(tz2[,4]-round(thisn/4,1)))%>% rowSums()
    out <- tz3/2
  }else if(str_detect(sel_var,"mst_750")){ #need to rearange the prob for the q1 and q2 
    tz2 <- tz1*thisn
    tz3 <- cbind(abs(tz2[,1]-round(thisn*0.32,1)),abs(tz2[,2]-round(thisn*0.18,1)),abs(tz2[,3]-round(thisn/4,1)),abs(tz2[,4]-round(thisn/4,1)))%>% rowSums()
    out <- tz3/2
  }else{
    tz2 <- tz1*thisn
    tz3 <- cbind(abs(tz2[,1]-thisn/4),abs(tz2[,2]-thisn/4),abs(tz2[,3]-thisn/4),abs(tz2[,4]-thisn/4))%>% rowSums()
    out <- tz3/2
  }
  out
}
  #end fun, run results
ep_test_list <- table(df3$grp) %>% names() %>% .[which(str_sub(table(df3$grp) %>% names,end=1) %in%c("0","1","2"))]
c_5vs <- as.list(pro_tested)  #this one is not a factor, is a list 
pp <- NULL
mm <- NULL
for(i in 1:length(ep_test_list)){
  #p3 <- kapp_byep(str_sub(z3, start= 7)[i],i,x) %>% as.numeric() %>% c(x,str_sub(z3, start= 7)[i],.)
  #pp <- rbind(pp, p3)
  cond <- str_sub(ep_test_list, start= 7)[i]
  p3 <- lapply(c_5vs, fun_count_misclass,pick_cond=cond) %>% do.call(rbind,.) %>% t  %>% as.data.frame()  %>% cbind(cond,c(1:100),.)%>%`colnames<-`(c("cond","loop.count","mis_airport","mis_mst","mis_ind","mis_ndvi"))
  pp <- rbind(pp, p3)
  p4 <- c(mean(p3[,3]),sd(p3[,3]),max(p3[,3]),
          mean(p3[,4]),sd(p3[,4]),max(p3[,4]),
          mean(p3[,5]),sd(p3[,5]),max(p3[,5]),
          mean(p3[,6]),sd(p3[,6]),max(p3[,6]))/thisn*100 #change n into percent
  p5 <- c(cond,p4)
  mm <- rbind(mm,p5)
  print(i)
}
z4 <- pp %>% as.data.frame()
z5 <- mm %>% as.data.frame()%>%`colnames<-`(c("cond","MEAN in mis_airport","SD in mis_airport","MAX in mis_airport",
                                              "MEAN in mis_mst","SD in mis_mst","MAX in mis_mst",
                                              "MEAN in mis_ind","SD in mis_ind","MAX in mis_ind",
                                              "MEAN in mis_ndvi","SD in mis_ndvi","MAX in mis_ndvi"))%>%`rownames<-`(NULL)
write.table(z4,paste0("4_r_output/output_mis_count_",thisn,".csv"),sep=",",row.names = F)
write.table(z5,paste0("4_r_output/output_mis_summary_",thisn,".csv"),sep=",",row.names = F)

#6.fun that have the histrogram to show how the classificatio of groups varies
fun_hist_misclass <- function(pick_cond,sel_var){
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
  
  z <- aggregate(df6[,c(str_which(colnames(df6),"_q1"),
                        str_which(colnames(df6),"_q2"),
                        str_which(colnames(df6),"_q3"),
                        str_which(colnames(df6),"_q4"))],by=list(df6$loop.count),sum,na.rm=T) %>%.[,-1]
  #make a overlap data frame
  #zll <- gather(z, whichq, countinq, 2:21, factor_key=TRUE) %>% .[str_detect(.$whichq,paste0("t","cm")),]
  #ggplot(zll, aes(x=countinq,fill=whichq)) + geom_histogram(position="identity",alpha=0.3,binwidth=1)
  p1 <- z[,paste0(sel_var,"_q1")] %>% as.data.frame()%>%`colnames<-`("x") %>%
      ggplot(., aes(x=x))+geom_histogram(binwidth=1)+labs(x=paste0(sel_var,"_q1"))+scale_x_continuous(limits = c(0, thisn))
  p2 <- z[,paste0(sel_var,"_q2")] %>% as.data.frame()%>%`colnames<-`("x") %>%
      ggplot(., aes(x=x))+geom_histogram(binwidth=1)+labs(x=paste0(sel_var,"_q2"))+scale_x_continuous(limits = c(0, thisn))
  p3 <- z[,paste0(sel_var,"_q3")] %>% as.data.frame()%>%`colnames<-`("x") %>%
      ggplot(., aes(x=x))+geom_histogram(binwidth=1)+labs(x=paste0(sel_var,"_q3"))+scale_x_continuous(limits = c(0, thisn))
  p4 <- z[,paste0(sel_var,"_q4")] %>% as.data.frame()%>%`colnames<-`("x") %>%
      ggplot(., aes(x=x))+geom_histogram(binwidth=1)+labs(x=paste0(sel_var,"_q4"))+scale_x_continuous(limits = c(0, thisn))
  
  p <- grid.arrange(p1,p2,p3,p4,ncol=1,top=paste("under:",pick_cond))
  p
}

#p <- fun_hist_misclass("all random",pro_tested[2])
#ggsave(paste0("4_r_output/kk.png"),p,width=8)
ep_test_list <- table(df3$grp) %>% names() %>% .[which(str_sub(table(df3$grp) %>% names,end=1) %in%c("0","1","2"))]
for(i in 1:length(ep_test_list)){
  p <- fun_hist_misclass(str_sub(ep_test_list, start= 7)[i],pro_tested[1])  
  p <- ggsave(paste0("4_r_output/5_hist_miss",pro_tested[1],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=8)
  p <- fun_hist_misclass(str_sub(ep_test_list, start= 7)[i],pro_tested[2])  
  p <- ggsave(paste0("4_r_output/5_hist_miss",pro_tested[2],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=8)
  p <- fun_hist_misclass(str_sub(ep_test_list, start= 7)[i],pro_tested[3])  
  p <- ggsave(paste0("4_r_output/5_hist_miss",pro_tested[3],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=8)
  p <- fun_hist_misclass(str_sub(ep_test_list, start= 7)[i],pro_tested[4])  
  p <- ggsave(paste0("4_r_output/5_hist_miss",pro_tested[4],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=8)
  #p <- fun_hist_misclass(str_sub(ep_test_list, start= 7)[i],pro_tested[5])  
  #p <- ggsave(paste0("4_r_output/5_hist_miss",pro_tested[5],"_under_",str_sub(ep_test_list, start= 7)[i],".png"),p,width=8)
  print(i)
}

