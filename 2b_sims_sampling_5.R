#mod:20190102: p4,p5 change n into percent
#mod:20190127: use the to have a grapch. make some time to save it auto
#mod:20190223: redo the figure 3
###DO NOT RUN THE SIMS!!!
#
##setting up the test variables #then I can just change this one
pro_tested <- c("dist_airport_m","mst_500m","ind_750m","ndvi_300m") #changed

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

#make figures
##
ldf_mis_count <- NULL
ldf_mis_summary <- NULL
#read the correspnding file by loop from 20 to 100
for (d in 5:25){
  thisn <- d*4
  print(thisn)##
  df3 <- fread( paste0("4_r_output/sims_result_",thisn,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note)) %>% as.data.frame()
  df3$cond_loop <- paste(df3$cond.note,df3$loop.count,sep="-")

  ep_test_list <- table(df3$grp) %>% names() %>% .[which(str_sub(table(df3$grp) %>% names,end=1) %in%c("0","1","2"))]
  c_vs <- as.list(pro_tested)  #this one is not a factor, is a list 
  pp <- NULL
  mm <- NULL
  for(i in 1:length(ep_test_list)){
    #p3 <- kapp_byep(str_sub(z3, start= 7)[i],i,x) %>% as.numeric() %>% c(x,str_sub(z3, start= 7)[i],.)
    #pp <- rbind(pp, p3)
    cond <- str_sub(ep_test_list, start= 7)[i]
    p3 <- lapply(c_vs, fun_count_misclass,pick_cond=cond) %>% do.call(rbind,.) %>% t  %>% as.data.frame()  %>% cbind(cond,c(1:100),.)%>%`colnames<-`(c("cond","loop.count","mis_airport","mis_mst","mis_ind","mis_ndvi"))
    pp <- rbind(pp, p3)
    p4 <- c(mean(p3[,3]),sd(p3[,3]),max(p3[,3]),
            mean(p3[,4]),sd(p3[,4]),max(p3[,4]),
            mean(p3[,5]),sd(p3[,5]),max(p3[,5]),
            mean(p3[,6]),sd(p3[,6]),max(p3[,6]))/thisn*100 #change n into percent
    p5 <- c(cond,p4)
    mm <- rbind(mm,p5)
    print(i)
  }
  z4 <- pp %>% as.data.frame() %>% cbind(.,thisn)
  z5 <- mm %>% as.data.frame()%>%`colnames<-`(c("cond","MEAN in mis_airport","SD in mis_airport","MAX in mis_airport",
                                                "MEAN in mis_mst","SD in mis_mst","MAX in mis_mst",
                                                "MEAN in mis_ind","SD in mis_ind","MAX in mis_ind",
                                                "MEAN in mis_ndvi","SD in mis_ndvi","MAX in mis_ndvi"))%>%`rownames<-`(NULL)%>% cbind(.,thisn)
  ldf_mis_count <- rbind(ldf_mis_count,z4)
  ldf_mis_summary <- rbind(ldf_mis_summary,z5)
}

#rename the condition
ldf_mis_summary$cond2 <- factor(ldf_mis_summary$cond, levels = c("all random","bi_airport_5k","bi_mst_500","bi_ind_750","bi_ndvi_300",
                                                                 "bi_mst_500 + bi_airport_5k","bi_ind_750 + bi_airport_5k","bi_airport_5k + bi_ndvi_300",
                                                                 "bi_mst_500 + bi_ind_750", "bi_mst_500 + bi_ndvi_300",
                                                                 "bi_ind_750 + bi_ndvi_300")) #sort the seq that i want, based on original varialbe's value
levels(ldf_mis_summary$cond2) <- c("None (simple random)","Air_B","MS_B","IND_B","NDVI_B",
                                   "Air_B + MS_B","Air_B + IND_B","Air_B + NDVI_B",
                                   "MS_B + IND_B","MS_B + NDVI_B",
                                   "IND_B + NDVI_B") #remain the factor (level)

#ggplot(ldf_mis_count, aes(x=thisn,y=mis_airport/thisn*100,group=thisn)) + geom_boxplot()
#ggplot(ldf_mis_count, aes(x=factor(thisn),y=mis_airport/thisn*100)) + geom_boxplot(aes(fill=factor(cond)))
#ggplot(ldf_mis_count, aes(x=thisn,y=mis_mst/thisn*100,group=thisn)) + geom_boxplot()
#ggplot(ldf_mis_count, aes(x=thisn,y=mis_ind/thisn*100,group=thisn)) + geom_boxplot()
#ggplot(ldf_mis_count, aes(x=thisn,y=mis_ndvi/thisn*100,group=thisn)) + geom_boxplot()
#boxplot
#p <- ggplot(ldf_mis_count, aes(factor(thisn), mis_airport/thisn*100))
#p <- p + geom_boxplot(aes(colour = factor(cond)))
#p

#lineplot #figure 1(mean)
for (ldf_m_s_c in c(2,5,8,11)){
  #ldf_m_s_c <- 2 # 2,5,8,11
  p2<- ggplot(ldf_mis_summary, aes(x=thisn, y=as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c])), color=cond2,group=cond2,shape=cond2)) +   geom_line() +  
    geom_point(size=4) #+
    #geom_errorbar(aes(ymin=as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c]))-as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c+1])), 
    #                  ymax=as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c]))+as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c+1]))), width=.2,
    #              position=position_dodge(2))
  p2 <- p2+scale_shape_manual(values=c(4,15,19,17,18,
                                       97,98,99,100,101,102))
  p2 <- p2+scale_color_manual(values=c("black", "gray32","gray32","gray32","gray32",
                                       "gray58","gray58","gray58","gray58","gray58","gray58")) 
  p2 <- p2+theme_bw(base_size = 26)+ theme(legend.position="right") + expand_limits(y =c(0,32))
  p2 <- p2+labs(#title = paste("The variable:",colnames(ldf_mis_summary)[ldf_m_s_c] %>% str_sub(.,9)),
                x="sampling campaign size, n",
                color="stratifying factor",shape="stratifying factor",
              y="Mean, sampling error percentage, %")+guides(colour = guide_legend(override.aes = list(linetype  = 0)))
  p2
  ggsave(paste0("4_r_output/figure/figure1_",ldf_m_s_c%/%3+1,".png"),p2,width=12,height=7)
}
#this one is for the boxplot
# ddr4 <- subset(ldf_mis_count,thisn%in%c(20,40,60,80,100))
# p <- ggplot(ddr4, aes(factor(thisn), mis_ndvi/thisn*100)) #
# p <- p + geom_boxplot(aes(color = factor(cond)),outlier.colour = NULL,outlier.size = 3,show.legend = F) #this one use colored box, with outlier colured / and not showing the legend (b/c been covered)
# p <- p + geom_boxplot(aes(fill = factor(cond)),outlier.shape = NA) #this one have filled box, lay on top the colored boxed. But the removed oulier won't lay on the coloured outlier
# p <- p+theme_bw(base_size = 26)+ theme(legend.position="bottom")
# p <- p+labs(y="sampling error percentage, %",x="sampling campaign size, n",
#             fill="stratifying factor",
#             title = paste("The variable:mis_ndvi"), #
#             subtitle="only showing n=20,40,60,80,100")
# p
#write.table(z4,paste0("4_r_output/output_mis_count_",thisn,".csv"),sep=",",row.names = F)
write.table(ldf_mis_summary,paste0("4_r_output/output_mis_summary_huge",thisn,".csv"),sep=",",row.names = F)

##figures for comparing sd with simple random
##line plot showing the trend of sd
#lineplot #figure 2 (SD)
for (ldf_m_s_c in c(2,5,8,11)){
  #ldf_m_s_c <- 2 # 2,5,8,11
  p4 <- ggplot(ldf_mis_summary, aes(x=thisn, y=as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c+1])), color=cond2,group=cond2,shape=cond2)) +   geom_line() +  
    geom_point(size=4) #+
  #geom_errorbar(aes(ymin=as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c]))-as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c+1])), 
  #                  ymax=as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c]))+as.numeric(as.character(ldf_mis_summary[,ldf_m_s_c+1]))), width=.2,
  #              position=position_dodge(2))
  p4 <- p4 +scale_shape_manual(values=c(4,15,19,17,18,
                                       97,98,99,100,101,102))
  p4 <- p4 +scale_color_manual(values=c("black", "gray32","gray32","gray32","gray32",
                                       "gray58","gray58","gray58","gray58","gray58","gray58")) 
  p4 <- p4 +theme_bw(base_size = 26)+ theme(legend.position="right") + expand_limits(y =c(0,8))
  p4 <- p4 +labs(#title = paste("The variable:",colnames(ldf_mis_summary)[ldf_m_s_c] %>% str_sub(.,9)),
                 x="sampling campaign size, n",
                color="stratifying factor",shape="stratifying factor",
                y="SD, sampling error percentage, %")+guides(colour = guide_legend(override.aes = list(linetype  = 0)))
  p4
  ggsave(paste0("4_r_output/figure/figure2_",ldf_m_s_c%/%3+1,".png"),p4,width=12,height=7)
}

###the t test
#t-test
F_ttest_miscount <- function(n,var){
tf_1 <- cbind(ldf_mis_count,rep(rep(1:11,each=100),21) ) 
colnames(tf_1)[8]<-"cond.num"
              # tf_t <- subset(ldf_mis_count,thisn==40,select=c(cond,mis_mst))
              # tf_t2 <- tf_t[which(tf_t$cond=="bi_mst_500 + bi_ndvi_300"),2]/40
              # tf_t3 <- tf_t[which(tf_t$cond=="bi_mst_500"),2]/40
L_40_ttest <- list()
#tf_1 <- subset(tf_1,thisn==40,select=c(cond.num,var,cond))
tf_1 <- tf_1[tf_1$thisn==n,which(colnames(tf_1)%in%c("cond.num",var,"cond"))]
for (k in 2:11){
tf_t2 <- tf_1[tf_1$cond.num==1,2]/n
tf_t3 <- tf_1[tf_1$cond.num==k,2]/n
L_40_ttest[k] <- t.test(tf_t2,tf_t3)$p.value
}
out_40_ttest <-  c(NA,(unlist(L_40_ttest) %>% round(.,3)))
names(out_40_ttest) <- table(ldf_mis_count$cond) %>% names(.)
out_40_ttest
}
#               
sapply(list("mis_airport","mis_mst","mis_ind","mis_ndvi"), F_ttest_miscount,n=40)
sapply(list("mis_airport","mis_mst","mis_ind","mis_ndvi"), F_ttest_miscount,n=80)

####this is for fixed bad stratum
##
ldfix_mis_count <- NULL
ldfix_mis_summary <- NULL
#read the correspnding file by loop from 40 to 80 (temp)
for (d in 5:25){
  thisn <- d*4
  print(thisn)##
  df3 <- fread( paste0("4_r_output/sims_fixed_bad/sims_result_",thisn,".csv"),sep=",",header=T) %>% 
    mutate(grp=paste(n_profiles,"ep:",cond.note)) %>% as.data.frame()
  df3$cond_loop <- paste(df3$cond.note,df3$loop.count,sep="-")
  
  ep_test_list <- table(df3$grp) %>% names() %>% .[which(str_sub(table(df3$grp) %>% names,end=1) %in%c("0","1","2"))]
  c_vs <- as.list(pro_tested)  #this one is not a factor, is a list 
  pp <- NULL
  mm <- NULL
  for(i in 1:length(ep_test_list)){
    #p3 <- kapp_byep(str_sub(z3, start= 7)[i],i,x) %>% as.numeric() %>% c(x,str_sub(z3, start= 7)[i],.)
    #pp <- rbind(pp, p3)
    cond <- str_sub(ep_test_list, start= 7)[i]
    p3 <- lapply(c_vs, fun_count_misclass,pick_cond=cond) %>% do.call(rbind,.) %>% t  %>% as.data.frame()  %>% cbind(cond,c(1:100),.)%>%`colnames<-`(c("cond","loop.count","mis_airport","mis_mst","mis_ind","mis_ndvi"))
    pp <- rbind(pp, p3)
    p4 <- c(mean(p3[,3]),sd(p3[,3]),max(p3[,3]),
            mean(p3[,4]),sd(p3[,4]),max(p3[,4]),
            mean(p3[,5]),sd(p3[,5]),max(p3[,5]),
            mean(p3[,6]),sd(p3[,6]),max(p3[,6]))/thisn*100 #change n into percent
    p5 <- c(cond,p4)
    mm <- rbind(mm,p5)
    print(i)
  }
  z4 <- pp %>% as.data.frame() %>% cbind(.,thisn)
  z5 <- mm %>% as.data.frame()%>%`colnames<-`(c("cond","MEAN in mis_airport","SD in mis_airport","MAX in mis_airport",
                                                "MEAN in mis_mst","SD in mis_mst","MAX in mis_mst",
                                                "MEAN in mis_ind","SD in mis_ind","MAX in mis_ind",
                                                "MEAN in mis_ndvi","SD in mis_ndvi","MAX in mis_ndvi"))%>%`rownames<-`(NULL)%>% cbind(.,thisn)
  ldfix_mis_count <- rbind(ldfix_mis_count,z4)
  ldfix_mis_summary <- rbind(ldfix_mis_summary,z5)
}

#lineplot #lazy code at first..
ddr6 <- subset(ldfix_mis_summary,cond%in%c("bi_airport_5k","all random")) ##for comparing fixed and all random
ddr6_long <- rbind( cbind(ddr6[,c(1,2,3,14)],"mis_airport") %>%`colnames<-`(c("cond","mean","sd","n","mis")),
                    cbind(ddr6[,c(1,5,6,14)],"mis_mst") %>%`colnames<-`(c("cond","mean","sd","n","mis")),
                    cbind(ddr6[,c(1,8,9,14)],"mis_ind") %>%`colnames<-`(c("cond","mean","sd","n","mis")),
                    cbind(ddr6[,c(1,11,12,14)],"mis_ndvi") %>%`colnames<-`(c("cond","mean","sd","n","mis"))  ) %>% mutate(d = paste0(cond,mis))

#figure 3
# p3<- ggplot(ddr6_long, aes(x=n, y=as.numeric(as.character(ddr6_long$mean)), color=mis,group=d,linetype=cond)) +   geom_line(size=2) +  
#       scale_linetype_manual(values=c("dotted", "solid")) + expand_limits(y = 0) +
#       geom_errorbar(aes(ymin=as.numeric(as.character(ddr6_long$mean))-as.numeric(as.character(ddr6_long$sd)), 
#                            ymax=as.numeric(as.character(ddr6_long$mean))+as.numeric(as.character(ddr6_long$sd))), width=.2,
#                        position=position_dodge(2))
# p3 <- p3+theme_bw(base_size = 26)+ theme(legend.position="right")  
# p3 <- p3+labs(title = paste("The stratifying factor: Air_B / none (simple random sampling)"),x="sampling campaign size, n",
#               color="the testing variable",
#               y="sampling error percentage, %",
#               subtitle="error bar shows the SD / the sampling fraction is adjusted to 14%-86%")
# p3
#new figure 3, no error bar
# p3<- ggplot(ddr6_long, aes(x=n, y=as.numeric(as.character(ddr6_long$mean)), group=d,color=cond,shape=mis)) +   geom_line() +  
#   scale_color_manual(values=c("gray60", "black")) + expand_limits(y = 0) +geom_point(size=4)
# p3 <- p3+theme_bw(base_size = 26)+ theme(legend.position="right")  
# #p3 <- p3+scale_color_manual(values=c("black", "gray28","gray45","gray62")) 
# p3 <- p3+labs(x="sampling campaign size, n",
#               shape="the testing variable",
#               color="sampling strategy",
#               y="sampling error percentage, %")
# p3
#new fig 3, just one v
ddr6_long <- ddr6_long[ddr6_long$mis=="mis_airport",]
p3<- ggplot(ddr6_long, aes(x=n, y=as.numeric(as.character(ddr6_long$mean)), group=d,color=cond)) +   geom_line() +  
  scale_color_manual(values=c("gray60", "black")) + expand_limits(y = 0) +geom_point(size=4)
p3 <- p3+theme_bw(base_size = 26)+ theme(legend.position="right")  
#p3 <- p3+scale_color_manual(values=c("black", "gray28","gray45","gray62")) 
p3 <- p3+labs(x="sampling campaign size, n",
              shape="the testing variable",
              color="sampling strategy",
              y="sampling error percentage, %")
p3
#
#
ggsave(paste0("4_r_output/figure/figure3.png"),p3,width=12,height=7)
####this is for fixed bad stratum

##caluate how many extrem
e_case_value <- NULL #use mean+2sd as extrem v
for (x in 5:25){
  for (y in 1:11){
    thisn <- 4*x
    z7 <- ldfix_mis_count[ldfix_mis_count$thisn==thisn,] %>% subset(.,cond==as.character(ldfix_mis_summary[y,1]))
    z8 <- ldfix_mis_summary[ldfix_mis_summary$thisn==thisn,]
        e_case_value <- rbind(e_case_value, 
        c(
            table(z7$mis_airport/thisn*100 > as.numeric(as.character(z8[1,2]))+2*as.numeric(as.character(z8[1,3]))) %>% .["TRUE"],
            table(z7$mis_mst/thisn*100 > as.numeric(as.character(z8[1,5]))+2*as.numeric(as.character(z8[1,6]))) %>% .["TRUE"],
            table(z7$mis_ind/thisn*100 > as.numeric(as.character(z8[1,8]))+2*as.numeric(as.character(z8[1,9]))) %>% .["TRUE"],
            table(z7$mis_ndvi/thisn*100 > as.numeric(as.character(z8[1,11]))+2*as.numeric(as.character(z8[1,12]))) %>% .["TRUE"] ,
            as.character(z8[y,1]),thisn)
    )
  }
}
e_case_value[is.na(e_case_value)] <- 0
e_case_value <- as.data.frame(e_case_value) %>%`colnames<-`(c("ev_c_airport","ev_c_mst","ev_c_ind","ev_c_ndvi","cond","n"))
write.table(e_case_value,paste0("4_r_output/output_mis_countforextrem",".csv"),sep=",",row.names = F)
#done
