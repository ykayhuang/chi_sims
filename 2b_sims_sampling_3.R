#1221 log: change the way of loading file, with thisn
#about the kappa between tests

#library(irr)
library(raters)

#set up n/variable for the analysis // BTW, need to load the df_truevs (2b....sampling_1)
thisn <- 80
##setting up the test variables #then I can just change this one
pro_tested <- c("dist_airport_m","mst_500m","ind_750m","ndvi_300m") #changed

#read the correspnding file
df3 <- fread( paste0("4_r_output/sims_result_",thisn,".csv"),sep=",",header=T) %>% 
  mutate(grp=paste(n_profiles,"ep:",cond.note)) %>% as.data.frame()
#df3 <- fread("4_r_output/sims_result_40.csv",sep=",",header=T) %>% 
#  mutate(grp=paste(n_profiles,"ep:",cond.note)) %>% as.data.frame()
#no_runs <- read.table("4_r_output/sims_skips.csv",sep=",",header=T)
df3$cond_loop <- paste(df3$cond.note,df3$loop.count,sep="-")
#cut to 2
df3 <- subset(df3,n_profiles<3)
#go for the anaylsis (agreement test)

kapp_byep <- function(pick_cond,sel_var){
#kappa by ep for every vars
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
  
  tz1 <- z[rownames(z) %>% str_detect(.,sel_var) %>% which(.==T),] %>% t %>% as.data.frame()
  tz2 <- tz1*thisn  
  
  c(wlin.conc(tz2,test="MC",B=25),sel_var)
  
  }
  
  #p2 <- kapp_byep("all random","dist_airport_m") 
#$. run the agreement test for the variables
  #z3 <- table(df3$grp) %>% names() %>% .[1:26]
  ep_test_list <- table(df3$grp) %>% names() %>% .[which(str_sub(table(df3$grp) %>% names,end=1) %in%c("0","1","2"))]
  #c_5vs <- list("dist_airport_m","dist_cta_m","ndvi_300m","ind_500m","tcm_500m")
  c_5vs <- as.list(pro_tested)  #this one is not a factor, is a list 
  pp <- NULL
  for(i in 1:length(ep_test_list)){
    #p3 <- kapp_byep(str_sub(z3, start= 7)[i],i,x) %>% as.numeric() %>% c(x,str_sub(z3, start= 7)[i],.)
    #pp <- rbind(pp, p3)
    cond <- str_sub(ep_test_list, start= 7)[i]
    p3 <- lapply(c_5vs, kapp_byep,pick_cond=cond) %>% do.call(rbind,.) %>% cbind(cond,.)
    pp <- rbind(pp, p3)
    print(i)
    }
  pp8 <- pp %>% as.data.frame()
  pp8$V7 <- factor(pp8$V7, levels = pro_tested )
  pp8$cond <- factor(pp8$cond, levels = c("all random","bi_airport_5k","bi_ind_750","bi_ndvi_300","bi_mst_500",
                                          "bi_airport_5k + bi_ndvi_300","bi_ind_750 + bi_airport_5k","bi_ind_750 + bi_ndvi_300",
                                          "bi_mst_500 + bi_airport_5k","bi_mst_500 + bi_ind_750", "bi_mst_500 + bi_ndvi_300"))
  out_s_kappa <- pp8[with(pp8, order(V7, cond)), ]
  write.table(out_s_kappa,paste0("4_r_output/out_s_kappa_",thisn,".csv"),sep=",",row.names = F)

  #THE FOLLOWING IS NOT CHAGNED
  #WORKED ON THE REST
  #STOP HERE  
  
##  
#not kappa, but the chisq testing for trend (q1 q2 q3 q4)
#here, testing if the pick_cond is different than the all random in each of the 100 loop
  fun_chisq_with100ran <- function(pick_cond,sel_var){
  df5 <- df3 %>% .[which(.$cond.note%in% c(pick_cond)),]
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
                        str_which(colnames(df6),"_q4"))],by=list(df6$loop.count,df6$cond.note),sum,na.rm=T) 
  thetestingout <- NULL
  for (i in 1:100){
  z2 <- z[which(z$Group.1==i),] 
  zl <- gather(z2, whichq, countinq, 3:(2+length(pro_tested)*4), factor_key=TRUE) %>%.[str_detect(.$whichq,sel_var),]
  obs <- (zl$countinq)
  #obs <- (zl2$countinq)
  #exp <- c(.25,.25,.25,.25) #
  #yt <- chisq.test(x=obs,p=exp)
  if (str_detect(sel_var,"ind")){ #ind is not c(0.25,0.25,.25,.25), should be (0.66,0.09,0.25) only three, remove q2 ->ind500
                                                                      #.56,.19,.25->ind750
    yt <- prop.test(obs[-2],rep(sum(obs[-2]),3),p=c(0.56,0.19,0.25))
  }else if(str_detect(sel_var,"mst_500")){
    yt <- prop.test(obs,rep(sum(obs),4),p=c(0.47,0.03,0.25,0.25))
  }else if(str_detect(sel_var,"mst_750")){
    yt <- prop.test(obs,rep(sum(obs),4),p=c(0.32,0.18,0.25,0.25))
  }else {
    yt <- prop.test(obs,rep(sum(obs),4)) #with Yate' correction, due to n>=40 (typical)  #p are equal so no p=c(.25,.25,.25,.25)
    }
  thetestingout <- rbind(thetestingout,  c(yt$statistic,yt$p.value))
  }
  thetestingout %>% as.data.frame()
  #end
  }
  #fun_chisq_with100ran(str_sub(ep_test_list, start= 7)[3],"ind_500m")
  
  ep_test_list <- table(df3$grp) %>% names() %>% .[which(str_sub(table(df3$grp) %>% names,end=1) %in%c("0","1","2"))]
  c_5vs <- as.list(pro_tested)  #this one is not a factor, is a list 
  pp <- NULL
  for(i in 1:length(ep_test_list)){
    #p3 <- kapp_byep(str_sub(z3, start= 7)[i],i,x) %>% as.numeric() %>% c(x,str_sub(z3, start= 7)[i],.)
    #pp <- rbind(pp, p3)
    cond <- str_sub(ep_test_list, start= 7)[i]
    p3 <- lapply(c_5vs, fun_chisq_with100ran,pick_cond=cond) %>% do.call(rbind,.) %>% 
      cbind(cond,.,rep(pro_tested,each=100),i)
    pp <- rbind(pp, p3)
    print(i)
  }
  pp5 <- subset(pp,V2>0.05) 
  colnames(pp5)[4] <- "testing_v"
  pp5$cond_n_v <- paste(pp5$cond,pp5$testing_v,sep = "&")
  pp6 <- table(pp5$cond_n_v) %>% as.data.frame() %>% cbind(str_split_fixed(.$Var1, "\\&", 2))%>%`colnames<-`(c("cond_v","runs","cond","v"))
  #
  pp7 <- expand.grid(cond = str_sub(ep_test_list, start= 7), v = c_5vs) %>% mutate(cond_v=paste(cond,v,sep="&")) %>%
          merge(.,pp6,by="cond_v",all.x=T) %>% .[,1:4]%>%`colnames<-`(c("cond_v","cond","v","runs"))
  #sort the output
  pp7$v <- factor(pp7$v, levels = c("dist_airport_m","dist_cta_m","ndvi_300m","ind_750m","tcm_500m"))
  pp7$cond <- factor(pp7$cond, levels = c("all random","bi_airport_5k","bi_ind_750","bi_ndvi_300","bi_mst_500",
                                          "bi_airport_5k + bi_ndvi_300","bi_ind_750 + bi_airport_5k","bi_ind_750 + bi_ndvi_300",
                                          "bi_mst_500 + bi_airport_5k","bi_mst_500 + bi_ind_750","bi_mst_500 + bi_ndvi_300"))
  pp7 <- pp7[with(pp7, order(v, cond)), ]
  #end sort
  write.table(pp5,paste0("4_r_output/detail_runs_",thisn,".csv"),sep=",",row.names = F)
  write.table(pp7,paste0("4_r_output/out_kappa_test_",thisn,".csv"),sep=",",row.names = F)