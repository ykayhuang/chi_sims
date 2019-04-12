#20181210 log: add mst, change 5 e.p into only 4 (mst,ind,ndvi,airport)
#20190110 log: set to ind750
#20190123 log: add the changeable n from 40/80 to 40/44....80(small n=10,11....20)
#20190411 log: change the mst (calculation in GIS is changed)
#              put to simulation program together

#Sys.setlocale("LC_ALL", "English")
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(gridExtra)
library(tidyr)
library(data.table)
library(ff)

raw_grid <- read.table("2_GIS_output/raw_200m_fc.txt",sep=",",header=T)
#replace by nearest dist

#read the gis var
air <- read.table("2_GIS_output/201904/fc_to_near_airport_ft.txt",sep=",",header=T)
grn <- read.table("2_GIS_output/201904/ndvi_300m.txt",sep=",",header=T)
mst <- read.table("2_GIS_output/201904/fc500mst201904.csv",sep=",",header=T)

mst2 <-aggregate(mst$t, by=list(mst$ORIG_FID), FUN=sum, na.rm=TRUE)%>%  `colnames<-`(c("oid","mst"))
prd2 <-aggregate(prd$F11, by=list(prd$OBJECTID), FUN=sum, na.rm=TRUE)%>%  `colnames<-`(c("oid","prd"))

clean_df <- cbind(air$OBJECTID,air$NEAR_DIST*0.3048,grn$MEAN) %>% 
  as.data.frame %>%  `colnames<-`(c("oid", "dair", "ndvi"))

clean_df2 <- merge(clean_df,mst2,by="oid",all=T) 
clean_df2[is.na(clean_df2)] <- 0
#
raw_big <- clean_df2
#
df2 <- raw_big

#make the condition (bi)  
df2$bi_airport_5k <- ifelse(df2$dair<=5000,1,0) 
df2$bi_ndvi_300 <- ifelse(df2$ndvi>=0.35,1,0)
df2$bi_mst_500 <- ifelse(df2$mst==0,0,1)

##
##simulation set
##
longdf <- NULL
c_pros <- c("bi_mst_500","bi_airport_5k","bi_ndvi_300") #change into 3ep
repeat_loop <- 100 #still 100 runs
#set.seed(1)
#t_sub_n became a var during 20190123
fun_sims_with_t_sub_n <- function(t_sub_n){
  longdf <- NULL
  set.seed(1)
  ########
  #set end
  #start
  ########
  start_time <- Sys.time()
  #sampling, 0. all random
  for (i in 1:repeat_loop){
    min_sub_n <- t_sub_n
    
    tf <- df2[sample(nrow(df2), min_sub_n), ]
    tf$loop.count <- i
    tf$cond.note <- "all random"
    tf$n_profiles <- 0
    longdf <- rbind(longdf,tf)
    if(i%%100==0)print(i) #show loop counts in 100
  }
  #sampling, 1. 1 profiles 
  for (j in 1:length(c_pros)){
    min_sub_n <- t_sub_n/2
    if((sum(df2[,(colnames(df2)==c_pros[j])]==0,na.rm = T)%/%min_sub_n*
        sum(df2[,(colnames(df2)==c_pros[j])]==1,na.rm = T)%/%min_sub_n)>0){
      for (i in 1:repeat_loop){
        set.seed(i)
        tf <- rbind(df2 %>% .[which(.[,which(colnames(.)==c_pros[j])]==0),] %>% .[sample(nrow(.), min_sub_n), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j])]==1),] %>% .[sample(nrow(.), min_sub_n), ])
        tf$loop.count <- i
        tf$cond.note <- c_pros[j]
        tf$n_profiles <- 1
        longdf <- rbind(longdf,tf)
        if(i%%100==0)print(i) #show loop counts in 100
      }
    }else{
      no_runs <- c(no_runs,c_pros[j])
    }
  }
  #sampling, 2. 2 profiles 
  c4_2 <- list(c(1,2),c(1,3),c(2,3)      )
  for (j in c4_2){
    j1 <- j[1]
    j2 <- j[2]
    min_sub_n <- t_sub_n/4
    if((sum(df2[,(colnames(df2)==c_pros[j1])]==0&df2[,(colnames(df2)==c_pros[j2])]==0,na.rm = T)%/%min_sub_n*
        sum(df2[,(colnames(df2)==c_pros[j1])]==1&df2[,(colnames(df2)==c_pros[j2])]==0,na.rm = T)%/%min_sub_n*
        sum(df2[,(colnames(df2)==c_pros[j1])]==0&df2[,(colnames(df2)==c_pros[j2])]==1,na.rm = T)%/%min_sub_n*
        sum(df2[,(colnames(df2)==c_pros[j1])]==1&df2[,(colnames(df2)==c_pros[j2])]==1,na.rm = T)%/%min_sub_n)>0){
      for (i in 1:repeat_loop){
        
        tf <- rbind(df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==0 & .[,which(colnames(.)==c_pros[j2])]==0),] %>% .[sample(nrow(.), min_sub_n), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==0 & .[,which(colnames(.)==c_pros[j2])]==1),] %>% .[sample(nrow(.), min_sub_n), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==1 & .[,which(colnames(.)==c_pros[j2])]==0),] %>% .[sample(nrow(.), min_sub_n), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==1 & .[,which(colnames(.)==c_pros[j2])]==1),] %>% .[sample(nrow(.), min_sub_n), ])
        tf$loop.count <- i
        tf$cond.note <- paste(c_pros[j1],"+",c_pros[j2])
        tf$n_profiles <- 2
        longdf <- rbind(longdf,tf)
        if(i%%100==0)print(i) #show loop counts in 100
      }
    }else{
      no_runs <- c(no_runs,paste(c_pros[j1],"+",c_pros[j2]))
    }
  }
  #sampling, 3. 3 profiles (12*8)
  #this part is taken out at 20190123
  #
  #print(no_runs)
  #no_runs <- no_runs%>%as.data.frame()%>%`colnames<-`("skip.group")
  rownames(longdf) <- NULL
  end_time <- Sys.time()
  print(c(start_time,end_time))
  difftime(start_time,end_time)
  ###
  #simulation done#
  ####
  longdf
  #
}
#a2 <- fun_sims_with_t_sub_n(48) #just a test

#make from 40 to 80, save in a list and save in the r_output
list_of_sims <- NULL
for(d in c(5:25)){
  t_sub_n <- d*4
  longdf <- fun_sims_with_t_sub_n(t_sub_n)
  write.table(longdf,paste0("4_r_output/sims_201904/sims_result_",t_sub_n,".csv"),sep=",",row.names = F)
  print(t_sub_n)
}


##next is the fixed fraction
##
##simulation set
##
longdf <- NULL
c_pros <- c("bi_mst_500","bi_airport_5k","bi_ndvi_300") #change into 3ep
repeat_loop <- 100 #still 100 runs
#set.seed(1)
#t_sub_n became a var during 20190123
fun_sims_with_t_sub_n <- function(t_sub_n){
  longdf <- NULL
  set.seed(1)
  ##this is for the fixed one
  fixed_v <- "bi_airport_5k"
  fixed_p <- 0.14 #list the precentage that above v should have (when v=1)
  ########
  #set end
  #start
  ########
  start_time <- Sys.time()
  #sampling, 0. all random
  for (i in 1:repeat_loop){
    min_sub_n <- t_sub_n
    
    tf <- df2[sample(nrow(df2), min_sub_n), ]
    tf$loop.count <- i
    tf$cond.note <- "all random"
    tf$n_profiles <- 0
    longdf <- rbind(longdf,tf)
    if(i%%100==0)print(i) #show loop counts in 100
  }
  #sampling, 1. 1 profiles 
  for (j in 1:length(c_pros)){
    min_sub_n <- t_sub_n/2
    adj_n_small <- round(t_sub_n*0.14)
    if (c_pros[j]=="bi_airport_5k"){
      for (i in 1:repeat_loop){
        set.seed(i)
        tf <- rbind(df2 %>% .[which(.[,which(colnames(.)==c_pros[j])]==0),] %>% .[sample(nrow(.), t_sub_n-adj_n_small), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j])]==1),] %>% .[sample(nrow(.), adj_n_small), ])
        tf$loop.count <- i
        tf$cond.note <- c_pros[j]
        tf$n_profiles <- 1
        longdf <- rbind(longdf,tf)
        if(i%%100==0)print(i) #show loop counts in 100
      }
    }else{
      for (i in 1:repeat_loop){
        set.seed(i)
        tf <- rbind(df2 %>% .[which(.[,which(colnames(.)==c_pros[j])]==0),] %>% .[sample(nrow(.), min_sub_n), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j])]==1),] %>% .[sample(nrow(.), min_sub_n), ])
        tf$loop.count <- i
        tf$cond.note <- c_pros[j]
        tf$n_profiles <- 1
        longdf <- rbind(longdf,tf)
        if(i%%100==0)print(i) #show loop counts in 100
      }
    }
  }
  #sampling, 2. 2 profiles 
  c4_2 <- list(c(1,2),c(1,3),c(2,3)       )
  for (j in c4_2){
    j1 <- j[1]
    j2 <- j[2]
    min_sub_n <- t_sub_n/2/2
    adj_n_small <- ceiling(t_sub_n*fixed_p/2)
    if(c_pros[j1]==fixed_v){
      for (i in 1:repeat_loop){
        tf <- rbind(df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==0 & .[,which(colnames(.)==c_pros[j2])]==0),] %>% .[sample(nrow(.), t_sub_n/2-adj_n_small), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==0 & .[,which(colnames(.)==c_pros[j2])]==1),] %>% .[sample(nrow(.), t_sub_n/2-adj_n_small), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==1 & .[,which(colnames(.)==c_pros[j2])]==0),] %>% .[sample(nrow(.), adj_n_small), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==1 & .[,which(colnames(.)==c_pros[j2])]==1),] %>% .[sample(nrow(.), adj_n_small), ])
        tf$loop.count <- i
        tf$cond.note <- paste(c_pros[j1],"+",c_pros[j2])
        tf$n_profiles <- 2
        longdf <- rbind(longdf,tf)
        if(i%%100==0)print(i) #show loop counts in 100
      }
    }else if(c_pros[j2]==fixed_v){
      for (i in 1:repeat_loop){ 
        tf <- rbind(df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==0 & .[,which(colnames(.)==c_pros[j2])]==0),] %>% .[sample(nrow(.), t_sub_n/2-adj_n_small), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==0 & .[,which(colnames(.)==c_pros[j2])]==1),] %>% .[sample(nrow(.), adj_n_small), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==1 & .[,which(colnames(.)==c_pros[j2])]==0),] %>% .[sample(nrow(.), t_sub_n/2-adj_n_small), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==1 & .[,which(colnames(.)==c_pros[j2])]==1),] %>% .[sample(nrow(.), adj_n_small), ])
        tf$loop.count <- i
        tf$cond.note <- paste(c_pros[j1],"+",c_pros[j2])
        tf$n_profiles <- 2
        longdf <- rbind(longdf,tf)
        if(i%%100==0)print(i) #show loop counts in 100
      } 
    }else{
      for (i in 1:repeat_loop){
        tf <- rbind(df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==0 & .[,which(colnames(.)==c_pros[j2])]==0),] %>% .[sample(nrow(.), min_sub_n), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==0 & .[,which(colnames(.)==c_pros[j2])]==1),] %>% .[sample(nrow(.), min_sub_n), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==1 & .[,which(colnames(.)==c_pros[j2])]==0),] %>% .[sample(nrow(.), min_sub_n), ],
                    df2 %>% .[which(.[,which(colnames(.)==c_pros[j1])]==1 & .[,which(colnames(.)==c_pros[j2])]==1),] %>% .[sample(nrow(.), min_sub_n), ])
        tf$loop.count <- i
        tf$cond.note <- paste(c_pros[j1],"+",c_pros[j2])
        tf$n_profiles <- 2
        longdf <- rbind(longdf,tf)
        if(i%%100==0)print(i) #show loop counts in 100
      }
    }
  }
  
  rownames(longdf) <- NULL
  end_time <- Sys.time()
  print(c(start_time,end_time))
  difftime(start_time,end_time)
  ###
  #simulation done#
  ####
  longdf
  #
}

#make from 40 to 80, save in a list and save in the r_output
list_of_sims <- NULL
for(d in c(5:25)){
  t_sub_n <- d*4
  longdf <- fun_sims_with_t_sub_n(t_sub_n)
  write.table(longdf,paste0("4_r_output/sims_fixed_201904/sims_result_",t_sub_n,".csv"),sep=",",row.names = F)
  print(t_sub_n)
}