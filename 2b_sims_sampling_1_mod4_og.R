#20181210 log: add mst, change 5 e.p into only 4 (mst,ind,ndvi,airport)
#20190110 log: set to ind750
#20190123 log: add the changeable n from 40/80 to 40/44....80(small n=10,11....20)

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
#raw_cta_500m <- read.table("2_GIS_output/cta_500m_buffer.txt",sep=",",header=T)
#raw_cta_750m <- read.table("2_GIS_output/cta_750m_buffer.txt",sep=",",header=T)
raw_tcm_500m <- read.table("2_GIS_output/tcm_500m_buffer.txt",sep=",",header=T)
raw_tcm_750m <- read.table("2_GIS_output/tcm_750m_buffer.txt",sep=",",header=T)
raw_ndvi_300m <- read.table("2_GIS_output/ndvi_300m.txt",sep=",",header=T)
raw_ndvi_500m <- read.table("2_GIS_output/ndvi_500m.txt",sep=",",header=T)
raw_ndvi_750m <- read.table("2_GIS_output/ndvi_750m.txt",sep=",",header=T)

raw_dist_airport <- read.table("2_GIS_output/fc_to_near_airport_ft.txt",sep=",",header=T)
raw_dist_cta <- read.table("2_GIS_output/fc_to_near_cta_ft.txt",sep=",",header=T)
raw_dist_ind <- read.table("2_GIS_output/fc_to_near_ind_ft.txt",sep=",",header=T)

raw_ind_500m <- fread("2_GIS_output/ind_500m_buffer_area.txt",sep=",",header=T)
raw_ind_750m <- fread("2_GIS_output/ind_750m_buffer_area.txt",sep=",",header=T)

#sum up the ind area by ORIG_FID
agg_ind_500m <-aggregate(raw_ind_500m$Shape_Area, by=list(raw_ind_500m$ORIG_FID), FUN=sum, na.rm=TRUE)%>%  `colnames<-`(c("oid","ind_500m"))
agg_ind_750m <-aggregate(raw_ind_750m$Shape_Area, by=list(raw_ind_750m$ORIG_FID), FUN=sum, na.rm=TRUE)%>%  `colnames<-`(c("oid","ind_750m"))

#make a big data frame, by ORIG_FID(n=14755), with different character
clean_df <- cbind(raw_grid$OBJECTID,
                  raw_dist_airport$NEAR_DIST*0.3048,raw_dist_cta$NEAR_DIST*0.3048,raw_dist_ind$NEAR_DIST*0.3048,
                  raw_tcm_500m$match_traffic_count_list_Exc_Total_Passing_Vehicle_Volume,
                  raw_tcm_750m$match_traffic_count_list_Exc_Total_Passing_Vehicle_Volume,
                  raw_ndvi_300m$MEAN,raw_ndvi_500m$MEAN,raw_ndvi_750m$MEAN) %>% 
  as.data.frame %>%  `colnames<-`(c("oid", "dist_airport_m", "dist_cta_m","dist_ind_m","tcm_500m","tcm_750m","ndvi_300m","ndvi_500m","ndvi_750m"))

clean_df2 <- merge(clean_df,agg_ind_500m,by="oid",all=T) %>% merge(.,agg_ind_750m,by="oid",all=T)
#data clean up and set
#save it
write.table(clean_df2,"2_GIS_output/gis_output_clean.csv",sep=",",row.names = F)
#
raw_big <- clean_df2
raw_big <- read.table("2_GIS_output/gis_output_clean.csv",sep=",",header=T)
#
df2 <- raw_big
#fill NA with 0 (only for the one should have values)
df2$ind_500m <- ifelse(is.na(df2$ind_500m),0,df2$ind_500m)
df2$ind_750m <- ifelse(is.na(df2$ind_750m),0,df2$ind_750m)
df2$mst_500m <- ifelse(is.na(df2$mst_500m),0,df2$mst_500m)
df2$mst_750m <- ifelse(is.na(df2$mst_750m),0,df2$mst_750m)

#make the condition (bi)  
df2$bi_tcm_500 <- ifelse(df2$tcm_500m>=20000,1,0)
df2$bi_tcm_750 <- ifelse(df2$tcm_750m>=20000,1,0)
df2$bi_ind_500 <- ifelse(df2$ind_500m==0,0,1)
df2$bi_ind_750 <- ifelse(df2$ind_750m==0,0,1)
df2$bi_cta_500 <- ifelse(df2$dist_cta_m<=500,1,0) 
df2$bi_cta_750 <- ifelse(df2$dist_cta_m<=750,1,0)
df2$bi_airport_5k <- ifelse(df2$dist_airport_m<=5000,1,0) 
df2$bi_airport_8k <- ifelse(df2$dist_airport_m<=8000,1,0) 
df2$bi_ndvi_300 <- ifelse(df2$ndvi_300m>=0.35,1,0)
df2$bi_ndvi_500 <- ifelse(df2$ndvi_500m>=0.35,1,0)
df2$bi_ndvi_750 <- ifelse(df2$ndvi_750m>=0.35,1,0)
df2$bi_mst_500 <- ifelse(df2$mst_500m==0,0,1)
df2$bi_mst_750 <- ifelse(df2$mst_750m==0,0,1)

##
##simulation set
##
longdf <- NULL
c_pros <- c("bi_mst_500","bi_ind_750","bi_airport_5k","bi_ndvi_300") #change into 4ep
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
  write.table(longdf,paste0("4_r_output/sims_fixed_bad/sims_result_",t_sub_n,".csv"),sep=",",row.names = F)
  print(t_sub_n)
}