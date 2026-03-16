setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_DataFinal_TMS_201808_202010_forUse.csv",header = T,sep = ",")

##############day################################################
results<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  dat_18<-dat[dat$year=="2018",]
  dat_18_08<-dat_18[dat_18$month=="8",]
  dat_18_08_10<-dat_18_08[dat_18_08$day=="10",]
  
  variable<-dat_18_08_10$soilTemp
  #variable<-dat_18_08_10$surfaceTemp
  #variable<-dat_18_08_10$airTemp
  #variable<-dat_18_08_10$moisSoil
  
  daily_mean<-mean(variable); dm_sd<-sd(variable)
  daily_range<-max(variable)-min(variable)
  daily_max<-max(variable)
  daily_min<-min(variable)

  result<-data.frame(daily_mean,daily_range,daily_max,daily_min,dm_sd)
  rownames(result)<-unique(climate$site)[i]
  
  results<-rbind(result,results)
}

write.csv(results,"day_soilTemp.csv")
#write.csv(results,"day_surfaceTemp.csv")
#write.csv(results,"day_airTemp.csv")
#write.csv(results,"day_moisSoil.csv")

##########################month############################################
setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_DataFinal_TMS_201808_202010_forUse.csv",header = T,sep = ",")

results<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  dat_18<-dat[dat$year=="2018",]
  dat_18_08<-dat_18[dat_18$month=="8",]
  
  
  #variable<-dat_18_08$soilTemp
  #variable<-dat_18_08$surfaceTemp
  #variable<-dat_18_08$airTemp
  variable<-dat_18_08$moisSoil
  
  month8_day_mean<-tapply(variable, dat_18_08$day, FUN = function(x) mean(x))  #8月每天的日均温
  
  month8_mean<-mean(month8_day_mean);m8m_sd<-sd(month8_day_mean)
  month8_range<-max(month8_day_mean)-min(month8_day_mean)
  month8_max<-max(month8_day_mean)
  month8_min<-min(month8_day_mean)
  month8_sd<-sd(month8_day_mean)
  
  result<-data.frame(month8_mean,month8_range,month8_max,month8_min,m8m_sd)
  rownames(result)<-unique(climate$site)[i]
  
  results<-rbind(result,results)
}

#write.csv(results,"month8_soilTemp.csv")
#write.csv(results,"month8_surfaceTemp.csv")
#write.csv(results,"month8_airTemp.csv")
write.csv(results,"month8_moisSoil.csv")


