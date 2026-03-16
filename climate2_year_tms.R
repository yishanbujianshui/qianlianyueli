##########################year############################################
##soilTemp#
setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_DataFinal_TMS_201808_202010_forUse.csv",header = T,sep = ",")

results<-c()
years_monthmean<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  
  dat_18<-dat[dat$year=="2018",];dat_19<-dat[dat$year=="2019",];dat_20<-dat[dat$year=="2020",] #18年、19年、20年
  
  dat_18_08<-dat_18[dat_18$month=="8",] #18.8
  dat_18_09<-dat_18[dat_18$month=="9",] #18.9
  dat_18_10<-dat_18[dat_18$month=="10",] #18.10
  dat_18_11<-dat_18[dat_18$month=="11",] #18.11
  dat_18_12<-dat_18[dat_18$month=="12",] #18.12
  
  dat_19_01<-dat_19[dat_19$month=="1",] #19.1
  dat_19_02<-dat_19[dat_19$month=="2",] #19.2
  dat_19_03<-dat_19[dat_19$month=="3",] #19.3
  dat_19_04<-dat_19[dat_19$month=="4",] #19.4
  dat_19_05<-dat_19[dat_19$month=="5",] #19.5
  dat_19_06<-dat_19[dat_19$month=="6",] #19.6
  dat_19_07<-dat_19[dat_19$month=="7",] #19.7
  dat_19_08<-dat_19[dat_19$month=="8",] #19.8
  dat_19_09<-dat_19[dat_19$month=="9",] #19.9
  dat_19_10<-dat_19[dat_19$month=="10",]
  dat_19_11<-dat_19[dat_19$month=="11",]
  dat_19_12<-dat_19[dat_19$month=="12",]
  
  dat_20_01<-dat_20[dat_20$month=="1",] #20.1
  dat_20_02<-dat_20[dat_20$month=="2",] #20.2
  dat_20_03<-dat_20[dat_20$month=="3",] #20.3
  dat_20_04<-dat_20[dat_20$month=="4",] #20.4
  dat_20_05<-dat_20[dat_20$month=="5",] #20.5
  dat_20_06<-dat_20[dat_20$month=="6",] #20.6
  dat_20_07<-dat_20[dat_20$month=="7",] #20.7
  dat_20_08<-dat_20[dat_20$month=="8",] #20.8
  dat_20_09<-dat_20[dat_20$month=="9",] #20.9
  dat_20_10<-dat_20[dat_20$month=="10",] #20.10
  
  
  
  
  #?月每天的日均温,加和平均=？月的月均温
  month_mean1<-mean(tapply(dat_18_08$soilTemp, dat_18_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean2<-mean(tapply(dat_18_09$soilTemp, dat_18_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean3<-mean(tapply(dat_18_10$soilTemp, dat_18_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean4<-mean(tapply(dat_18_11$soilTemp, dat_18_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean5<-mean(tapply(dat_18_12$soilTemp, dat_18_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean6<-mean(tapply(dat_19_01$soilTemp, dat_19_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean7<-mean(tapply(dat_19_02$soilTemp, dat_19_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean8<-mean(tapply(dat_19_03$soilTemp, dat_19_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean9<-mean(tapply(dat_19_04$soilTemp, dat_19_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean10<-mean(tapply(dat_19_05$soilTemp, dat_19_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean11<-mean(tapply(dat_19_06$soilTemp, dat_19_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean12<-mean(tapply(dat_19_07$soilTemp, dat_19_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean13<-mean(tapply(dat_19_08$soilTemp, dat_19_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean14<-mean(tapply(dat_19_09$soilTemp, dat_19_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean15<-mean(tapply(dat_19_10$soilTemp, dat_19_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean16<-mean(tapply(dat_19_11$soilTemp, dat_19_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean17<-mean(tapply(dat_19_12$soilTemp, dat_19_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean18<-mean(tapply(dat_20_01$soilTemp, dat_20_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean19<-mean(tapply(dat_20_02$soilTemp, dat_20_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean20<-mean(tapply(dat_20_03$soilTemp, dat_20_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean21<-mean(tapply(dat_20_04$soilTemp, dat_20_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean22<-mean(tapply(dat_20_05$soilTemp, dat_20_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean23<-mean(tapply(dat_20_06$soilTemp, dat_20_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean24<-mean(tapply(dat_20_07$soilTemp, dat_20_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean25<-mean(tapply(dat_20_08$soilTemp, dat_20_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean26<-mean(tapply(dat_20_09$soilTemp, dat_20_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean27<-mean(tapply(dat_20_10$soilTemp, dat_20_10$day, FUN = function(x) mean(x)),na.rm = T)
  
  year_monthmean<-data.frame(month_mean1,month_mean2,month_mean3,month_mean4,month_mean5,month_mean6,month_mean7,month_mean8,month_mean9,month_mean10,month_mean11,month_mean12,month_mean13,month_mean14,month_mean15,month_mean16,month_mean17,month_mean18,month_mean19,month_mean20,month_mean21,month_mean22,month_mean23,month_mean24,month_mean25,month_mean26,month_mean27) #每月月均温（共22个月）；考虑换成12个月的数据
  
  year_monthmean<-t(year_monthmean)
  colnames(year_monthmean)<-unique(climate$site)[i]
  years_monthmean<-cbind(years_monthmean,year_monthmean)
  

  #年均温、气温年较差、年最高、最低温度(2018.8-2020.5)
  year_mean<-mean(year_monthmean,na.rm = T);ym_sd<-sd(year_monthmean,na.rm = T)
  year_range<-max(year_monthmean,na.rm = T)-min(year_monthmean,na.rm = T)
  year_max<-max(year_monthmean,na.rm = T)
  year_min<-min(year_monthmean,na.rm = T)
  
  result<-data.frame(year_mean,year_range,year_max,year_min,ym_sd)
  rownames(result)<-unique(climate$site)[i]
  
  results<-rbind(result,results)
}

write.csv(results,"year_soilTemp.csv")
write.csv(years_monthmean,"每月soilTemp.csv")


#####################################################################################
##surfaceTemp#
setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_DataFinal_TMS_201808_202010_forUse.csv",header = T,sep = ",")

results<-c()
years_monthmean<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  
  dat_18<-dat[dat$year=="2018",];dat_19<-dat[dat$year=="2019",];dat_20<-dat[dat$year=="2020",] #18年、19年、20年
  
  dat_18_08<-dat_18[dat_18$month=="8",] #18.8
  dat_18_09<-dat_18[dat_18$month=="9",] #18.9
  dat_18_10<-dat_18[dat_18$month=="10",] #18.10
  dat_18_11<-dat_18[dat_18$month=="11",] #18.11
  dat_18_12<-dat_18[dat_18$month=="12",] #18.12
  
  dat_19_01<-dat_19[dat_19$month=="1",] #19.1
  dat_19_02<-dat_19[dat_19$month=="2",] #19.2
  dat_19_03<-dat_19[dat_19$month=="3",] #19.3
  dat_19_04<-dat_19[dat_19$month=="4",] #19.4
  dat_19_05<-dat_19[dat_19$month=="5",] #19.5
  dat_19_06<-dat_19[dat_19$month=="6",] #19.6
  dat_19_07<-dat_19[dat_19$month=="7",] #19.7
  dat_19_08<-dat_19[dat_19$month=="8",] #19.8
  dat_19_09<-dat_19[dat_19$month=="9",] #19.9
  dat_19_10<-dat_19[dat_19$month=="10",]
  dat_19_11<-dat_19[dat_19$month=="11",]
  dat_19_12<-dat_19[dat_19$month=="12",]
  
  dat_20_01<-dat_20[dat_20$month=="1",] #20.1
  dat_20_02<-dat_20[dat_20$month=="2",] #20.2
  dat_20_03<-dat_20[dat_20$month=="3",] #20.3
  dat_20_04<-dat_20[dat_20$month=="4",] #20.4
  dat_20_05<-dat_20[dat_20$month=="5",] #20.5
  dat_20_06<-dat_20[dat_20$month=="6",] #20.6
  dat_20_07<-dat_20[dat_20$month=="7",] #20.7
  dat_20_08<-dat_20[dat_20$month=="8",] #20.8
  dat_20_09<-dat_20[dat_20$month=="9",] #20.9
  dat_20_10<-dat_20[dat_20$month=="10",] #20.10
  
  #?月每天的日均温,加和平均=？月的月均温
  month_mean1<-mean(tapply(dat_18_08$surfaceTemp, dat_18_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean2<-mean(tapply(dat_18_09$surfaceTemp, dat_18_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean3<-mean(tapply(dat_18_10$surfaceTemp, dat_18_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean4<-mean(tapply(dat_18_11$surfaceTemp, dat_18_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean5<-mean(tapply(dat_18_12$surfaceTemp, dat_18_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean6<-mean(tapply(dat_19_01$surfaceTemp, dat_19_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean7<-mean(tapply(dat_19_02$surfaceTemp, dat_19_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean8<-mean(tapply(dat_19_03$surfaceTemp, dat_19_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean9<-mean(tapply(dat_19_04$surfaceTemp, dat_19_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean10<-mean(tapply(dat_19_05$surfaceTemp, dat_19_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean11<-mean(tapply(dat_19_06$surfaceTemp, dat_19_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean12<-mean(tapply(dat_19_07$surfaceTemp, dat_19_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean13<-mean(tapply(dat_19_08$surfaceTemp, dat_19_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean14<-mean(tapply(dat_19_09$surfaceTemp, dat_19_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean15<-mean(tapply(dat_19_10$surfaceTemp, dat_19_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean16<-mean(tapply(dat_19_11$surfaceTemp, dat_19_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean17<-mean(tapply(dat_19_12$surfaceTemp, dat_19_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean18<-mean(tapply(dat_20_01$surfaceTemp, dat_20_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean19<-mean(tapply(dat_20_02$surfaceTemp, dat_20_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean20<-mean(tapply(dat_20_03$surfaceTemp, dat_20_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean21<-mean(tapply(dat_20_04$surfaceTemp, dat_20_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean22<-mean(tapply(dat_20_05$surfaceTemp, dat_20_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean23<-mean(tapply(dat_20_06$surfaceTemp, dat_20_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean24<-mean(tapply(dat_20_07$surfaceTemp, dat_20_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean25<-mean(tapply(dat_20_08$surfaceTemp, dat_20_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean26<-mean(tapply(dat_20_09$surfaceTemp, dat_20_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean27<-mean(tapply(dat_20_10$surfaceTemp, dat_20_10$day, FUN = function(x) mean(x)),na.rm = T)
  
  year_monthmean<-data.frame(month_mean1,month_mean2,month_mean3,month_mean4,month_mean5,month_mean6,month_mean7,month_mean8,month_mean9,month_mean10,month_mean11,month_mean12,month_mean13,month_mean14,month_mean15,month_mean16,month_mean17,month_mean18,month_mean19,month_mean20,month_mean21,month_mean22,month_mean23,month_mean24,month_mean25,month_mean26,month_mean27) #每月月均温（共22个月）；考虑换成12个月的数据
  
  year_monthmean<-t(year_monthmean)
  colnames(year_monthmean)<-unique(climate$site)[i]
  years_monthmean<-cbind(years_monthmean,year_monthmean)

  
  #年均温、气温年较差、年最高、最低温度(2018.8-2020.5)
  year_mean<-mean(year_monthmean,na.rm = T);ym_sd<-sd(year_monthmean,na.rm = T)
  year_range<-max(year_monthmean,na.rm = T)-min(year_monthmean,na.rm = T)
  year_max<-max(year_monthmean,na.rm = T)
  year_min<-min(year_monthmean,na.rm = T)
  
  result<-data.frame(year_mean,year_range,year_max,year_min,ym_sd)
  rownames(result)<-unique(climate$site)[i]
  
  results<-rbind(result,results)
}

write.csv(results,"year_surfaceTemp.csv")
write.csv(years_monthmean,"每月surfaceTemp.csv")

####################################################################################
##airTemp#
setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_DataFinal_TMS_201808_202010_forUse.csv",header = T,sep = ",")

results<-c()
years_monthmean<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  
  dat_18<-dat[dat$year=="2018",];dat_19<-dat[dat$year=="2019",];dat_20<-dat[dat$year=="2020",] #18年、19年、20年
  
  dat_18_08<-dat_18[dat_18$month=="8",] #18.8
  dat_18_09<-dat_18[dat_18$month=="9",] #18.9
  dat_18_10<-dat_18[dat_18$month=="10",] #18.10
  dat_18_11<-dat_18[dat_18$month=="11",] #18.11
  dat_18_12<-dat_18[dat_18$month=="12",] #18.12
  
  dat_19_01<-dat_19[dat_19$month=="1",] #19.1
  dat_19_02<-dat_19[dat_19$month=="2",] #19.2
  dat_19_03<-dat_19[dat_19$month=="3",] #19.3
  dat_19_04<-dat_19[dat_19$month=="4",] #19.4
  dat_19_05<-dat_19[dat_19$month=="5",] #19.5
  dat_19_06<-dat_19[dat_19$month=="6",] #19.6
  dat_19_07<-dat_19[dat_19$month=="7",] #19.7
  dat_19_08<-dat_19[dat_19$month=="8",] #19.8
  dat_19_09<-dat_19[dat_19$month=="9",] #19.9
  dat_19_10<-dat_19[dat_19$month=="10",]
  dat_19_11<-dat_19[dat_19$month=="11",]
  dat_19_12<-dat_19[dat_19$month=="12",]
  
  dat_20_01<-dat_20[dat_20$month=="1",] #20.1
  dat_20_02<-dat_20[dat_20$month=="2",] #20.2
  dat_20_03<-dat_20[dat_20$month=="3",] #20.3
  dat_20_04<-dat_20[dat_20$month=="4",] #20.4
  dat_20_05<-dat_20[dat_20$month=="5",] #20.5
  dat_20_06<-dat_20[dat_20$month=="6",] #20.6
  dat_20_07<-dat_20[dat_20$month=="7",] #20.7
  dat_20_08<-dat_20[dat_20$month=="8",] #20.8
  dat_20_09<-dat_20[dat_20$month=="9",] #20.9
  dat_20_10<-dat_20[dat_20$month=="10",] #20.10
  
  #?月每天的日均温,加和平均=？月的月均温
  month_mean1<-mean(tapply(dat_18_08$airTemp, dat_18_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean2<-mean(tapply(dat_18_09$airTemp, dat_18_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean3<-mean(tapply(dat_18_10$airTemp, dat_18_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean4<-mean(tapply(dat_18_11$airTemp, dat_18_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean5<-mean(tapply(dat_18_12$airTemp, dat_18_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean6<-mean(tapply(dat_19_01$airTemp, dat_19_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean7<-mean(tapply(dat_19_02$airTemp, dat_19_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean8<-mean(tapply(dat_19_03$airTemp, dat_19_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean9<-mean(tapply(dat_19_04$airTemp, dat_19_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean10<-mean(tapply(dat_19_05$airTemp, dat_19_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean11<-mean(tapply(dat_19_06$airTemp, dat_19_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean12<-mean(tapply(dat_19_07$airTemp, dat_19_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean13<-mean(tapply(dat_19_08$airTemp, dat_19_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean14<-mean(tapply(dat_19_09$airTemp, dat_19_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean15<-mean(tapply(dat_19_10$airTemp, dat_19_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean16<-mean(tapply(dat_19_11$airTemp, dat_19_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean17<-mean(tapply(dat_19_12$airTemp, dat_19_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean18<-mean(tapply(dat_20_01$airTemp, dat_20_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean19<-mean(tapply(dat_20_02$airTemp, dat_20_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean20<-mean(tapply(dat_20_03$airTemp, dat_20_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean21<-mean(tapply(dat_20_04$airTemp, dat_20_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean22<-mean(tapply(dat_20_05$airTemp, dat_20_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean23<-mean(tapply(dat_20_06$airTemp, dat_20_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean24<-mean(tapply(dat_20_07$airTemp, dat_20_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean25<-mean(tapply(dat_20_08$airTemp, dat_20_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean26<-mean(tapply(dat_20_09$airTemp, dat_20_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean27<-mean(tapply(dat_20_10$airTemp, dat_20_10$day, FUN = function(x) mean(x)),na.rm = T)
  
  year_monthmean<-data.frame(month_mean1,month_mean2,month_mean3,month_mean4,month_mean5,month_mean6,month_mean7,month_mean8,month_mean9,month_mean10,month_mean11,month_mean12,month_mean13,month_mean14,month_mean15,month_mean16,month_mean17,month_mean18,month_mean19,month_mean20,month_mean21,month_mean22,month_mean23,month_mean24,month_mean25,month_mean26,month_mean27) #每月月均温；考虑换成12个月的数据
  
  year_monthmean<-t(year_monthmean)
  colnames(year_monthmean)<-unique(climate$site)[i]
  years_monthmean<-cbind(years_monthmean,year_monthmean)

  
  #年均温、气温年较差、年最高、最低温度(2018.8-2020.5)
  year_mean<-mean(year_monthmean,na.rm = T);ym_sd<-sd(year_monthmean,na.rm = T)
  year_range<-max(year_monthmean,na.rm = T)-min(year_monthmean,na.rm = T)
  year_max<-max(year_monthmean,na.rm = T)
  year_min<-min(year_monthmean,na.rm = T)
  
  result<-data.frame(year_mean,year_range,year_max,year_min,ym_sd)
  rownames(result)<-unique(climate$site)[i]
  
  results<-rbind(result,results)
}

write.csv(results,"year_airTemp.csv")
write.csv(years_monthmean,"每月airTemp.csv")

################################################################################
##moisSoil#
setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_DataFinal_TMS_201808_202010_forUse.csv",header = T,sep = ",")

results<-c()
years_monthmean<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  
  dat_18<-dat[dat$year=="2018",];dat_19<-dat[dat$year=="2019",];dat_20<-dat[dat$year=="2020",] #18年、19年、20年
  
  dat_18_08<-dat_18[dat_18$month=="8",] #18.8
  dat_18_09<-dat_18[dat_18$month=="9",] #18.9
  dat_18_10<-dat_18[dat_18$month=="10",] #18.10
  dat_18_11<-dat_18[dat_18$month=="11",] #18.11
  dat_18_12<-dat_18[dat_18$month=="12",] #18.12
  
  dat_19_01<-dat_19[dat_19$month=="1",] #19.1
  dat_19_02<-dat_19[dat_19$month=="2",] #19.2
  dat_19_03<-dat_19[dat_19$month=="3",] #19.3
  dat_19_04<-dat_19[dat_19$month=="4",] #19.4
  dat_19_05<-dat_19[dat_19$month=="5",] #19.5
  dat_19_06<-dat_19[dat_19$month=="6",] #19.6
  dat_19_07<-dat_19[dat_19$month=="7",] #19.7
  dat_19_08<-dat_19[dat_19$month=="8",] #19.8
  dat_19_09<-dat_19[dat_19$month=="9",] #19.9
  dat_19_10<-dat_19[dat_19$month=="10",]
  dat_19_11<-dat_19[dat_19$month=="11",]
  dat_19_12<-dat_19[dat_19$month=="12",]
  
  dat_20_01<-dat_20[dat_20$month=="1",] #20.1
  dat_20_02<-dat_20[dat_20$month=="2",] #20.2
  dat_20_03<-dat_20[dat_20$month=="3",] #20.3
  dat_20_04<-dat_20[dat_20$month=="4",] #20.4
  dat_20_05<-dat_20[dat_20$month=="5",] #20.5
  dat_20_06<-dat_20[dat_20$month=="6",] #20.6
  dat_20_07<-dat_20[dat_20$month=="7",] #20.7
  dat_20_08<-dat_20[dat_20$month=="8",] #20.8
  dat_20_09<-dat_20[dat_20$month=="9",] #20.9
  dat_20_10<-dat_20[dat_20$month=="10",] #20.10
  
  #?月每天的日均温,加和平均=？月的月均温
  month_mean1<-mean(tapply(dat_18_08$moisSoil, dat_18_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean2<-mean(tapply(dat_18_09$moisSoil, dat_18_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean3<-mean(tapply(dat_18_10$moisSoil, dat_18_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean4<-mean(tapply(dat_18_11$moisSoil, dat_18_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean5<-mean(tapply(dat_18_12$moisSoil, dat_18_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean6<-mean(tapply(dat_19_01$moisSoil, dat_19_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean7<-mean(tapply(dat_19_02$moisSoil, dat_19_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean8<-mean(tapply(dat_19_03$moisSoil, dat_19_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean9<-mean(tapply(dat_19_04$moisSoil, dat_19_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean10<-mean(tapply(dat_19_05$moisSoil, dat_19_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean11<-mean(tapply(dat_19_06$moisSoil, dat_19_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean12<-mean(tapply(dat_19_07$moisSoil, dat_19_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean13<-mean(tapply(dat_19_08$moisSoil, dat_19_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean14<-mean(tapply(dat_19_09$moisSoil, dat_19_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean15<-mean(tapply(dat_19_10$moisSoil, dat_19_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean16<-mean(tapply(dat_19_11$moisSoil, dat_19_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean17<-mean(tapply(dat_19_12$moisSoil, dat_19_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean18<-mean(tapply(dat_20_01$moisSoil, dat_20_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean19<-mean(tapply(dat_20_02$moisSoil, dat_20_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean20<-mean(tapply(dat_20_03$moisSoil, dat_20_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean21<-mean(tapply(dat_20_04$moisSoil, dat_20_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean22<-mean(tapply(dat_20_05$moisSoil, dat_20_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean23<-mean(tapply(dat_20_06$moisSoil, dat_20_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean24<-mean(tapply(dat_20_07$moisSoil, dat_20_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean25<-mean(tapply(dat_20_08$moisSoil, dat_20_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean26<-mean(tapply(dat_20_09$moisSoil, dat_20_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean27<-mean(tapply(dat_20_10$moisSoil, dat_20_10$day, FUN = function(x) mean(x)),na.rm = T)
  
  year_monthmean<-data.frame(month_mean1,month_mean2,month_mean3,month_mean4,month_mean5,month_mean6,month_mean7,month_mean8,month_mean9,month_mean10,month_mean11,month_mean12,month_mean13,month_mean14,month_mean15,month_mean16,month_mean17,month_mean18,month_mean19,month_mean20,month_mean21,month_mean22,month_mean23,month_mean24,month_mean25,month_mean26,month_mean27) #每月月均温（共22个月）；考虑换成12个月的数据
  
  year_monthmean<-t(year_monthmean)
  colnames(year_monthmean)<-unique(climate$site)[i]
  years_monthmean<-cbind(years_monthmean,year_monthmean)
  
  
  #年均温、气温年较差、年最高、最低温度(2018.8-2020.5)
  year_mean<-mean(year_monthmean,na.rm = T);ym_sd<-sd(year_monthmean,na.rm = T)
  year_range<-max(year_monthmean,na.rm = T)-min(year_monthmean,na.rm = T)
  year_max<-max(year_monthmean,na.rm = T)
  year_min<-min(year_monthmean,na.rm = T)
  
  result<-data.frame(year_mean,year_range,year_max,year_min,ym_sd)
  rownames(result)<-unique(climate$site)[i]
  
  results<-rbind(result,results)
}

write.csv(results,"year_moisSoil.csv")
write.csv(years_monthmean,"每月moisSoil.csv")





