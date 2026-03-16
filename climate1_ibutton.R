setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_AirClimFinal_iButton_201808_202005.csv",header = T,sep = ",")

##############day################################################
#####1.日均温、气温日较差、日最高、最低温度(采样当天,2018.8.10)#################
results1<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  dat_18<-dat[dat$year=="2018",]
  dat_18_08<-dat_18[dat_18$month=="8",]
  dat_18_08_10<-dat_18_08[dat_18_08$day=="10",]
  
  #日均温、气温日较差、日最高、最低温度(采样当天,2018.8.10)
  daily_mean<-mean(dat_18_08_10$tempAir); dm_sd<-sd(dat_18_08_10$tempAir)
  daily_range<-max(dat_18_08_10$tempAir)-min(dat_18_08_10$tempAir)
  daily_max<-max(dat_18_08_10$tempAir)
  daily_min<-min(dat_18_08_10$tempAir)

  result1<-data.frame(daily_mean,daily_range,daily_max,daily_min,dm_sd)
  rownames(result1)<-unique(climate$site)[i]
  
  results1<-rbind(result1,results1)
}

write.csv(results1,"day_air_temp.csv")

#######2.日均湿度、湿度日较差、日最高、最低湿度(采样当天,2018.8.10)##########
results2<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  dat_18<-dat[dat$year=="2018",]
  dat_18_08<-dat_18[dat_18$month=="8",]
  dat_18_08_10<-dat_18_08[dat_18_08$day=="10",]
  
  #日均温、气温日较差、日最高、最低温度(采样当天,2018.8.10)
  daily_mean<-mean(dat_18_08_10$moisAir); dm_sd<-sd(dat_18_08_10$moisAir)
  daily_range<-max(dat_18_08_10$moisAir)-min(dat_18_08_10$moisAir)
  daily_max<-max(dat_18_08_10$moisAir)
  daily_min<-min(dat_18_08_10$moisAir)
  
  result2<-data.frame(daily_mean,daily_range,daily_max,daily_min,dm_sd)
  rownames(result2)<-unique(climate$site)[i]
  
  results2<-rbind(result2,results2)
}

write.csv(results2,"day_air_mois.csv")

##########################month############################################
###3.月均温、气温月较差、月最高、最低温度(采样当月,2018.8)

results3<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  dat_18<-dat[dat$year=="2018",]
  dat_18_08<-dat_18[dat_18$month=="8",]
  dat_18_08_10<-dat_18_08[dat_18_08$day=="10",]
  
  #月均温、气温月较差、月最高、最低温度(采样当月,2018.8)
  month8_day_mean<-tapply(dat_18_08$tempAir, dat_18_08$day, FUN = function(x) mean(x))  #8月每天的日均温
  
  month8_mean<-mean(month8_day_mean);m8m_sd<-sd(month8_day_mean)
  month8_range<-max(month8_day_mean)-min(month8_day_mean)
  month8_max<-max(month8_day_mean)
  month8_min<-min(month8_day_mean)
  
  result3<-data.frame(month8_mean,month8_range,month8_max,month8_min,m8m_sd)
  rownames(result3)<-unique(climate$site)[i]
  
  results3<-rbind(result3,results3)
}

write.csv(results3,"month8_air_temp.csv")


###4.月均湿度、湿度月较差、月最高、最低湿度(采样当月,2018.8)
results4<-c()
for (i in length(unique(climate$site)):1){
  dat<-climate[climate$site==unique(climate$site)[i],]
  dat_18<-dat[dat$year=="2018",]
  dat_18_08<-dat_18[dat_18$month=="8",]
  dat_18_08_10<-dat_18_08[dat_18_08$day=="10",]
  
  #月均湿度、湿度月较差、月最高、最低湿度(采样当月,2018.8)
  month8_day_mean<-tapply(dat_18_08$moisAir, dat_18_08$day, FUN = function(x) mean(x))  #8月每天的日均湿度
  
  month8_mean<-mean(month8_day_mean);m8m_sd<-sd(month8_day_mean)
  month8_range<-max(month8_day_mean)-min(month8_day_mean)
  month8_max<-max(month8_day_mean)
  month8_min<-min(month8_day_mean)
  
  result4<-data.frame(month8_mean,month8_range,month8_max,month8_min,m8m_sd)
  rownames(result4)<-unique(climate$site)[i]
  
  results4<-rbind(result4,results4)
}

write.csv(results4,"month8_air_mois.csv")

##########################year############################################
###5.年均温、气温年较差、年最高、最低温度(2018.8-2020.5)
setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_AirClimFinal_iButton_201808_202005.csv",header = T,sep = ",")

results5<-c()
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
  
  #?月每天的日均温,加和平均=？月的月均温
  month_mean1<-mean(tapply(dat_18_08$tempAir, dat_18_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean2<-mean(tapply(dat_18_09$tempAir, dat_18_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean3<-mean(tapply(dat_18_10$tempAir, dat_18_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean4<-mean(tapply(dat_18_11$tempAir, dat_18_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean5<-mean(tapply(dat_18_12$tempAir, dat_18_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean6<-mean(tapply(dat_19_01$tempAir, dat_19_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean7<-mean(tapply(dat_19_02$tempAir, dat_19_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean8<-mean(tapply(dat_19_03$tempAir, dat_19_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean9<-mean(tapply(dat_19_04$tempAir, dat_19_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean10<-mean(tapply(dat_19_05$tempAir, dat_19_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean11<-mean(tapply(dat_19_06$tempAir, dat_19_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean12<-mean(tapply(dat_19_07$tempAir, dat_19_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean13<-mean(tapply(dat_19_08$tempAir, dat_19_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean14<-mean(tapply(dat_19_09$tempAir, dat_19_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean15<-mean(tapply(dat_19_10$tempAir, dat_19_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean16<-mean(tapply(dat_19_11$tempAir, dat_19_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean17<-mean(tapply(dat_19_12$tempAir, dat_19_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean18<-mean(tapply(dat_20_01$tempAir, dat_20_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean19<-mean(tapply(dat_20_02$tempAir, dat_20_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean20<-mean(tapply(dat_20_03$tempAir, dat_20_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean21<-mean(tapply(dat_20_04$tempAir, dat_20_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean22<-mean(tapply(dat_20_05$tempAir, dat_20_05$day, FUN = function(x) mean(x)),na.rm = T)
  
  year_monthmean<-data.frame(month_mean1,month_mean2,month_mean3,month_mean4,month_mean5,month_mean6,month_mean7,month_mean8,month_mean9,month_mean10,month_mean11,month_mean12,month_mean13,month_mean14,month_mean15,month_mean16,month_mean17,month_mean18,month_mean19,month_mean20,month_mean21,month_mean22) #每月月均温（共22个月）；考虑换成12个月的数据
  
  year_monthmean<-t(year_monthmean)
  colnames(year_monthmean)<-unique(climate$site)[i]
  years_monthmean<-cbind(years_monthmean,year_monthmean)
  
  #年均温、气温年较差、年最高、最低温度(2018.8-2020.5)
  year_mean<-mean(year_monthmean,na.rm = T);ym_sd<-sd(year_monthmean,na.rm = T)
  year_range<-max(year_monthmean,na.rm = T)-min(year_monthmean,na.rm = T)
  year_max<-max(year_monthmean,na.rm = T)
  year_min<-min(year_monthmean,na.rm = T)
  
  result5<-data.frame(year_mean,year_range,year_max,year_min,ym_sd)
  rownames(result5)<-unique(climate$site)[i]
  
  results5<-rbind(result5,results5)
}

write.csv(results5,"year_air_temp.csv")
write.csv(years_monthmean,"每月月均温.csv")


###6.年均湿度、湿度年较差、年最高、最低湿度(2018.8-2020.5)
setwd("C:/Users/WennyIsACoolLady/Desktop")
climate<-read.csv("Guanshan_AirClimFinal_iButton_201808_202005.csv",header = T,sep = ",")

results6<-c()
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
  
  #?月每天的日均温,加和平均=？月的月均温
  month_mean1<-mean(tapply(dat_18_08$moisAir, dat_18_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean2<-mean(tapply(dat_18_09$moisAir, dat_18_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean3<-mean(tapply(dat_18_10$moisAir, dat_18_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean4<-mean(tapply(dat_18_11$moisAir, dat_18_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean5<-mean(tapply(dat_18_12$moisAir, dat_18_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean6<-mean(tapply(dat_19_01$moisAir, dat_19_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean7<-mean(tapply(dat_19_02$moisAir, dat_19_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean8<-mean(tapply(dat_19_03$moisAir, dat_19_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean9<-mean(tapply(dat_19_04$moisAir, dat_19_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean10<-mean(tapply(dat_19_05$moisAir, dat_19_05$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean11<-mean(tapply(dat_19_06$moisAir, dat_19_06$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean12<-mean(tapply(dat_19_07$moisAir, dat_19_07$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean13<-mean(tapply(dat_19_08$moisAir, dat_19_08$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean14<-mean(tapply(dat_19_09$moisAir, dat_19_09$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean15<-mean(tapply(dat_19_10$moisAir, dat_19_10$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean16<-mean(tapply(dat_19_11$moisAir, dat_19_11$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean17<-mean(tapply(dat_19_12$moisAir, dat_19_12$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean18<-mean(tapply(dat_20_01$moisAir, dat_20_01$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean19<-mean(tapply(dat_20_02$moisAir, dat_20_02$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean20<-mean(tapply(dat_20_03$moisAir, dat_20_03$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean21<-mean(tapply(dat_20_04$moisAir, dat_20_04$day, FUN = function(x) mean(x)),na.rm = T)
  month_mean22<-mean(tapply(dat_20_05$moisAir, dat_20_05$day, FUN = function(x) mean(x)),na.rm = T)
  
  year_monthmean<-data.frame(month_mean1,month_mean2,month_mean3,month_mean4,month_mean5,month_mean6,month_mean7,month_mean8,month_mean9,month_mean10,month_mean11,month_mean12,month_mean13,month_mean14,month_mean15,month_mean16,month_mean17,month_mean18,month_mean19,month_mean20,month_mean21,month_mean22) #每月月均温（共22个月）；考虑换成12个月的数据
  
  year_monthmean<-t(year_monthmean)
  colnames(year_monthmean)<-unique(climate$site)[i]
  years_monthmean<-cbind(years_monthmean,year_monthmean)
  
  #年均温、气温年较差、年最高、最低温度(2018.8-2019.9)
  year_mean<-mean(year_monthmean,na.rm = T);ym_sd<-sd(year_monthmean,na.rm = T)
  year_range<-max(year_monthmean,na.rm = T)-min(year_monthmean,na.rm = T)
  year_max<-max(year_monthmean,na.rm = T)
  year_min<-min(year_monthmean,na.rm = T)
  
  result6<-data.frame(year_mean,year_range,year_max,year_min,ym_sd)
  rownames(result6)<-unique(climate$site)[i]
  
  results6<-rbind(result6,results6)
}

write.csv(results6,"year_air_mois.csv")
write.csv(years_monthmean,"每月月均湿度.csv")


#plot6特殊#
a<-c(94.2,
     88.2,
     84.3,
     85.4,
     78,
     96.4,
     92.6
)

sd(a)
mean(a)
max(a)-min(a)


b<-c(91,94.2,85.4,92.6)
sd(b)
mean(b)
max(b)-min(b)

(mean(a)+mean(b))/2
