load("Tianmu_TMS_202111.RData")

############################################
##------ 先基于原始数据，计算每个月的平均 最小和最大值，然后再基于月水平的数据计算以下这些指标
# This scheme follows that of ANUCLIM & WorldClim, except that for temperature seasonality the standard deviation was used because a coefficient of variation does not make sense with temperatures between -1 and 1).

## 大气温度相关的指标
# airtemp_bio1 = Mean annual temperature
# airtemp_bio2 = Mean diurnal range of monthly temperature (mean of max temp - min temp)
# airtemp_bio3 = Isothermality (bio2/bio7) (* 100)
# airtemp_bio4 = Temperature seasonality (standard deviation *100)
# airtemp_bio5 = Max temperature of warmest month
# airtemp_bio6 = Min temperature of coldest month
# airtemp_bio7 = Temperature annual range (bio5-bio6)
# airtemp_bio10 = Mean temperature of warmest quarter/growing season (6-8月)
# airtemp_bio11 = Mean temperature of coldest quarter (12, 1 & 2三个月)

## 土壤温度相关的指标
# soiltemp_bio1 = Mean annual temperature
# soiltemp_bio2 = Mean diurnal range of monthly temperature (mean of max temp - min temp)
# soiltemp_bio3 = Isothermality (bio2/bio7) (* 100)
# soiltemp_bio4 = Temperature seasonality (standard deviation *100)
# soiltemp_bio5 = Max temperature of warmest month
# soiltemp_bio6 = Min temperature of coldest month
# soiltemp_bio7 = Temperature annual range (bio5-bio6)
# soiltemp_bio10 = Mean temperature of warmest quarter/growing season (6-8月)
# soiltemp_bio11 = Mean temperature of coldest quarter (12, 1 & 2三个月)

## 土壤湿度相关的指标
# soilMois_bio1 = Mean annual moisture
# soilMois_bio2 = Mean diurnal range of monthly moisture (mean of (max moisture - min moisture))
# soilMois_bio4 = Moisture seasonality (Coefficient of Variation)
# soilMois_bio5 = Max moisture of warmest quarter/growing season (6-8月)
# soilMois_bio6 = Min moisture of coldest quarter (12, 1 & 2三个月)
# soilMois_bio7 = Moisture annual range (bio5-bio6)
# soilMois_bio10 = Mean moisture of warmest quarter/growing season (6-8月)
# soilMois_bio11 = Mean moisture of coldest quarter (12, 1 & 2三个月)

################################
## Warning: 如果一些月份的采集天数少于20天，使用时要小心
tst <- tapply(dat$Day, list(dat$siteNew, dat$Month), function(x)length(unique(x[!is.na(x)]))); tst
rm(tst)

################################
################################
## 计算各个月的相关信息。最大值选总数据95%以上的部分数据的平均值；最小值选总数据5%以下的部分数据的中位数

##------ tempSoil6cm
# Monthly tempSoil6cm (median)
tempSoil6cm_mean <- tapply(dat$tempSoil6cm, list(dat$siteNew, dat$Month), median)
# Monthly tempSoil6cm (min)
tempSoil6cm_min <- tapply(dat$tempSoil6cm, list(dat$siteNew, dat$Month), function(x)median(sort(x)[1:round(length(x)*.05)]))
# Monthly tempSoil6cm (max)
tempSoil6cm_max <- tapply(dat$tempSoil6cm, list(dat$siteNew, dat$Month), function(x)median(sort(x,decreasing=TRUE)[1:round(length(x)*.05)]))

##------ moistSoil
# Monthly moistSoil (median)
moistSoil_mean <- tapply(dat$moistSoil, list(dat$siteNew, dat$Month), median)
# Monthly moistSoil (min)
moistSoil_min <- tapply(dat$moistSoil, list(dat$siteNew, dat$Month), function(x)median(sort(x)[1:round(length(x)*.05)]))
# Monthly moistSoil (max)
moistSoil_max <- tapply(dat$moistSoil, list(dat$siteNew, dat$Month), function(x)median(sort(x,decreasing=TRUE)[1:round(length(x)*.05)]))

##------ tempAir12cm
# Monthly tempAir12cm (median)
tempAir12cm_mean <- tapply(dat$tempAir12cm, list(dat$siteNew, dat$Month), median)
# Monthly tempAir12cm (min)
tempAir12cm_min <- tapply(dat$tempAir12cm, list(dat$siteNew, dat$Month), function(x)median(sort(x)[1:round(length(x)*.05)]))
# Monthly tempAir12cm (max)
tempAir12cm_max <- tapply(dat$tempAir12cm, list(dat$siteNew, dat$Month), function(x)median(sort(x,decreasing=TRUE)[1:round(length(x)*.05)]))


################################
################################
##------ 计算各个指标

## 大气温度相关的指标 tempAir12cm
# airtemp_bio1 = Mean annual temperature
airtemp_bio1 <- apply(tempAir12cm_mean, 1, function(x)mean(x[!is.na(x)]))
# airtemp_bio2 = Mean diurnal range of monthly temperature (mean of max temp - min temp)
airtemp_bio2 <- apply((tempAir12cm_max - tempAir12cm_min), 1, function(x)mean(x[!is.na(x)]))
# airtemp_bio4 = Temperature seasonality (standard deviation *100)
airtemp_bio4 <- apply(tempAir12cm_mean, 1, function(x)sd(x[!is.na(x)]))*100
# airtemp_bio5 = Max temperature of warmest month
airtemp_bio5 <- apply(tempAir12cm_max, 1, function(x)max(x[!is.na(x)]))
# airtemp_bio6 = Min temperature of coldest month
airtemp_bio6 <- apply(tempAir12cm_min, 1, function(x)min(x[!is.na(x)]))
# airtemp_bio7 = Temperature annual range (bio5-bio6)
airtemp_bio7 <- airtemp_bio5 - airtemp_bio6
# airtemp_bio3 = Isothermality (bio2/bio7) (* 100)
airtemp_bio3 <- airtemp_bio2/airtemp_bio7 * 100
# airtemp_bio10 = Mean temperature of warmest quarter/growing season (6-8月)
airtemp_bio10 <- apply(tempAir12cm_mean[,c(6:8)], 1, function(x)mean(x[!is.na(x)]))
# airtemp_bio11 = Mean temperature of coldest quarter (12, 1 & 2三个月)
airtemp_bio11 <- apply(tempAir12cm_mean[,c(1,2,12)], 1, function(x)mean(x[!is.na(x)]))

## 土壤温度相关的指标 tempSoil6cm
# soiltemp_bio1 = Mean annual temperature
soiltemp_bio1 <- apply(tempSoil6cm_mean, 1, function(x)mean(x[!is.na(x)]))
# soiltemp_bio2 = Mean diurnal range of monthly temperature (mean of max temp - min temp)
soiltemp_bio2 <- apply((tempSoil6cm_max - tempSoil6cm_min), 1, function(x)mean(x[!is.na(x)]))
# soiltemp_bio4 = Temperature seasonality (standard deviation *100)
soiltemp_bio4 <- apply(tempSoil6cm_mean, 1, function(x)sd(x[!is.na(x)]))*100
# soiltemp_bio5 = Max temperature of warmest month
soiltemp_bio5 <- apply(tempSoil6cm_max, 1, function(x)max(x[!is.na(x)]))
# soiltemp_bio6 = Min temperature of coldest month
soiltemp_bio6 <- apply(tempSoil6cm_min, 1, function(x)min(x[!is.na(x)]))
# soiltemp_bio7 = Temperature annual range (bio5-bio6)
soiltemp_bio7 <- soiltemp_bio5 - soiltemp_bio6
# soiltemp_bio3 = Isothermality (bio2/bio7) (* 100)
soiltemp_bio3 <- soiltemp_bio2/soiltemp_bio7 * 100
# soiltemp_bio10 = Mean temperature of warmest quarter/growing season (6-8月)
soiltemp_bio10 <- apply(tempSoil6cm_mean[,c(6:8)], 1, function(x)mean(x[!is.na(x)]))
# soiltemp_bio11 = Mean temperature of coldest quarter (12, 1 & 2三个月)
soiltemp_bio11 <- apply(tempSoil6cm_mean[,c(1,2,12)], 1, function(x)mean(x[!is.na(x)]))


## 土壤湿度相关的指标
# soilMois_bio1 = Mean annual moisture
soilMois_bio1 <- apply(moistSoil_mean, 1, function(x)mean(x[!is.na(x)]))
# soilMois_bio2 = Mean diurnal range of monthly moisture (mean of (max moisture - min moisture))
soilMois_bio2 <- apply((moistSoil_max - moistSoil_min), 1, function(x)mean(x[!is.na(x)]))
# soilMois_bio4 = Moisture seasonality (coefficient of variation)
soilMois_bio4 <- apply(moistSoil_mean, 1, function(x)sd(x[!is.na(x)]))/soilMois_bio1
# soilMois_bio10 = Mean moisture of warmest quarter/growing season (6-8月)
soilMois_bio10 <- apply(moistSoil_mean[,c(6:8)], 1, function(x)mean(x[!is.na(x)]))
# soilMois_bio11 = Mean moisture of coldest quarter (12, 1 & 2三个月)
soilMois_bio11 <- apply(moistSoil_mean[,c(1,2,12)], 1, function(x)mean(x[!is.na(x)]))
# soilMois_bio5 = Max moisture of warmest quarter/growing season (6-8月)
soilMois_bio5 <- apply(moistSoil_max[,c(6:8)], 1, function(x)mean(x[!is.na(x)]))
# soilMois_bio6 = Min moisture of coldest quarter (12, 1 & 2三个月)
soilMois_bio6 <- apply(moistSoil_min[,c(1,2,12)], 1, function(x)mean(x[!is.na(x)]))
# soilMois_bio7 = Moisture annual range (bio5-bio6)
soilMois_bio7 <- soilMois_bio5 - soilMois_bio6


####################################
## combine all the results and save
res <- cbind(airtemp_bio1,airtemp_bio2, airtemp_bio3,airtemp_bio4,airtemp_bio5,airtemp_bio6,airtemp_bio7,airtemp_bio10,airtemp_bio11,soiltemp_bio1,soiltemp_bio2, soiltemp_bio3,soiltemp_bio4,soiltemp_bio5,soiltemp_bio6,soiltemp_bio7,soiltemp_bio10,soiltemp_bio11,soilMois_bio1,soilMois_bio2,soilMois_bio4,soilMois_bio5,soilMois_bio6,soilMois_bio7,soilMois_bio10,soilMois_bio11)

rm(airtemp_bio1,airtemp_bio2, airtemp_bio3,airtemp_bio4,airtemp_bio5,airtemp_bio6,airtemp_bio7,airtemp_bio10,airtemp_bio11,soiltemp_bio1,soiltemp_bio2, soiltemp_bio3,soiltemp_bio4,soiltemp_bio5,soiltemp_bio6,soiltemp_bio7,soiltemp_bio10,soiltemp_bio11,soilMois_bio1,soilMois_bio2,soilMois_bio4,soilMois_bio5,soilMois_bio6,soilMois_bio7,soilMois_bio10,soilMois_bio11)

## save the results
write.csv(res,"Tianmushan_TMS_AllVariables_202111.csv")
