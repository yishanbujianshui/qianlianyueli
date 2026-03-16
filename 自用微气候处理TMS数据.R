rm(list = ls())
options(stringsAsFactors = FALSE)
library(openxlsx)
library(dplyr)
library(lubridate)
library(readxl)
library(purrr)
library(readr)
加载工作路径
# 读取并处理所有xlsx文件
file_paths <- list.files(path = "C:/Users/qianl/Desktop/TMS数据霸王岭2303-2509/2301-2508TMS/1",
                         pattern = "\\.xlsx$",
                         full.names = TRUE)
data_list <- map(file_paths, read_excel)
# 3. 找出所有表格的共有列名
common_cols <- reduce(map(data_list, names), intersect)
# 4. 提取每个表格的共有列并合并
combined_data <- do.call(rbind,
                         lapply(data_list, function(df) {
                           df[common_cols] <- lapply(df[common_cols], function(x) {
                             tryCatch(as.POSIXct(x), error = function(e) as.character(x))
                           })
                           df[common_cols]
                         })
)

standardize_columns <- function(df, common_cols) {
  for(col in common_cols) {
    if(col %in% names(df)) {
      # 尝试统一转换为字符型
      df[[col]] <- as.character(df[[col]])
    }
  }
  return(df)
}

# 3. 对每个数据框统一数据类型后合并
combined_data <- do.call(rbind,
                         lapply(data_list, function(df) {
                           df_subset <- df[common_cols]
                           standardize_columns(df_subset, common_cols)
                         })
)
#备用合并方案
combined_data <- map_dfr(data_list, ~ .x %>%
                           select(any_of(common_cols)) %>%
                           select(where(~ !all(is.na(.)))))


# 以b列为基准去重
final_data <- as_tibble(combined_data) %>%
  distinct("Beijing Time", .keep_all = TRUE)

#载入数据
dat <- read.xlsx("S2.xlsx")
dat=final_data
dat=combined_data
target_cols <- c("Soil temperature -6 cm","Surface temperature","Air temperature +12 cm","Vol. moisture")
dat <- dat %>%
  mutate_at(vars(all_of(target_cols)),
            ~as.numeric(as.character(.)))
#数据处理
dat <- dat %>%
  rename(
    datetime     = `Beijing Time`,
    tempSoil6cm  = `Soil temperature -6 cm`,
    tempSurf0cm  = `Surface temperature`,       # 如果后面不用，可以忽略
    tempAir12cm  = `Air temperature +12 cm`,
    moistSoil    = `Vol. moisture`
    # chazhi 列不参与计算，就不用改名也没关系
  )
class(dat$datetime)
str(dat$datetime)
dat$datetime <- as.character(dat$datetime)
class(dat$datetime)
sum(is.na(dat$datetime))
dat$datetime <- trimws(dat$datetime)
dat$datetime <- gsub("/", ".", dat$datetime)
dat$datetime <- ymd_hm(dat$datetime, tz = "Asia/Shanghai")
dat <- dat %>%
  mutate(
    Year  = as.integer(format(datetime, "%Y")),
    Month = as.integer(format(datetime, "%m")),
    Day   = as.integer(format(datetime, "%d"))
  )
dat$Yearmonth <- paste(dat$Year, dat$Month, dat$Day, sep = "-")
head(dat)
summary(dat$datetime)
range(dat$datetime, na.rm = TRUE)


#计算各个月的相关信息。最大值选总数据95%以上的部分数据的平均值；最小值选总数据5%以下的部分数据的平均值
if(!"Site" %in% colnames(dat)) {
  dat$Site <- 1  # 如果有多个站点，可以根据实际情况修改，此处假设为同一个站点
}
##------ tempSoil6cm
# Monthly tempSoil6cm (median)
tempSoil6cm_median <- tapply(dat$tempSoil6cm, list(dat$Site, dat$Month), function(x)median(x[!is.na(x)]))
# Monthly tempSoil6cm (min)
tempSoil6cm_min <- tapply(dat$tempSoil6cm, list(dat$Site, dat$Month), function(x)mean(sort(x[!is.na(x)])[1:round(length(x)*.05)]))
# Monthly tempSoil6cm (max)
tempSoil6cm_max <- tapply(dat$tempSoil6cm, list(dat$Site, dat$Month), function(x)mean(sort(x[!is.na(x)],decreasing=TRUE)[1:round(length(x)*.05)]))

##------ moistSoil
# Monthly moistSoil (median)
moistSoil_median <- tapply(dat$moistSoil, list(dat$Site, dat$Month), median)
# Monthly moistSoil (min)
moistSoil_min <- tapply(dat$moistSoil, list(dat$Site, dat$Month), function(x)mean(sort(x)[1:round(length(x)*.05)]))
# Monthly moistSoil (max)
moistSoil_max <- tapply(dat$moistSoil, list(dat$Site, dat$Month), function(x)mean(sort(x,decreasing=TRUE)[1:round(length(x)*.05)]))

##------ tempAir12cm
# Monthly tempAir12cm (median)
tempAir12cm_median <- tapply(dat$tempAir12cm, list(dat$Site, dat$Month), function(x)median(x[!is.na(x)]))
# Monthly tempAir12cm (min)
tempAir12cm_min <- tapply(dat$tempAir12cm, list(dat$Site, dat$Month), function(x)mean(sort(x)[1:round(length(x)*.05)]))
# Monthly tempAir12cm (max)
tempAir12cm_max <- tapply(dat$tempAir12cm, list(dat$Site, dat$Month), function(x)mean(sort(x,decreasing=TRUE)[1:round(length(x)*.05)]))


################################
################################
##------ 计算各个指标

## 大气温度相关的指标 tempAir12cm
# airtemp_bio1 = Mean annual temperature
airtemp_bio1 <- apply(tempAir12cm_median, 1, function(x)mean(x[!is.na(x)]))
# airtemp_bio4 = Temperature seasonality (standard deviation *100)
airtemp_bio4 <- apply(tempAir12cm_median, 1, function(x)sd(x[!is.na(x)]))*100


## 土壤温度相关的指标 tempSoil6cm
# soiltemp_bio1 = Mean annual temperature
soiltemp_bio1 <- apply(tempSoil6cm_median, 1, function(x)mean(x[!is.na(x)]))
# soiltemp_bio4 = Temperature seasonality (standard deviation *100)
soiltemp_bio4 <- apply(tempSoil6cm_median, 1, function(x)sd(x[!is.na(x)]))*100



## 土壤湿度相关的指标
# soilMois_bio1 = Mean annual moisture
soilMois_bio1 <- apply(moistSoil_median, 1, function(x)mean(x[!is.na(x)]))
# soilMois_bio4 = Moisture seasonality (coefficient of variation)
soilMois_bio4 <- apply(moistSoil_median, 1, function(x)sd(x[!is.na(x)]))/soilMois_bio1




####################################
## combine all the results and save
res <- cbind(airtemp_bio1,airtemp_bio4,
             soiltemp_bio1,soiltemp_bio4,
             soilMois_bio1,soilMois_bio4
)
## save the results
write.csv(res,"山名_TMS_时间范围.csv")#文件名根据山名和时间范围订正
###### ===== 基于TMS观测数据计算微气候Bioclim指标（气温、土壤温度与土壤湿度）=====######


##------ 先基于原始数据，计算每个月的中位数 最小和最大值，然后再基于月水平的数据计算以下这些指标
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
file.choose()

date0=read.csv("C:\\Users\\qianl\\Desktop\\山名_TMS_时间范围.csv")
date0=res

date0 <- date0 %>%
  rename(
    aMat = `airtemp_bio1`,
    aTs = `airtemp_bio4`,
    sMat = `soiltemp_bio1`,
    sTs = `soiltemp_bio4`,
    sMom = `soilMois_bio1`,
    sMs = `soilMois_bio4`,
    site = `X`
  )
write.csv(date0,"山名_TMS_时间范围.csv")
