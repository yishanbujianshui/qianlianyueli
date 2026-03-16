#linear mixed-effects models#

#install.packages('lmerTest')
library(lmerTest)

###随机森林#####

library(vegan)
library(rfPermute)
library(randomForest)
library(rfUtilities)


library(lattice)
library(grid)
library(DMwR)
library(rpart)
library(ipred)
library(randomForest)



setwd("C:/Users/WennyIsACoolLady/Desktop")

env<-read.csv("Env_2022.4.28_Tianshan _NO_NA.csv",header=T,check.names = F)

env<-c(env[1:2],env[5:12],env[15:28])

env<-as.data.frame(env)

#选择1: 初步挑选出共线性不强的因子# 不考虑海拔，只带入随海拔变化的因子#

env1<-env[,c(4,5,7:10,12,14,16,17,19,21:24)]



#(#必要时进行数据清洗：#平均值填充NA值#)

#env <- env[-manyNAs(env,0.2), ]#占有20%的NA值的行去掉

#env1 <- knnImputation(env1,k=10)  #平均值填充NA值,清洗后的数据


#(必要时进行数据转换：#log(1+x)#)

env1[1:15] <- log1p(env1[1:15])##

attach(env1)

##—————————————alpha_env模型筛选————————————————————————————————————————


#读取alpha多样性数据

alpha<-read.csv("Bacteria alpha index_Tianshan.csv",header=T,check.names = F)

options(scipen=100,digits=4)


#共线性筛选(此处可以跳过)#

#library("corrplot")


#matrix1<-cor(env1[,c(3:15)],env1[,c(3:15)])
#corrplot(matrix1,type = c("upper"),diag = FALSE,addCoef.col = "gray20")

#去掉(系数>0.8)



#

rfP1 <- rfPermute(alpha$observed_species ~ pH + SOC + Total_K + NH4_N + NO3_N + P_Shannon + P_Pielou + soiltemp_bio2 + soiltemp_bio4 + soiltemp_bio10 + soilmois_bio4 + soilmois_bio10, data = env1, num.cores = 3, nrep = 1000)


importance(rfP1)

(result<-as.data.frame(rp.importance(rfP1)))
(result<-result[,-3:-4])

write.csv(result,"Richness_RF.csv")


#
rfP2 <- rfPermute(alpha$shannon ~ pH + SOC + Total_K + NH4_N + NO3_N + P_Shannon + P_Pielou + soiltemp_bio2 + soiltemp_bio4 + soiltemp_bio10 + soilmois_bio4 + soilmois_bio10, num.cores = 3, nrep = 1000)


rp.importance(rfP2)

(result<-as.data.frame(rp.importance(rfP2)))
(result<-result[,-3:-4])

write.csv(result,"Shannon_RF.csv")


#######################################################################################


#ggplot2绘制柱状图#

library(ggplot2)

setwd("C:/Users/WennyIsACoolLady/Desktop")

#OTU richness重要性分解#

dat<-read.csv("Richness_RF.csv")

dat<-dat[which(dat$p<0.1),]

dat<-dat[1:5,]

attach(dat)

colors <- c("green","orange","brown")

barplot(value,main="OTU Richness",names.arg=variable,ylab="Importance (%IncMSE)",col=colors,space = c(0,0.2))


dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)
dat$variable <- factor(dat$variable, levels=c("TC","NH4_N", "TP","pH","soilTemp_GS678_daily_min"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "OTU richness", x = 'Category',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=variable), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序


##重新作图##

#a#

dat<-read.csv("Richness_RF.csv")

dat<-dat[which(dat$p<0.1),]  #以p值小于0.1为阈值#

dat$variable <- factor(dat$variable, levels=c("pH","soilTemp_MiniGS", "TP","SWC","NH4","ShrubPro1"), ordered=TRUE)

ggplot(dat,mapping = aes(variable,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "OTU richness", x = 'Variable',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,5,10,15))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=value), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序


#b#

dat<-read.csv("Shannon_RF.csv")

dat<-dat[which(dat$p<0.1),]  #以p值小于0.1为阈值#

dat$variable <- factor(dat$variable, levels=c("soilTemp_MiniGS","pH", "TP","ShrubPro1","SWC","TN"), ordered=TRUE)

ggplot(dat,mapping = aes(variable,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "Shannon-Wiener index", x = 'Variable',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,5,10,15))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=value), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序


#c#

dat<-read.csv("PD_RF.csv")

dat<-dat[which(dat$p<0.1),]  #以p值小于0.1为阈值#

dat$variable <- factor(dat$variable, levels=c("soilTemp_MiniGS","EBPro1", "soilTemp_Seasonality_Annual","TP","NH4"), ordered=TRUE)

ggplot(dat,mapping = aes(variable,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "Faith's PD", x = 'Variable',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,5,10,15))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=value), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序



#d#

dat<-read.csv("PSV_RF.csv")

dat<-dat[which(dat$p<0.1),]  #以p值小于0.1为阈值#

dat$variable <- factor(dat$variable, levels=c("SWC","TP"), ordered=TRUE)

ggplot(dat,mapping = aes(variable,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "PSV", x = 'Variable',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,5,10,15))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=value), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序

























#Shannon diverSity重要性分解#

dat<-read.csv("2.csv")

dat<-dat[which(dat$p<0.1),]

dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)
dat$variable <- factor(dat$variable, levels=c("NH4_N", "TP","pH","soilTemp_GS678_daily_min"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  geom_bar(stat='identity',position='stack') +
  labs(title  = "Shannon-Winner index", x = 'Category',y = 'Importance (%IncMSE)') +
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  geom_text(aes(label=variable), size = 6, hjust = 0.5, vjust = 0, position = position_stack(0.5))+
  guides(fill=F)#此句改变图例顺序


#Faith's PD重要性分解#

dat<-read.csv("3.csv")

dat<-dat[which(dat$p<0.1),]

dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)
dat$variable <- factor(dat$variable, levels=c("NH4_N", "TP","soilTemp_GS678_daily_min","DB_Proportion1","P_Shannon_Diversity"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  geom_bar(stat='identity',position='stack') +
  labs(title  = "Faith's PD", x = 'Category',y = 'Importance (%IncMSE)') +
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  geom_text(aes(label=variable), size = 6, hjust = 0.5, vjust = 4, position = "stack")+
  guides(fill=F)#此句改变图例顺序


#MPD_01重要性分解#

dat<-read.csv("rF4_01.csv")


dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)
dat$variable <- factor(dat$variable, levels=c("SWC", "pH", "soilTemp_GS678_daily_min"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "PSV (MPD)", x = 'Category',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=variable), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序



#MPD_abund重要性分解#

dat<-read.csv("rF5_abund.csv")


dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)
dat$variable <- factor(dat$variable, levels=c("SWC", "pH", "soilTemp_GS678_daily_min"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "PSV (MPD_abund)", x = 'Category',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=variable), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序


##—————————————beta_env模型筛选————————————————————————————————————————

#生成NMDS axis数据


library(vegan)	#排序分析

setwd("C:/Users/WennyIsACoolLady/Desktop")
otu <- read.csv('OTU_final_bacteria.csv', row.names = 1, sep = ',')
otu <- data.frame(t(otu))


#1.—#基于Bray的前两轴（基于 OTU 丰度表）——————

nmds1 <- metaMDS(otu, distance = 'bray')  #默认K=2

#可简要查看结果
nmds1
#或
summary(nmds1)

#提取应力函数值（stress）
(nmds1.stress <- nmds1$stress)

#提取样本排序坐标
nmds1.point <- data.frame(nmds1$point)

write.table(nmds1.point, 'nmds_axis_bray.csv', sep = ',',row.names = T,col.names = T )


#2.—#基于Jaccard的前两轴（基于 OTU 丰度表）————————

nmds2 <- metaMDS(otu, distance = 'jaccard')

#可简要查看结果
nmds2
#或
summary(nmds2)

#提取应力函数值（stress）
(nmds2.stress <- nmds2$stress)

#提取样本排序坐标
nmds2.point <- data.frame(nmds2$point)

write.table(nmds2.point, 'nmds_axis_jac.csv', sep = ',',row.names = T,col.names = T )


#3.#基于Weighted UniFrac的前两轴（基于距离矩阵）————————

dis <- read.csv('Weighted UniFrac.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
nmds3 <- metaMDS(as.dist(dis))

summary(nmds3)

#提取应力函数值（stress）
(nmds3.stress <- nmds3$stress)

#提取样本排序坐标
nmds3.point <- data.frame(nmds3$point)

write.table(nmds3.point, 'nmds_axis_weiUni.csv', sep = ',',row.names = T,col.names = T )





#3.#基于Unweighted UniFrac的前两轴（基于距离矩阵）————————

dis <- read.csv('unWeighted UniFrac.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
nmds4 <- metaMDS(as.dist(dis))

summary(nmds4)

#提取应力函数值（stress）
(nmds4.stress <- nmds4$stress)

#提取样本排序坐标
nmds4.point <- data.frame(nmds4$point)

write.table(nmds4.point, 'nmds_axis_unUni.csv', sep = ',',row.names = T,col.names = T )






#读取NMDS axis数据


#1. Bray

nmds_axis<-read.csv("nmds_axis_bray.csv",header=T,check.names = F)

options(scipen=100,digits=4)

#1
rf_axis1 <- rfPermute(nmds_axis$MDS1 ~ pH + SWC + AP + AK + TK + TN + TP + CN + PPielou + ShrubPro1 + EBPro1 + soilTemp_MiniGS + soilTemp_RanGS + soilMois_RanGS + soilTemp_Seasonality_Annual + soilMois_Seasonality_GS, data = env1,num.cores = 5, nrep = 1000)  

rp.importance(rf_axis1)

(result1<-as.data.frame(rp.importance(rf_axis1)))
(result1<-result1[,-3:-4])

#% Var explained: 73.77#

write.csv(result1,"bray_axis1_RF.csv")

#2
rf_axis2 <- rfPermute(nmds_axis$MDS2 ~ pH + SWC + AP + AK + TK + TN + TP + CN + PPielou + ShrubPro1 + EBPro1 + soilTemp_MiniGS + soilTemp_RanGS + soilMois_RanGS + soilTemp_Seasonality_Annual + soilMois_Seasonality_GS, data = env1,num.cores = 5, nrep = 1000)  

rp.importance(rf_axis2)

(result2<-as.data.frame(rp.importance(rf_axis2)))
(result2<-result2[,-3:-4])

#% Var explained: 61.42#

write.csv(result2,"bray_axis2_RF.csv")



#2. Jac

nmds_axis<-read.csv("nmds_axis_jac.csv",header=T,check.names = F)

options(scipen=100,digits=4)

#1
rf_axis1 <- rfPermute(nmds_axis$MDS1 ~ pH + SWC + AP + AK + TK + TN + TP + CN + NH4 + NO3 + PPielou + ShrubPro1 + EBPro1 + soilTemp_MiniGS + soilTemp_RanGS + soilMois_RanGS + soilTemp_Seasonality_Annual + soilMois_Seasonality_GS, data = env1, num.cores = 5, nrep = 1000)  

rp.importance(rf_axis1)

(result1<-as.data.frame(rp.importance(rf_axis1)))
(result1<-result1[,-3:-4])

#% Var explained: 74.49#

write.csv(result1,"jac_axis1_RF.csv")


#2
rf_axis2 <- rfPermute(nmds_axis$MDS2 ~ pH + SWC + AP + AK + TK + TN + TP + CN + NH4 + NO3 + PPielou + ShrubPro1 + EBPro1 + soilTemp_MiniGS + soilTemp_RanGS + soilMois_RanGS + soilTemp_Seasonality_Annual + soilMois_Seasonality_GS, data = env1, num.cores = 5, nrep = 1000)  

rp.importance(rf_axis2)

(result2<-as.data.frame(rp.importance(rf_axis2)))
(result2<-result2[,-3:-4])

#% Var explained: 60.13#

write.csv(result2,"jac_axis2_RF.csv")



#3. Weighted UniFrac

nmds_axis<-read.csv("nmds_axis_weiUni.csv",header=T,check.names = F)

options(scipen=100,digits=4)

#1
rf_axis1 <- rfPermute(nmds_axis$MDS1 ~ pH + SWC + AP + AK + TN + TP + CN + PPielou + EBPro1 + soilTemp_MiniGS + soilTemp_RanGS + soilMois_RanGS + soilTemp_Seasonality_Annual, data = env1,num.cores = 4, nrep = 1000)  

rp.importance(rf_axis1)

(result1<-as.data.frame(rp.importance(rf_axis1)))
(result1<-result1[,-3:-4])

#% Var explained: 65.84#

write.csv(result1,"WeiUni_axis1_RF.csv")


#2
rf_axis2 <- rfPermute(nmds_axis$MDS2 ~ pH + SWC + AP + AK + TN + TP + CN + PPielou + EBPro1 + soilTemp_MiniGS + soilTemp_RanGS + soilMois_RanGS + soilTemp_Seasonality_Annual, data = env1,num.cores = 4, nrep = 1000)

rp.importance(rf_axis2)

(result2<-as.data.frame(rp.importance(rf_axis2)))
(result2<-result2[,-3:-4])

#% Var explained: 50.35#

write.csv(result2,"WeiUni_axis2_RF.csv")




#4. Unweighted UniFrac

nmds_axis<-read.csv("nmds_axis_unUni.csv",header=T,check.names = F)

options(scipen=100,digits=4)

#1
rf_axis1 <- rfPermute(nmds_axis$MDS1 ~ pH + SWC + AP + AK + TK + TN + TP + CN + NH4 + NO3 + PSR + PPielou + ShrubPro1 + EBPro1 + soilTemp_MiniGS + soilTemp_RanGS + soilMois_RanGS + soilTemp_Seasonality_Annual + soilMois_Seasonality_GS, data = env1, num.cores = 6, nrep = 1000)

rp.importance(rf_axis1)

(result1<-as.data.frame(rp.importance(rf_axis1)))
(result1<-result1[,-3:-4])

#% Var explained: 75.36#

write.csv(result1,"UnUni_axis1_RF.csv")


#2
rf_axis2 <- rfPermute(nmds_axis$MDS2 ~ pH + SWC + AP + AK + TK + TN + TP + CN + NH4 + NO3 + PSR + PPielou + ShrubPro1 + EBPro1 + soilTemp_MiniGS + soilTemp_RanGS + soilMois_RanGS + soilTemp_Seasonality_Annual + soilMois_Seasonality_GS, data = env1, num.cores = 6, nrep = 1000)

rp.importance(rf_axis2)

(result2<-as.data.frame(rp.importance(rf_axis2)))
(result2<-result2[,-3:-4])

#% Var explained: 68.27#

write.csv(result2,"UnUni_axis2_RF.csv")









#绘图#

#_________________________bray________________________________________#

library(ggplot2)

#axis1重要性分解#

dat<-read.csv("rF_axis1.csv")

dat<-dat[which(dat$p<0.1),]

dat$variable <- factor(dat$variable, levels=c("SWC","NH4_N", "C_N","TK", "pH","soilTemp_GS678_daily_min","Shrub_Proportion1"), ordered=TRUE)
dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "Community composition no.1", x = 'Category',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=variable), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序




#axis2重要性分解#

dat<-read.csv("rF_axis2.csv")

dat<-dat[which(dat$p<0.1),]

dat$variable <- factor(dat$variable, levels=c("SWC", "TC","TP","TK", "pH","soilTemp_GS678_daily_min","EB_Proportion1","Shrub_Proportion1"), ordered=TRUE)
dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "Community composition no.2", x = 'Category',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=variable), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序


#_________________________UnweightedUniFrac________________________________________#


#axis1重要性分解#

dat<-read.csv("rF_UnweightedUniFrac_axis1.csv")

dat<-dat[which(dat$p<0.1),]

dat$variable <- factor(dat$variable, levels=c("SWC","NH4_N", "C_N","TK", "pH","soilTemp_GS678_daily_min","Shrub_Proportion1"), ordered=TRUE)
dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "Community composition no.1", x = 'Category',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=variable), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序




#axis2重要性分解#

dat<-read.csv("rF_UnweightedUniFrac_axis2.csv")

dat<-dat[which(dat$p<0.1),]

dat$variable <- factor(dat$variable, levels=c("SWC", "TC","NH4_N","TP","pH","soilTemp_GS678_daily_min","EB_Proportion1","Shrub_Proportion1"), ordered=TRUE)
dat$category <- factor(dat$category, levels=c("Soil", "Microclimate", "Plant"), ordered=TRUE)


attach(dat)

ggplot(dat,mapping = aes(category,value,fill=variable))+
  
  geom_bar(stat='identity',position='stack') +
  
  labs(title  = "Community composition no.2", x = 'Category',y = 'Importance (%IncMSE)') +
  
  scale_y_continuous(breaks = c(0,10,20,30,40))+
  
  theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),axis.title =element_text(size = 26),axis.text =element_text(size = 26, color = 'black'),
      plot.margin = margin(1, 1, 1, 1, "cm"),
  )+
  
  geom_text(aes(label=variable), size = 6, stat="identity", position = "stack", vjust=4)+
  
  guides(fill=F)#此句改变图例顺序



