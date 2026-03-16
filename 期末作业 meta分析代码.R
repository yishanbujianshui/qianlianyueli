#载入所有程序包
library(readxl)
require(ggmap)
library(metafor)
library(ggplot2)
library(glmulti)
library(dplyr)
library(esc)
library(metagear)
library(boot)
library(export)
require(maps)
library(tidyr)

#制作采样点地图
d0 <- read_excel("土壤呼吸数据.xlsx")
mapWorld <- annotation_borders("world", colour="gray50", fill="gray65")
mp <- ggplot() + theme_bw()+  mapWorld+xlab("Longitude") + ylab("Latitude")+
  theme(axis.text=element_text(size=16,face="bold", color="black"),
        axis.title=element_text(size=16,face="bold"))
mp <- mp+ geom_point(data=d0 , aes(x=Longitude, y=Latitude), size=2, color="red")
mp

#输出地图
graph2ppt(file="Map.ppt", width=5, height=3.5)

#读入分析数据
d1 <- read_excel("土壤呼吸数据.xlsx", col_types = c("text", "text", "text", "text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))

##查看数据
View(d1)
names(d1)
str(d1)

#格式转换
numeric_cols <- c("treatment_mean", "treatment_sd", "treatment_n","control_mean","control_sd","control_n","MAT (℃)","MAP (mm)")
clean_numeric <- function(x) {
  if(is.character(x)) {
    x <- trimws(x)  # 去除首尾空格
    x <- gsub("[^0-9.eE+-]", "", x)  # 只保留有效字符
  }
  as.numeric(x)
}
d1[numeric_cols] <- lapply(d1[numeric_cols], clean_numeric)

#meta分析第一步：计算各个案例研究的效应值和研究内方差
##各命令的意义:m1i,处理的均值；sd1i，处理的标准差；n1i，处理的样本量
##m2i，对照的均值；sd2i=对照的标准差，n2i=对照的样本量
d2_1<-escalc(measure="ROM",data=d1,m1i=treatment_mean,sd1i=treatment_sd,n1i=treatment_n,m2i=control_mean,sd2i=control_sd,n2i=control_n)
#删除NA值
d2 <- drop_na(d2_1, c("yi", "vi"))

##查看计算出了效应值的数据，即d2
View(d2)

### 可以导出数据
### write.csv(d2, file="d2.csv")
#Meta分析第二步: 计算所有案例研究的累计效应值
##分别采用随机效应模型和混合效应模型计算所有案例研究的平均(累计)效应值
##并依据对Qt的异质性检验判断是否要引入影响因素(解释变量)
##固定效应模型，采用rma命令，需指定method="FE"
##对效应值yi按从小到大的顺序进行排序(非必要步骤)
d2<- d2[order(-d2$yi),]

##固定效应模型运算，指定method="FE"
fixed1<-rma(yi,vi, data=d2, method="FE")

##查看结果
summary(fixed1)

##森林图:单个案例的效应值和置信区间的展示
par(mar=c(5, 4, 4, 2), mgp=c(2.5, 0.8, 0))
n_studies <- nrow(d2)
forest(d2$yi, d2$vi, annotate=FALSE,
       slab=NA, pch=19, col="#3498DB", cex=0.7, psize=0.8,
       xlab="Log response ratio (lnRR)", xlim=c(-5, 5))
abline(v=0, lty=1, col="gray30", lwd=1.5)
abline(v=coef(fixed1), lty=2, col="#E74C3C", lwd=2)
points(d2$yi, n_studies:1, pch=19, cex=0.6, col="#3498DB")
text(4.5, n_studies + 2, "lnRR [95% CI]", font=2, cex=0.9, pos=2)
legend("topright", legend=c("Overall effect", "Zero line"),
       lty=c(2, 1), col=c("#E74C3C", "gray30"), lwd=c(2, 1.5), cex=0.85, box.lty=0)

##随机效应模型（指定采用的方法为限制性最大似然法REML，也可以指定其他方法）
random1<-rma(yi,vi, data=d2, method="REML")
summary(random1)
par(mar=c(5, 6, 2, 3))##设置作图区的下左上右边缘宽度
attach(d2)
fd3<- d2[order(-yi),]
attach(fd3)
forest(yi, vi, annotate=FALSE, col="grey", slab=NA, xlab="Log response ratio", pch=20,efac=0,font=2,cex=1,psize=1,xlim=c(-10,10), lty=c("solid","blank"))
addpoly(random1, mlab=NA, efac=1,annotate=FALSE, cex=1)
addpoly(fixed1, mlab=NA, efac=1,annotate=FALSE, cex=1)
abline(v=coef(random1), lty="solid",col="red", lwd=2)
abline(v=coef(fixed1), lty="solid",col="blue", lwd=2)
abline(v=0, lty="dotted",col="red")
kk<-381
points(fd3$yi, kk:1, pch=19, cex=0.5)

##两模型结果对比作图，分别提取两模型的平均效应值和方差，并作图
par(mar=c(6, 5, 4, 2), mgp=c(2.5, 0.8, 0))
estimates <- c(coef(random1), coef(fixed1))
variances <- c(vcov(random1), vcov(fixed1))
ci_low <- estimates - 1.96 * sqrt(variances)
ci_high <- estimates + 1.96 * sqrt(variances)
labels <- c("Random effect model", "Fixed effect model")
forest(estimates, variances, slab=labels, ylim=c(-0.5, 5.5),
       font=2, cex=1.2, psize=1.5, xlab="Effect size (lnRR)",
       xlim=c(-2, 2), col=c("#E74C3C", "#3498DB"),
       annotate=TRUE, efac=1.5)
text(c(-0.8, 0.3), 4.8, c("Model type", "Mean [95% CI]"), cex=1.2, font=2)
text(c(-0.8), 4.3, c("Random effect model"), cex=1.1, font=2, pos=4)
text(estimates[1], 4.3, sprintf("%.3f [%.3f, %.3f]", estimates[1], ci_low[1], ci_high[1]), cex=1.1, font=2, pos=4)
text(c(-0.8), 3.3, c("Fixed effect model"), cex=1.1, font=2, pos=4)
text(estimates[2], 3.3, sprintf("%.3f [%.3f, %.3f]", estimates[2], ci_low[2], ci_high[2]), cex=1.1, font=2, pos=4)
abline(v=0, lty=2, col="gray50", lwd=1.5)

##计算某一类群的累计效应值，subset命令，
##如单独计算因子Climatic.region类型为Humid region或Arid region时对应的数据的累计效应值
r2<-rma(yi,vi, data=subset(d2,Climatic.region=="Arid region"), method="REML")
r3<-rma(yi,vi, data=subset(d2,Climatic.region=="Humid region"), method="REML")

##查看模型结果
summary(r2)
summary(r3)

####得到tau^2, I^2等指标的均值与置信区间
confint(r2)

##对结果作图，首先分别提取两个模型分别对累计效应值的均值和方差的估计值
par(mar=c(6, 5, 4, 2), mgp=c(2.5, 0.8, 0))
estimates2 <- c(coef(r2), coef(r3))
variances2 <- c(vcov(r2), vcov(r3))
ci_low2 <- estimates2 - 1.96 * sqrt(variances2)
ci_high2 <- estimates2 + 1.96 * sqrt(variances2)
labels2 <- c("Arid region", "Humid region")
forest(estimates2, variances2, slab=labels2, ylim=c(-0.5, 5.5),
       font=2, cex=1.2, psize=1.5, xlab="Effect size (lnRR)",
       xlim=c(-2, 2), col=c("#E67E22", "#27AE60"),
       annotate=TRUE, efac=1.5)
text(c(-0.8, 0.3), 4.8, c("Climatic region", "Mean [95% CI]"), cex=1.2, font=2)
text(c(-0.8), 4.3, c("Arid region"), cex=1.1, font=2, pos=4)
text(estimates2[1], 4.3, sprintf("%.3f [%.3f, %.3f]", estimates2[1], ci_low2[1], ci_high2[1]), cex=1.1, font=2, pos=4)
text(c(-0.8), 3.3, c("Humid region"), cex=1.1, font=2, pos=4)
text(estimates2[2], 3.3, sprintf("%.3f [%.3f, %.3f]", estimates2[2], ci_low2[2], ci_high2[2]), cex=1.1, font=2, pos=4)
abline(v=0, lty=2, col="gray50", lwd=1.5)

##如单独计算因子Ecosystem类型为Forest\Wetland或Grassland时对应的数据的累计效应值
r4<-rma(yi,vi, data=subset(d2,Ecosystem=="Forest"), method="REML")
r5<-rma(yi,vi, data=subset(d2,Ecosystem=="Grassland"), method="REML")
r6<-rma(yi,vi, data=subset(d2,Ecosystem=="Wetland"), method="REML")

##对结果作图，首先分别提取两个模型分别对累计效应值的均值和方差的估计值
par(mar=c(6, 5, 4, 2), mgp=c(2.5, 0.8, 0))
estimates21 <- c(coef(r4), coef(r5), coef(r6))
variances21 <- c(vcov(r4), vcov(r5), vcov(r6))
ci_low21 <- estimates21 - 1.96 * sqrt(variances21)
ci_high21 <- estimates21 + 1.96 * sqrt(variances21)
labels21 <- c("Forest", "Grassland", "Wetland")
forest(estimates21, variances21, slab=labels21, ylim=c(-0.5, 6.5),
       font=2, cex=1.2, psize=1.5, xlab="Effect size (lnRR)",
       xlim=c(-2, 2), col=c("#27AE60", "#F39C12", "#3498DB"),
       annotate=TRUE, efac=1.5)
text(c(-0.8, 0.3), 5.8, c("Ecosystem", "Mean [95% CI]"), cex=1.2, font=2)
text(c(-0.8), 5.3, c("Forest"), cex=1.1, font=2, pos=4)
text(estimates21[1], 5.3, sprintf("%.3f [%.3f, %.3f]", estimates21[1], ci_low21[1], ci_high21[1]), cex=1.1, font=2, pos=4)
text(c(-0.8), 4.3, c("Grassland"), cex=1.1, font=2, pos=4)
text(estimates21[2], 4.3, sprintf("%.3f [%.3f, %.3f]", estimates21[2], ci_low21[2], ci_high21[2]), cex=1.1, font=2, pos=4)
text(c(-0.8), 3.3, c("Wetland"), cex=1.1, font=2, pos=4)
text(estimates21[3], 3.3, sprintf("%.3f [%.3f, %.3f]", estimates21[3], ci_low21[3], ci_high21[3]), cex=1.1, font=2, pos=4)
abline(v=0, lty=2, col="gray50", lwd=1.5)

#Meta分析第三步，引入解释变量（模型从随机效应模型转变为混合效应模型），即检验某些影响因素是否对效应值有显著影响
numeric_cols_1 <- c("MAT....", "MAP..mm.")
d2[numeric_cols_1] <- lapply(d2[numeric_cols_1], as.numeric)
r7<-rma(yi,vi, mods=~MAT....,data=d2, method="REML")
summary(r7)
r8<-rma(yi,vi, mods=~MAP..mm.,data=d2, method="REML")
summary(r8)
unique_vals <- unique(model.matrix(r7)[, "MAT...."])
preds <- predict(r7, newmods = unique_vals)
wi<- 1/sqrt(d2$vi)
size <-0.8 + 3.0 * (wi- min(wi))/(max(wi)- min(wi))
plot(d2$MAT...., d2$yi, pch=19, cex=size, col="black",ylab="",
     xlab="MAT",
     las=1, bty="l", font=2,cex.lab=1.5)
lines(unique_vals, preds$pred)
lines(unique_vals, preds$ci.lb, lty="dashed")
lines(unique_vals, preds$ci.ub, lty="dashed")
abline(h=0, lty="dotted",col="red",lwd=2)
title(ylab="Effect size (log resposne ratio)", line=2, cex.lab=1.5)
text(c(500), 4.5, c("Qm=15.4975,P<0.0001"),cex=1.2,font=1,col="red")

unique_vals <- unique(model.matrix(r8)[, "MAP..mm."])
preds <- predict(r8, newmods = unique_vals)
wi<- 1/sqrt(d2$vi)
size <-0.8 + 3.0 * (wi- min(wi))/(max(wi)- min(wi))
plot(d2$MAP..mm., d2$yi, pch=19, cex=size, col="grey",ylab="",
     xlab="MAP",
     las=1, bty="l", font=2,cex.lab=1.5)
lines(0:53, preds$pred)
lines(0:53, preds$ci.lb, lty="dashed")
lines(0:53, preds$ci.ub, lty="dashed")
abline(h=0, lty="dotted",col="red",lwd=2)
title(ylab="Effect size (log resposne ratio)", line=2, cex.lab=1.5)
text(c(500), 4.5, c("Qm=15.4975,P<0.0001"),cex=1.2,font=1,col="red")
#Meta分析第四步，模型诊断，判定结果的可靠性.
##发表偏爱性诊断--漏斗图
funnel(r2,main="Arid region")
funnel(r3,main="Humid region")
funnel(r4,main="Forest")
funnel(r5,main="Grassland")
funnel(r6,main="Wetland")
##对漏斗图的对称性进行检验
regtest(r2, model="rma")
regtest(r3, model="rma")
regtest(r4, model="rma")
regtest(r5, model="rma")
regtest(r6, model="rma")
##失安全系数计算
fsn(yi,vi, data=d2)
