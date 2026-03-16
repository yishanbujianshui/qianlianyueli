##如对本代码有任何疑问、建议请联系本人， 张霜，中科院生态环境研究中心，shuangzhang@rcees.ac.cn
##练习数据来源于Benítez-López, A., R. Alkemade, A. M. Schipper, D. J. Ingram, P. A. Verweij, J. A. J. Eikelboom, and M. A. J. Huijbregts. 2017. The impact of hunting on tropical mammal and bird populations. Science 356:180-183.
##为了便于练习，将原数据中含有缺失值的数据行全部删去，特此说明
##设置工作目录
##读入数据
##加载能够直接读取excel文件的包，readxl
setwd("D:/document")
library(readxl)
require(ggmap)
install.packages("metafor")
install.packages("export")
library(metafor)
library(ggplot2)
library(glmulti)
library(dplyr)
library(esc)
library(metagear)
library(boot)

library(export)
graph2ppt(file="Map.ppt", width=5, height=3.5)

require(maps)

d0 <- read_excel("Benitez-Lopez_et_al._2017_Science.xlsx")

mapWorld <- borders("world", colour="gray50", fill="gray65") # create a layer of borders
mp <- ggplot() + theme_bw()+  mapWorld+xlab("Longitude") + ylab("Latitude")+
  theme(axis.text=element_text(size=16,face="bold", color="black"),
        axis.title=element_text(size=16,face="bold"))
mp <- mp+ geom_point(data=d0 , aes(x=longitude, y=latitude), size=2, color="red")
mp

library(export)
graph2ppt(file="Map.ppt", width=5, height=3.5)





#####读入要分析的案例数据

d1 <- read_excel("meta_sampledata.xls")

##查看数据
View(d1)
variable.names(d1)
##或者
## d1 <- read.csv("meta_sampledata.csv")

##加载要用到的软件包
library(metafor)
library(ggplot2)
library(glmulti)

#meta分析第一步：计算各个案例研究的效应值和研究内方差


##计算效应值，案例中采用的是log response ration (ROM)
##各命令的意义:m1i,处理的均值；sd1i，处理的标准差；n1i，处理的样本量
##m2i，对照的均值；sd2i=对照的标准差，n2i=对照的样本量

d2<-escalc(measure="ROM",data=d1,m1i=treatment_mean,sd1i=treatment_sd,n1i=treatment_n,m2i=control_mean,sd2i=control_sd,n2i=control_n)

##查看计算出了效应值的数据，即d2
View(d2)
##查看计算出来的效应值与研究内方差与原文中给出的效应值与研究内方差的关系
plot(yi~rr,data=d2)
plot(vi~var,data=d2)

### 可以导出数据
###write.csv(d2, file="d2.csv")

###二联表数据效应值的计算
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

##########################不同类型效应值的标准化转换

library(esc)

###把不同类型的效应值转化到同一标准之下
###二联表数据

### 二联表数据 odds ratio
e1 <- esc_2x2(grp1yes = 30, grp1no = 50, grp2yes = 40, grp2no = 45,
              es.type = "logit", study = "Study 2")
### t值
e2 <- esc_t(p = 0.03, grp1n = 100, grp2n = 150, es.type = "logit", study = "Study 3")

###均值， SD
e3 <- esc_mean_sd(grp1m = 7, grp1sd = 2, grp1n = 50, grp2m = 9, grp2sd = 3,
                  grp2n = 60, es.type = "logit", study = "Study 4")

mydat <- combine_esc(e1, e2, e3)
View(mydat)



#Meta分析第二步: 计算所有案例研究的累效应值

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
forest(fixed1,annotate=FALSE, 
       slab=NA,pch=20,efac=0,font=2,cex=1,psize=1,lty=c("solid","blank"))
abline(v=0, lty="solid",col="red") 
abline(v=coef(fixed1), lty="dotted",col="red") 
kk<-1779
points(d2$yi, kk:1, pch=19, cex=0.5, col="red")


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
kk<-1779
points(fd3$yi, kk:1, pch=19, cex=0.5)
### text(c(-6), 1800, c("overall effect size"),cex=1.5,font=2)



##两模型结果对比作图，分别提取两模型的平均效应值和方差，并作图

estimates <- c(coef(random1), coef(fixed1))
variances <- c(vcov(random1), vcov(fixed1))
labels <- c("Random effect model", "Fixed effect model")


forest(estimates, variances,slab=labels,ylim=c(0.8, 4.5),font=2,cex=1.5,psize=1,xlab="Effect size (log response ratio)")
text(c(-0.6,-0.1), 2.7, c("Model type", "Mean [95% CI]"),cex=1.5,font=2)


##计算某一类群的累计效应值，subset命令，
##如单独计算因子group类型为Birds或Mammals时对应的数据的累计效应值

r2<-rma(yi,vi, data=subset(d2,group=="Birds"), method="REML")
r3<-rma(yi,vi, data=subset(d2,group=="Mammals"), method="REML")

##查看模型结果
summary(r2)
summary(r3)


####得到tau^2, I^2等指标的均值与置信区间
confint(r2)


##对结果作图，首先分别提取两个模型分别对累计效应值的均值和方差的估计值
estimates2 <- c(coef(r2), coef(r3))
variances2 <- c(vcov(r2), vcov(r3))
labels2 <- c("Birds", "Mammals")
forest(estimates2, variances2,slab=labels2,ylim=c(0.8, 4.5),font=2,cex=1.5,psize=1,xlab="Effect size (log response ratio)")
text(c(-0.62,-0.08), 2.7, c("Group", "Mean [95% CI]"),cex=1.5,font=2)



#Meta分析第三步，引入解释变量（模型从随机效应模型转变为混合效应模型），即检验某些影响因素是否对效应值有显著影响

##解释变量为分类变量时的分析
##在示例数据中，依据模型r1的结果（通过summary(random1)查看），
##其效应值整体异质性指标Qt(df = 1778) = 52841.2851, p-val < .0001
##说明数据的异质性很强，有必要引入解释变量对其进行解释。
##比如可分析动物所属的类群(birds or mammal)对效应值有无影响，即不同动物类群受打猎的影响可能不同。
###固定效应模型
fr4<-rma(yi,vi, mods=~group,data=d2, method="FE")
summary(fr4)

### 直接获取模型对不同group类群累计效应值及其置信区间的估计值
fr5<-rma(yi,vi, mods=~group-1,data=d2, method="FE")
summary(fr5)

estimates_fr5 <- coef(summary(fr5))[,1]

## Birds 和Mammals各自的的方差
se_fr5 <- coef(summary(fr5))[,2]
variance_fr5<-se_fr5^2 


labels_fr5 <- c("Birds", "Mammals")
forest(
  estimates_fr5,
  variance_fr5,
  slab = labels_fr5,ylim=c(0.8, 4.5),
  font = 2,
  cex = 1.5,
  psize = 1,
  xlab = "Effect size (log response ratio)"
)
text(c(-0.54,-0.30), 2.7, c("Group", "Mean [95% CI]"),cex=1.5,font=2)
text(c(-0.4), 1.4, c("Qm= 47.46,df=1, P<0.0001"),cex=1.2,font=1,col="red")

### dev.off() 




###混合效应模型
r4<-rma(yi,vi, mods=~group,data=d2, method="REML")
summary(r4)

###########F检验替代Q检验，t值替代Q值
r4_1<-rma(yi,vi, mods=~group,data=d2, method="REML", test="knha")
summary(r4_1)



##其结果中，效应值组间异质性指标：QM(df = 1) = 0.1274, p-val = 0.7211
##说明group的不同水平之间（即Birds vs Mammals）的效应值差别不显著，即这两个类群受打猎的影响相似，无显著差别

##直接查看模型对不同group水平的估计值，在解释变量后加上“-1”即可
##注意：此模型的Qm值无意义，只需查看模型对group不同水平的平均效应值及其置信区间的估计即可

r5<-rma(yi,vi, mods=~group-1,data=d2, method="REML")
summary(r5)

##依据模型r5的结果作图
##提取r5的参数估计结果

coef(summary(r5))

## Birds 和Mammals各自的的累计效应值

estimates3 <- coef(summary(r5))[,1]

## Birds 和Mammals各自的的方差
se3 <- coef(summary(r5))[,2]
variance3<-se3^2 


labels3 <- c("Birds", "Mammals")
forest(
  estimates3,
  variance3,
  slab = labels3,ylim=c(0.8, 4.5),
  font = 2,
  cex = 1.5,
  psize = 1,
  xlab = "Effect size (log response ratio)"
)
text(c(-0.65,-0.06), 2.7, c("Group", "Mean [95% CI]"),cex=1.5,font=2)
text(c(-0.275), 1.4, c("Qm= 0.1274,df=1,p=0.7211"),cex=1.2,font=1,col="red")

#### mmr1, mmr1_1 均为多水平混合模型，运算所需时间较长，建议课下练习

### mmr1<-rma.mv(yi,vi, mods=~group,data=d2, random=~1|reference/id,method="REML")

### mmr1_1<-rma.mv(yi,vi, mods=~group-1,data=d2, random=~1|reference/id,method="REML")


### 对mmr1_1模型的结果作图
### estimates_mmr1_1 <- coef(summary(mmr1_1))[,1]

## Birds 和Mammals各自的的方差
### semmr1_1 <- coef(summary(mmr1_1))[,2]
### variance1_1<-semmr1_1^2 


### labels3 <- c("Birds", "Mammals")
### forest(
###  estimates_mmr1_1,
###  variance1_1,
###  slab = labels3,ylim=c(0.8, 4.5),
###  font = 2,
###  cex = 1.5,
###  psize = 1,
###  xlab = "Effect size (log response ratio)"
### )
## text(c(-0.94,-0.00), 2.7, c("Group", "Mean [95% CI]"),cex=1.5,font=2)
### text(c(-0.4), 1.4, c("Qm= 0.8088,df=1,p=0.3685"),cex=1.2,font=1,col="red")


##解释因子为连续变量时
##比如，欲分析在鸟类中，人口密度对效应值的影响
r6<-rma(yi,vi, mods=~human_dens,data=d2, method="REML")
summary(r6)
##结果表明effect size=-0.2536-0.001*human_dens
##Qm=15.4975,P<0.0001
##这说明人口密度度效应值有显著影响，
##人口密度越高，效应值越低，即打猎对动物种群的负面影响越大

##解释变量为数值变量时的作图（回归图）
##模型对人口密度不同时，效应值的估计值

preds <- predict(r6, newmods=c(0:700))
wi<- 1/sqrt(d2$vi)
size  <-0.8 + 3.0 * (wi - min(wi))/(max(wi) - min(wi))
plot(d2$human_dens, d2$yi, pch=19, cex=size, col="grey",ylab="",
     xlab="Human population density",
     las=1, bty="l", font=2,cex.lab=1.5)
lines(0:700, preds$pred)
lines(0:700, preds$ci.lb, lty="dashed")
lines(0:700, preds$ci.ub, lty="dashed")
abline(h=0, lty="dotted",col="red",lwd=2)
title(ylab="Effect size (log resposne ratio)", line=2, cex.lab=1.5)
text(c(500), 4.5, c("Qm=15.4975,P<0.0001"),cex=1.2,font=1,col="red")


############# 数值变量和效应值的关系为非直线关系时 #######

fd <- read_excel("Benitez-Lopez_et_al._2017_Science.xlsx",na = "NA")
fd2<-subset(fd,group=="Birds")
fd2$logdist<-log(fd2$distance_km)
nlm1<-rma.mv(rr,samplingvar,mods=~logdist, random = list(~1|species,~1|study),method="REML",data=fd2)
summary(nlm1)

## 对结果作图代码
summary(fd2$logdist)
a<-seq(from = -4, to = 4.07, by = 0.01)

##对需要估计的x轴的确定范围也可采用
## logd<-fd2$logdist
## logd <- na.omit(logd)
## a<-seq(min(logd), max(logd), length.out=10000)

preds <- predict(nlm1, newmods=a)
x=exp(a)
wi<- 1/sqrt(fd2$samplingvar)
wi <- na.omit(wi)
size  <-1.5+ 5.0 * (wi - min(wi))/(max(wi) - min(wi))

plot(fd2$distance_km, fd2$rr,type="n")
plot(fd2$distance_km, fd2$rr, axes=TRUE, xlab="Distance to hunter's access points (km)",ylab="Effect size (logRR)", cex.lab=1.2, type="n")
cols <- c("pink")
polygon(c(rev(x), x), c(rev(preds$ci.lb), preds$ci.ub), col =alpha(cols, 0.5), border = NA)
lines(x, preds$pred,col="red")
abline(h=0, lty="dotted",col="grey",lwd=3)
points(fd2$distance_km, fd2$rr, cex=size, col="blue")


##多重比较，一个因素的水平多于2个，不同水平之间的比较：
## 示例数据中，guild有多个水平：Carn,Frug,Herb, Inver, Omn5个水平
table(d2$guild)

r7<-rma(yi,vi, mods=~guild,data=d2, method="REML")
summary(r7)

##结果显示QM(df = 4) = 50.8778, p-val < .0001，
##即不同guild之间差异显著，但这里面guild有五个水平：Carn,Frug,Herb, Inver, Omn
##究竟是哪些水平之间差异显著？采用多重比较分析

##作图，直观查看不同水平的差异
r8<-rma(yi,vi, mods=~guild-1,data=d2, method="REML")
summary(r8)
coef(summary(r8))
estimates4 <- coef(summary(r8))[,1]
se4 <- coef(summary(r8))[,2]
variances4<- se4^2
labels4 <- c("Carn", "Frug", "Herb", "Inver", "Omn")
forest(estimates4, variances4, slab=labels4, font=2,cex=1.5,psize=1,
       xlim=c(-0.8,0.75),
       xlab="Effect size (log response ratio)")
text(c(-0.7,0.48), 6.3, c("Group", "Mean [95% CI]"),cex=1.5,font=2)
text(c(-0.1), 6.3, c("Qm=50.87,df=4,P<0.0001"),cex=1.3,font=1,col="red")

##查看结果可知，其中Carn被作为基准值(即结果中的截距)，其他水平分别与其比较。
##即结果中，第一行对应的为对Carn的估计值, 第2-5行分别为：Frug, Herb, Inver Omn与Carn对比后的估计值.
##比如，Frug的值为：-0.0974(截距估计值)+(-0.2232)(即结果中第2行估计值)=-0.3206
##同样，Omn的值为-0.0974(截距估计值)+0.0984(即结果中第5行估计值)=0.001
##如果想比较Frug与Carn这两个水平的差异是否显著，那么：
anova(r8, L=c(1,-1,0,0,0))
##如果想比较Herb与Omn这两个水平的差异是否显著，那么：
anova(r8, L=c(0,0,1,0,-1))
##如果想比较Inver与Omn这两个水平的差异是否显著，那么：
anova(r8, L=c(0,0,0,1,-1))

#####多重比较的另一种方法
library(multcomp)
summary(glht(r8,
             linfct = rbind(
               c(1,-1,0,0,0),
               c(0,0,1,0,-1),
               c(0,0,0,1,-1)),
             df = df.residual(r8)),
        test = adjusted("holm"))



###多因素作用，单独求某一因素的影响：
View(d2)

r9<-rma(yi,vi, mods=~guild+group,data=d2, method="REML")
summary(r9)

##查看结果可知，该模型中，guild=Carns,且group="Birds"时被作为基准值(即截距)，
##其他水平都与其比较
##那么如果想求guild独自对效应值的影响，那么：
anova(r9, btt=2:5)
##如果想求group独自对效应值的影响，那么：
anova(r9, btt=6)

##交互作用
r10<-rma(yi,vi, mods=~guild+human_dens+guild:human_dens,data=d2, method="REML")
summary(r10)


anova(r10, btt=2:4)
##guild单独对效应值的影响
anova(r10, btt=2:5)
##human_dens单独对效应值的影响
anova(r10, btt=6)
##guild与human_dens的交互单独作用对效应值的影响
anova(r10, btt=7:10)




#Meta分析第四步，模型诊断，判定结果的可靠性. 注意，目前，除漏斗图外，模型诊断功能值只能用于rma命令，对于rma.mv命令，尚不适用
##比如要对模型r11和r22进行模型诊断，
r11<-rma(yi,vi, data=subset(d2,group=="Birds"), method="REML")
r22<-rma(yi,vi, data=subset(d2,group=="Mammals"), method="REML")
##发表偏爱性诊断--漏斗图
funnel(r11,main="For birds")
funnel(r22,main="For mammals")

####不同类型的漏斗图
funnel(r11, main="Standard Error")
funnel(r11, yaxis="vi", main="Sampling Variance")
funnel(r11, yaxis="seinv", main="Inverse Standard Error")
funnel(r11, yaxis="vinv", main="Inverse Sampling Variance")


##漏斗图类型2
funnel(r11,yaxis="seinv",level=c(90, 95, 99),ylab="Precision (1/SE)" ,shade=c("white", "gray", "darkgray"), refline=0)
funnel(r22,yaxis="seinv",level=c(90, 95, 99),ylab="Precision (1/SE)" ,shade=c("white", "gray", "darkgray"), refline=0)
##对漏斗图的对称性进行检验(Egger's regression test for funnel plot asymmetry)
##P>0.05则表明漏斗图对称，受发表偏爱性的影响较小，或没有发表偏爱性

regtest(r11, model="rma") ##Egger's regression test for funnel plot asymmetry,for the meta-analytic models
regtest(r22, model="rma")


###Trim and fill method
###参考文献：Duval, S. J., & Tweedie, R. L. (2000a). 
###Trim and fill: A simple funnel-plot-based method 
###of testing and adjusting for publication bias 
###in meta-analysis. Biometrics, 56(2), 455–463. 
###注意此方法在实际应用中并不多见

summary(r11)
t<-trimfill(r11)
t
funnel(t)


##qq正态图
qqnorm(r11)


##雷达图
radial(r11)
radial(r22)

##快捷式，多图合一命令
plot(r11)



##失安全系数计算
##失安全系数用于检验发表偏爱性，与所选模型无关，只有yi和vi有关
##Rosenthal fail safe number（临界值为5k+10,其中k为纳入分析的研究的个数，大于5k+10表明无发表偏爱）
fsn(yi,vi, data=d2) 



##查看帮助文件，其他失安全系数的用法
?fsn


##判断单个研究对总体结果的影响，累计加入法，逐次加入一个案例
rcu <- cumul(r11)
forest(rcu)


##判断单个研究对总体结果的影响，去除法，每次去除一个案例

inf<-influence(r11)
plot(inf, plotdfb=T)







###Meta分析中的其他问题
## 其他问题1，SD替代
##数据应包含计算效应值和研究内方差所需的变量
##如果原数据中处理和对照的SD都有缺失值，那么可以对其SD值进行替估计
fd0 <- read.csv("Benitez-Lopez_Hunting_Science.csv")
View(fd0)
##然后用impute_SD命令对缺失值进行估计
library(metagear)
##采用Bracken1992
##分别输入处理和对照的均值和标准差对应的列名，并选择估算方法
##Bracken1992法
fd1<-impute_SD(fd0, columnSDnames=c("SD_hunted","SD_control"), columnXnames=c("Abund_hunted", "Abund_control"), method = "Bracken1992")
View(fd1)
##查看帮助文件，各命令的意义
??impute_SD




##其他问题2，采用重取样法(bootstrap)求累计效应值的置信区间
## 以鸟类数据为例
##加载boot软件包
library(boot)
##bootstrap方法1
##设置boot命令，下面一段不用做任何改动
boot.func1 <- function(dat, indices) {
  res <- rma(yi, vi, data=dat, subset=indices, method="REML")
  c(res$beta, res$se^2)}

##调用boot.func1命令进行重取样运算
BS_CI_birds1<- boot(dat=subset(d2,group=="Birds"), boot.func1, R=500)
##查看重取样置信区间，
##注意：结果中给出了5种置信区间，分别为Normal，Basic,Studentized，Percentile与BCa
##选用Percentile置信区间即可
boot.ci(BS_CI_birds1)

##bootstrap方法2
##设置boot命令，下面一段不用做任何改动
boot.func2 <- function(dat, indices) {
  
  res <- try(rma(yi, vi, data=dat, subset=indices), silent=TRUE)
  
  if (is.element("try-error", class(res))) {
    NA
  } else {
    c(coef(res), vcov(res), res$tau2, res$se.tau2^2)
  }
}

##调用boot.func2命令进行重取样运算
BS_CI_birds2<- boot(dat=subset(d2,group=="Birds"), boot.func2, R=500)

##查看重取样置信区间，
##注意：结果中给出了5种置信区间，分别为Normal，Basic,Studentized，Percentile与BCa
##选用Percentile置信区间即可
##该方法还可对tau^2的均值和置信区间进行重取样估计，设置index=3:4即可
boot.ci(BS_CI_birds2,index=1:2)

###其他问题3，模型选择：从众多模型中选出最优模型
##rma命令下的模型选择
##注意：下面一句命令不用做任何修改
rma.glmulti <- function(formula, data, ...) { rma(formula, vi, data=data, method="ML", ...)} ##此句命令不用做任何修改
##设置全模型
modelselection<- glmulti(yi ~ body_mass+distance+human_dens+biome, 
                         data=subset(d2,group=="Birds"), level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=100)
##对所有模型的AICc值排序作图
plot(modelselection)

##各个模型的权重
modelweights<- weightable(modelselection)
modelweights
##查看AICc值最低的模型结果
summary(modelselection@objects[[1]])

##输出最优模型，有时候最优模型不止一个，可以有多个模型与AICc值与AICc值最低的模型的AICc差值<2
bestmodelweights<- modelweights[modelweights$aicc <= min(modelweights$aicc) + 2,]
##查最优模型中各模型的权重
bestmodelweights
##作图，查看各个影响因素的重要性
plot(modelselection, type="s")

##计算各个影响因素在模型加权后的参数估计值及其重要性（以importance>0.8为界限）
##设置命令，提取各模型的参数估计值，下一段代码不需任何改动
setOldClass("rma.uni")
setMethod('getfit', 'rma.uni', function(object, ...) {
  if (object$test=="z") {
    cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=Inf)
  } else {
    cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=object$k-object$p)
  }
})

##查看各因素在模型加权后的参数估计值及其重要性（以importance>0.8为界限）
round(coef(modelselection), 4)







##其他问题4，复杂meta分析模型--分层（嵌套）模型分析
###hierarchical meta-analysis
##分层（嵌套）模型，来自同一研究的不同案例研究结果可能彼此相关，本例中，即id嵌套在reference之下
##分层模型要调用rma.mv命令，其参数估计有两种方法，最大似然法(ML)和限制性最大似然法(REML)
##注意这里首先要将id转换为因子
d2$id<-as.factor(d2$id)
##以下2种写法意义相同

mr1<-rma.mv(yi,vi, mods=~biome,data=subset(d2,group=="Birds"), random=~1|reference/id,method="ML")
mr2<-rma.mv(yi,vi, mods=~biome,data=subset(d2,group=="Birds"), random=list(~1|reference/id),method="ML")

summary(mr1)
summary(mr2)


##也可以同时加入多个随机因子
mr7<-rma.mv(yi,vi, mods=~human_dens,data=subset(d2,group=="Birds"), random = list(~1|id,~1|species,~1|reference),method="ML")
summary(mr7)

##对比是否考虑分层时模型结果的变化
##因分层模型运算速度较慢，所以只对鸟类数据进行练习
mm1<-rma(yi,vi, mods=~biome,data=subset(d2,group=="Birds"), method="ML")
hmm1<-rma.mv(yi,vi, mods=~biome,data=subset(d2,group=="Birds"), random=~1|reference/id,method="ML")
summary(mm1)
summary(hmm1)


##分层模型与一般模型对不同类群效应值估计值的对比
mm2<-rma(yi,vi, mods=~biome-1,data=subset(d2,group=="Birds"), method="ML")
hmm2<-rma.mv(yi,vi, mods=~biome-1,data=subset(d2,group=="Birds"), random=~1|reference/id,method="ML")
summary(mm2)
summary(hmm2)

##rma.mv命令下的模型选择，分层（嵌套）模型选择
##分层（嵌套）模型选择过程
##首先定义模型选择功能rma.glmulti，注意下一段代码不需做任何修改
rma.glmulti <- function(formula, data, V, random,...) {
  do.call("rma.mv", list(as.formula(paste(deparse(formula))), V = as.name(V), random = as.name(random), data = data,  method = "ML", ...))
}


d2$id<-as.factor(d2$id)
##定义分层类型
random_effect <- list(~id|reference,~1|species)
##构造full model，包括所有影响因素
modelselection2 <- glmulti(yi ~ guild+biome+body_mass+distance, V = "vi", random="random_effect", data=subset(d2,group=="Birds"), level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize =32)

##对各个模型的AIC值进行作图
plot(modelselection2)
##查看最优模型与最差模型
print(modelselection2)
##将模型按AICc值排序
rank.models <- weightable(modelselection2)
##查看不同模型的权重
rank.models
##查看最优模型(与最低AICc值差距在2以内的模型)
rank.models <- rank.models[rank.models$aicc <= min(rank.models$aicc) + 2, ]
rank.models
##对不同因素的重要性作图
plot(modelselection2,type="s")

##查看加入模型权重的各因素参数估计
##设置命令，提取各模型的参数估计值，下面一段不需要任何修改
setOldClass("rma.mv")
setMethod('getfit', 'rma.mv', function(object, ...) {
  if (object$test=="z") {
    cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=Inf)
  } else {
    cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=object$k-object$p)
  }
})

##调用命令，查看加入模型权重的各因素参数估计
round(coef(modelselection2), 4)

##rma.mv模型下，漏斗图，但目前无法用regtest,或ranktest对该模型下的漏斗图的对称性进行定量检验
mr3<-rma.mv(yi,vi, mods=~human_dens,data=subset(d2,group=="Birds"), random=~1|reference/id,method="ML")
funnel(mr3,yaxis="seinv",level=c(90, 95, 99),ylab="Precision (1/SE)" ,shade=c("white", "gray", "darkgray"), refline=0)


################################ 方差协方差矩阵 ##############
general_data<-read.csv("meta-analysis_Midolo_et_al.csv")
dat<-general_data[general_data$trait=="SLA",] # select the trait you want, e.g. ("SLA")
####对于来自同一海拔样带的不同数据，计算一个方差协方差矩阵
calc.v <- function(x) {
  v <- matrix((x$sd_control[1]^2 / (x$n_individuals[1] * x$control[1]^2)) , nrow=nrow(x), ncol=nrow(x))
  diag(v) <- x$vi
  v
} 
library(metafor)
V <- bldiag(lapply(split(dat, dat$common_id), calc.v))
###用V替代vi进行meta分析运算
m0<-rma.mv(yi, V, data=dat, random =list(~ 1 | gradient_id/id,~1|species)) 



############# 系统发育（phhylogenetic）meta分析案例 ################
##### 本案例数据和主要代码均来自：Bennett, J. M. et al. Land use and pollinator dependency drives global patterns of pollen limitation in the Anthropocene. 
##### Nature Communications 11, 3999, doi:10.1038/s41467-020-17751-y (2020).
##### 出于演示目的，其代码由张霜稍加修改


library(ape)
library(multcomp)
library(readr)


# Cleans environment
###rm(list = ls(all.names = TRUE))

dt <- read_csv("dt.csv")
tree<-read.tree("tree.tre")
library(phytools)
plotTree(tree, main="phylo-tree", node.numbers = TRUE, fsize = 0.2, type="fan")


vcov <- ape::vcv(tree)

sub_pd <- dt[which(dt$pd == "PD"), ]
# Table only for vegetation land-use
sub_pd_no_urban <- subset(sub_pd, ! luh %in% 'LUH.urban')
sub_pd_no_urban <- droplevels(sub_pd_no_urban)
###rm(sub_pd)


# Model for the results reported in Table S7
dt$id <- 1:nrow(dt)
LUH.pd.lnr.2 <- rma.mv(es ~ pd + luh + pd * luh,
                       V = var_es,
                       random = list(~ 1|factors,
                                     ~ 1|species,
                                     ~ 1|id),
                       R = list(species = vcov),
                       data = dt,
                       method = "ML")
# Values reported in Table S7
summary(LUH.pd.lnr.2)


##森林图的优化
##固定效应模型与随机效应模型对对案例2，计数数数案例
## 固定效应模型
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
fixed11<- rma(yi, vi, method="FE", data=dat)
forest(fixed11, main="Fixed effect model")
random11<- rma(yi, vi, data=dat)
## 固定效应模型
forest(random11, main="Random effect model")

forest(random11, xlim=c(-18, 6), slab = paste(dat$author, dat$year),
       ilab=cbind(dat$tpos, dat$tneg, dat$cpos, dat$cneg),
       ilab.xpos=c(-10,-8,-6,-4), cex=1.2, ylim=c(-2, 16),
       xlab="effect size (ln odds ratio)", mlab="",font=2, psiz=1)
text(-18, -0.7, pos=4, cex=1.2,font=3, bquote(paste("Heterogeneity test: Qt = ",
                                                    .(formatC(random11$QE, digits=2, format="f")), ", df = ", .(random11$k - random11$p),
                                                    ", P = ", .(formatC(random11$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(random11$I2, digits=1, format="f")), "%")))
text(-18, -1.7, pos=4, cex=1.2,font=3, 
     bquote(paste("Test of overall effect: Z = ",.(formatC(random11$zval, digits=4, format="f")), 
                  ", P = ", .(formatC(random11$pval, digits=4, format="f")))))

text(c(-10,-8,-6,-4), 15, c("+", "-", "+", "-"),cex=1.2,font=2)
text(c(-9,-5), 15.5, c("Yes", "No"),cex=1.2,font=2)
text(-18, 15, "Study ID", cex=1.2,font=2, pos=4)
text(6, 15, "Mean [95% CI]", cex=1.2,font=2,pos=2)

##设置annotate=FALSE可使forest图中效应值的值和置信区间数字消失
##更多森林图命令参考网址
##https://www.rdocumentation.org/packages/metafor/versions/1.9-9/topics/forest.default

##查看研究间方差tau^2的似然值（此命令实际在文献中报道较少见，参见science案例文章附件中的图）
profile(r2, xlim=c(0,5), steps=50)

##练习代码结束