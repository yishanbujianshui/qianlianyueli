
#结构方程模型基础--模拟案例#
library(lavaan)
library(semPlot)
library(tidyverse)

#-------------1.基础演示，对比多元回归---------
#读入数据。这里模拟了一组数据。
xm.data<-tibble(diversity=rnorm(100,0.8,0.3),fert=rnorm(100,5,2),temp=rnorm(100,20,5),micro=rnorm(100,0.3,0.2),biomass=fert*0.6+rnorm(100,0,1))
ggplot(data=xm.data,aes(fert,biomass))+geom_point()

#指定模型
model.sem<-"
biomass~diversity+fert+temp+micro
micro~fert+temp
"
#拟合模型
fit.sem<-sem(model.sem,data=xm.data)
fit.sem1<-sem(model=
"biomass~diversity+fert+temp+micro
micro~fert+temp",data=xm.data
)
fit.sem2<-sem(model="biomass~diversity+fert+temp+micro; micro~fert+temp",data=xm.data)
#提取结果
summary(fit.sem)
summary(fit.sem,fit.measures=T,standardized=T,modindices=T,rsq=T)
summary(fit.sem1,fit.measures=T,standardized=T,modindices=T,rsq=T)
summary(fit.sem2,fit.measures=T,standardized=T,modindices=T,rsq=T)

#自动画出结果草图
semPaths(fit.sem)
semPaths(fit.sem,"std")


#对比多元回归
fit.lm<-lm(biomass~diversity+fert+temp+micro,data=xm.data)
summary(fit.lm)

fit.lm1<-lm(micro~fert+temp,data=xm.data)
summary(fit.lm1)

#拟合只有一个结构方程的SEM，继续与多元回归对比。拟合参数完全一致
fit.sem3<-sem(model="biomass~diversity+fert+temp+micro", data=xm.data)
summary(fit.sem3,fit.measures=T,standardized=T,modindices=T,rsq=T)
summary(fit.lm)

#所以，多元回归只是SEM的一个特例，是假定了数据由这一个结构方程完全反映了。
#这时SEM是饱和模型,无法检验拟合度，也无须检验。

#---------------2. 设置自动计算和检验间接效应-------------
#-----预热，查看，说明我们可以指定模型的参数名称。
model.sem1<-"
biomass~a*diversity+b*fert+c*temp+d*micro
micro~e*fert+f*temp
"
fit.sem4<-sem(model.sem1,data=xm.data)
summary(fit.sem4)
summary(fit.sem)#与fit.sem4对比

#-----真正开始设置随机效应
model.sem2<-"
biomass~a*diversity+b*fert+c*temp+d*micro
micro~e*fert+f*temp
#指定施肥通过改变微生物影响生物量的间接效应
indirect:=e*d
"
fit.sem5<-sem(model.sem2,data=xm.data)
summary(fit.sem5)
summary(fit.sem5,fit.measures=T,standardized=T,modindices=T,rsq=T)

#简单开展潜变量分析：验证性因子分析cfa（拟合测量模型）
data.xm2<-data.frame(N=rnorm(200,3,1),P=rnorm(200,10,1),K=rnorm(200,30,20),Ca=rnorm(200,10,5),Mg=rnorm(200,60,5))
model.cfa<-"Soil=~N+P+K+Ca+Mg"
fit.cfa<-cfa(model.cfa,data=data.xm2)
summary(fit.cfa,fit.measures=T,standardized=T,rsq=T,modindices=T)





