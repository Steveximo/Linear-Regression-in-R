
 # THESIS PROJECT:ANALYSIS OF MAJOR RISK FACTORS ATTRIBUTED TO BLOOD PRESSURE
  
  
  
  #.. Loading and importing data from Excel into R


Data<-read.csv("C:/Users/SSEKIRIWO ANDREW/Desktop/Diabetes/Diabetes data.csv")


  #... An overview of the data

names(Data)
nrow(Data)
ncol(Data)
head(Data,10)
tail(Data,10)
str(Data)
summary(Data)

 
  #...Manipulations,transformations and insights in the data
  #.. Generating new variable, BMI calculating its mean

library(magrittr)
library(dplyr)
Data%>%
  mutate(BMI=weight/height^2)%>%
  summarise(avgBMI=mean(as.integer(BMI),na.rm=T))



 #... Getting mean of BMI by gender 

Data%>%
  mutate(BMI=weight/height^2)%>%
  summarise(avgBMIf=mean(as.integer(BMI[gender=="f"]),na.rm=T))

Data%>%
  mutate(BMI=weight/height^2)%>%
  summarise(avgBMIm=mean(as.integer(BMI[gender=="m"]),na.rm=T))




  #...Getting average age by on gender

Data%>%
  summarise(avgmean=mean(as.integer(age[gender=="f"]),na.rm=T))
Data%>%
  summarise(avgmean=mean(as.integer(age[gender=="m"]),na.rm=T))


  #...Getting summary measures of all variables based on smoking status

smoking_status<-factor(Data$smoking_sta,levels=c(1,2),labels=c("yes","no"))

tapply(Data$age,INDEX=smoking_status,FUN=mean,na.rm=T)

tapply(Data$height,INDEX=smoking_status,FUN=mean,na.rm=T)

tapply(Data$weight,INDEX=smoking_status,FUN=mean,na.rm=T)

tapply(Data$sbp,INDEX=smoking_status,FUN=mean,na.rm=T)

tapply(Data$age,smoking_status,summary)


 #... Factoring family status variable

Familystatus_hist<-factor(Data$family_his,levels=c(1,2),labels=c("yes","no"))

tapply(Data$age,Familystatus_hist,summary)


  #...Getting summary measures of variables based on gender

tapply(Data$age,INDEX=list(smoking_status,Data$gender),FUN=mean,na.rm=T)

tapply(Data$age,INDEX=list(Familystatus_hist,Data$gender),FUN=mean,na.rm=T
)
tapply(Data$height,INDEX=list(smoking_status,Data$gender),FUN=mean,na.rm=T)

tapply(Data$height,INDEX=list(Familystatus_hist,Data$gender),FUN=mean,na.rm=T)

tapply(Data$weight,INDEX=list(smoking_status,Data$gender),FUN=mean,na.rm=T)

tapply(Data$age,INDEX=list(Familystatus_hist,Data$gender),FUN=mean,na.rm=T)

tapply(Data$sbp,INDEX=list(smoking_status,Data$gender),FUN=mean,na.rm=T)

tapply(Data$sbp,INDEX=list(Familystatus_hist,Data$gender),FUN=mean,na.rm=T)


  #... Filtering out needed output

filter(Data,age >=65,sbp<=150)
filter(Data,gender=="f",smoking_status=="yes")

filter(Data,gender=="m",Familystatus_hist=="no")

filter(Data,is.na(age))


  #... Exploring relationships between variable through scatter plot visualizations

library(ggplot2)
ggplot(data=Data,mapping=aes(x=age,y=sbp,linetype=gender))+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(data=Data,mapping=aes(x=weight,y=sbp))+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(data=Data,mapping=aes(x=Data1$BMI,y=sbp))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~gender)+
  geom_smooth(se=FALSE)

ggplot(data=Data1,mapping=aes(x=Data$age,y=Data1$BMI))+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(data=Data1,mapping=aes(x=Data1$BMI,y=weight))+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(data=Data1,mapping=aes(x=height,y=Data1$BMI))+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(data=Data1,mapping=aes(x=Data1$BMI,y=Data$sbp))+
  geom_point()+
  geom_smooth(se=FALSE)


  #... Bar chart visualizations based on gender and smoking status

ggplot(data=Data)+
  geom_bar(mapping=aes(x=gender,fill=gender))

ggplot(data=Data)+
  geom_bar(mapping=aes(x=gender,fill=Familystatus_hist))


  #...Exploring the relationship between gender and smoking status
  # H0:There is no association between gender and smoking status

table1<-xtabs(~smoking_status+gender,data=Data)
barplot(as.matrix(table1),main="Gender by Smoking Status",col=c(2,4),
        legend.text=c("non smoker","smoker"),beside=TRUE)

TAB<-table(Data1$gender,smoking_status)

chisq.test(table(TAB),correct=T)


  #... Exploring relationship between gender and SBP
  # H0 There is no association between gender and SBP

boxplot(Data$sbp,main="Box Plot for Systolic Blood Pressure",ylab="sbp")

boxplot(Data$sbp~Data$gender,main="Box Plot by gender")

Males=subset(Data,gender=="m")
Females=subset(Data,gender=="f")

mean(Males$sbp,na.rm=T)
mean(Females$sbp,na.rm=T)

t.test(Males$sbp,Females$sbp,alternative="two.sided",conf.level=0.95)


  #... Exploring relationship between Smoking status and SBP
  #H0:There is no association between SBP and smoking status

mean(smokers$sbp,na.rm=T)
mean(nonsmokers$sbp,na.rm=T)


var(Data$sbp[smoking_status=="yes"])
var(Data$sbp[smoking_status=="no"])

boxplot(Data$sbp~smoking_status,ylab="sbp",main="Blood pressure Vs Smoking Status")

t.test(smokers$sbp,nonsmokers$sbp,alternative="two.sided",conf.level=0.95)


  #...Exploring relationship between Family history status and SBP
 # Ho: There is no association between SBP and family history of disease

fam_yes=subset(Data,Familystatus_hist=="yes")
fam_no=subset(Data,Familystatus_hist=="no")

mean(fam_yes$sbp,na.rm=T)
mean(fam_no$sbp,na.rm=T)

t.test(fam_yes$sbp,fam_no$sbp,alternative="two.sided",conf.level=0.95)


 #... Exploring relationship between gender and Family history status
 # Ho:There is no association between gender and family history of disease

table2<-xtabs(~smoking_status+Familystatus_hist,data=Data)

barplot(as.matrix(table2),main="Family History by Smoking Status",col=c(5,12),
        legend.text=c("non smoker","smoker"),beside=TRUE)

TAB2<-table(Data1$gender,Familystatus_hist)

chisq.test(table(TAB2),correct=T)


 #... Visualizing BMI by Smoking status

plot(Data$sbp[smoking_status=="no"],Data1$BMI[smoking_status=="no"],
     main="plot",col=4,xlab ="BMI",ylab="sbp")
points(Data$sbp[smoking_status=="yes"],Data1$BMI[smoking_status=="yes"],col=2)
legend(x=145,y=35,legend=c("nonsmoker","smoker"),col=c(4,2),pch=c(16,17),bty="n")



  #...Visualizing age and smoking status according to SBP stratified groups

boxplot(Data$age~smoking_status*SBPgroups,ylab="age",main="Age vs Smoking by Blood pressure groups",las=2,col=c("blue","red"),xlab="SBP Strata",pch=15,cex=0.8)



  #... Exploring distribution of SBP using histogram

hist(Data$sbp,freq=F,main="Histogram for Blood pressure",xlab="sbp")
lines(density(Data$sbp),col=2,lwd=3)


 #...Generating Correlations between continous variables

r<-c(Data$age,Data$sbp,Data1$BMI)
mat<-matrix(r,ncol = 3,nrow = 93)
result<-rcorr(mat)
result$r[result$n<5]<-0
result$r



  #...Fitting  simple regression models

plot(Data$age,Data$sbp,main="scatter plot",col=5)
text(x=35, y=175,adj=0,label="r=0.4",cex=2,font=4,)
abline(lm(Data$sbp~ Data$age),col=2)

model1<-lm(Data$sbp~Data$age)
model1
summary(model1)
model1$coefficients

model2<-lm(Data1$BMI~Data$age)
model2
model2$coefficients
summary(model2)



  #...Fitting a multiple regression model

MODEL1<-lm(Data$sbp~Data$age+Data1$BMI)
summary(MODEL1)


MODEL2<-lm(Data$sbp~Data$age+Data1$BMI+Data$gender+Data$smoking_sta+Familystatus_hist)
summary(MODEL2)
