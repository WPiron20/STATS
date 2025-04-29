## STAT 3300 Homework 8 R Code Solutions

## Question 1 ############################################################
#part c
b1=1.4 #enter coefficient estimate
SE_b1=0.65 #enter standard error
ts=b1/SE_b1 #calculate test statistic
n=20 #enter sample size
2*pt(ts,n-2,lower=F) #calculate p-value
#part e
t=qt(0.975,n-2) #calculate t critical value
b1-t*SE_b1 #calculate lower bound
b1+t*SE_b1 #calculate upper bound

## Question 2 ############################################################
#part c
b1=2.1 #enter coefficient estimate
SE_b1=1.05 #enter standard error
ts=b1/SE_b1 #calculate test statistic
n=30 #enter sample size
2*pt(ts,n-2,lower=F) #calculate p-value
#part e
t=qt(0.975,n-2) #calculate t critical value
b1-t*SE_b1 #calculate lower bound
b1+t*SE_b1 #calculate upper bound

## Question 3 ############################################################
Storms=read.csv(file.choose()) #read in data frame
head(Storms) #view first 6 rows of data
SLR=lm(Tornadoes~Year,data=Storms) #run the regression
PIs=predict(SLR,interval='predict') #calculate prediction intervals
Storms_PIs=cbind(Storms,PIs) #bind PIs to original data
library(ggplot2)
ggplot(Storms_PIs,aes(x=Year,y=Tornadoes))+ #call the data
  geom_point()+ #plot x and y values as a scatterplot
  geom_smooth(method='lm')+ #add fit line with CIs for mean
  geom_line(aes(y=lwr),col='red',lty='dashed')+ #add lower PI
  geom_line(aes(y=upr),col='red',lty='dashed')+ #add upper PI
  ggtitle('Tornadoes vs Year') #add graph title

## Question 4 ############################################################
ggplot(Storms,aes(x=Year,y=SLR$residuals))+ #call the data
  geom_point()+geom_hline(yintercept=0)+ #plot values and add horizontal line
  ggtitle('Residual Plot') #add graph title

## Question 6 ############################################################
summary(SLR) #view regression results

## Question 9 ############################################################
confint(SLR) #calculate confidence intervals for betas

## Question 11 ############################################################
newdata=data.frame(Year=2000) #define new data
predict(SLR,interval='confidence',newdata) #calculate CI for mean response 

## Question 12 ############################################################
newdata=data.frame(Year=2015) #define new data
predict(SLR,interval='predict',newdata) #calculate PI for individual response 

## Question 14 ############################################################
Tuition=read.csv(file.choose()) #read in data frame
head(Tuition) #view first 6 rows
SLR=lm(Y2014~Y2008,Tuition) #run the regression
summary(SLR) #view regression results

## Question 15 ############################################################
Tuition2=subset(Tuition,CalifSchool==0) #keep only non-California schools
SLR2=lm(Y2014~Y2008,Tuition2) #re-run the regression
summary(SLR2) #view new regression results
SLR2 #view coefficients without scientific notation

## Question 16 ############################################################
newdata=data.frame(University=c("Skinflint","I.O.U."),Y2008=c(8800,15700)) #define new data
newdata #view new data to confirm
predict(SLR2,interval='predict',newdata) #calculate PIs for individual responses 