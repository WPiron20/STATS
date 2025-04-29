## STAT 3300 Homework 9 R Code Solutions

## Question 2 ##################################################################
College=read.csv(file.choose()) #read in data frame
head(College) #view first 6 rows of data
College_sub=College[,c(3,4,6,8,9)] #create smaller data frame with only variables of interest
head(College_sub) #view first 6 rows to confirm
plot(College_sub,pch=16) #plot only the variables of interest

## Question 3 ##################################################################
round(cor(College_sub),2) #calculate pairwise correlations for the variables of interest

## Question 5 ##################################################################
MLR=lm(AvgDebt~.,College_sub) #fit the full model with all predictors of interest
summary(MLR) #view regression results

## Question 6 ##################################################################
plot(MLR,pch=16) #create residual plot and Q-Q plot

## Questions 8, 10, and 11 ##################################################################
#remove Baruch College
NewCollege=subset(College,Name!="Baruch College of the City University of New York")
nrow(NewCollege) #confirm reduced sample size after removing outlier
NewMLR=lm(AvgDebt~Admit+GradRate+InCostAid+OutCostAid,NewCollege) #re-run regression using new data
summary(NewMLR) #view regression results

## Question 12 ##################################################################
NewMLR2=lm(AvgDebt~InCostAid,NewCollege) #re-run regression with only InCostAid using new data
anova(NewMLR2,NewMLR) #run partial ANOVA F test to cross validate

## Question 13 ##################################################################
Storms=read.csv(file.choose()) #read in data frame
head(Storms) #view first 6 rows of data
plot(Storms,pch=16) #plot each pair of variables

## Question 14 ##################################################################
round(cor(Storms),3) #calculate correlations for each pair of variables

## Question 15 ##################################################################
MLR=lm(Tornadoes~Year+Census,Storms) #run regression
summary(MLR) #view results

## Question 18 ##################################################################
Crops=read.csv(file.choose()) #read in data frame
head(Crops) #view first 6 rows
SLR=lm(Yield~Temp,Crops) #fit simple linear regression
summary(SLR) #view linear results
QLR=lm(Yield~poly(Temp,2),Crops) #fit quadratic regression
summary(QLR) #view quadratic results
CLR=lm(Yield~poly(Temp,3),Crops) #fit cubic regression
summary(CLR) #view cubic results