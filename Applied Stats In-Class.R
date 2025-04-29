#Example 3 in the slides
z = qnorm(.95)
phat = 10/24
phat - z*sqrt(phat*(1-phat)/24)
phat + z*sqrt(phat*(1-phat)/24)

#Example 4 in the slides

z = qnorm(.995)
m = .04
pstar = .5
(z/m)^2 * .5*(.5)

z = qnorm(.95)
z

n = 1/2 * (z/.1)^2
n


prop.test(x=40, n=340, alternative = "less")

prop.test(x=c(63,27), n=c(296,251), correct = F)
1-.7984408

x=c(3,16.5,10.5,40.5,5.5,33.5,0,6.5)
xbar=mean(x)
s=sd(x)
t=qt(.975,7)
xbar-t*s/sqrt(8)
xbar+t*s/sqrt(8)

n=4*(.52)^2/.06^2
n

qnorm(.98)


x=c(1519,1441,1570,1380,1209,1712,1410,1263,1199,1480)
mu_0=mean(x)
hist(x)
qqnorm(x)
t.test(x,conf.level = .99)
t.test(x,mu=1300,alt="g")
t.test(x,mu=1500,alt="l")
power.t.test(n=8,delta = 400,sd=500,type = "one.sample", alternative = "one.sided")

z = qnorm(.975)
phat = 55/75
se = sqrt(phat*(1-phat)/75)
z*se
se = .65/sqrt(25)
se


Reading = read.csv(file.choose())
library(ggplot2)
ggplot(Reading,aes(x=Treatment, y=Score))+geom_boxplot()
ggplot(Reading,aes(sample=Score))+geom_qq()+facet_wrap(~Treatment)
t.test(Reading$Score~Reading$Treatment)
s = sd(Reading$Score)

SSHA = read.csv(file.choose())
head(SSHA)
library(ggplot2)
ggplot(SSHA, aes(x=Gender, y=Score))+geom_boxplot()
ggplot(SSHA, aes(sample = Score))+geom_qq()+facet_wrap(~Gender)


mytable = as.table(matrix(c(370,230,220,280), ncol = 2, byrow = TRUE, dimnames = list(Sex = c('F', 'M'), Instagram = c('Uses', 'Does Not'))))
mytable
mydata = data.frame(mytable)
mydata
library(ggplot2)
ggplot(mytable, aes(x = Sex, y=Freq, fill = Instagram))+geom_bar(position = 'stack', stat = 'identity')

Military = read.csv(file.choose(), row.names = 1)
Military
chisq = chisq.test(Military, correct = F)
chisq$expected

Eyes = read.csv(file.choose())
library(tidyverse)
data.summary = Eyes %>% group_by(Group) %>% summarise(Count = n(),Mean = mean(Score),STDEV = sd(Score)) %>% as.data.frame()
data.summary
fit = aov(formula = Score~factor(Group), data = Eyes)
summary(fit)

# Define the matrix
WadeTract = matrix(c(18, 22, 39, 21), nrow = 1,dimnames = list(c("Count"), c("Q1", "Q2", "Q3", "Q4")))
print(WadeTract)


WadeTract_df <- data.frame(
  Quadrant = c("Q1", "Q2", "Q3", "Q4"),
  Count = c(18, 22, 39, 21)
)
library(ggplot2)
ggplot(WadeTract_df, aes(x = Quadrant, y = Count, fill = Quadrant)) + geom_bar(stat = "identity",fill = "green") +
  labs(title = "Tree Distribution in Wade Tract", x = "Quadrants", y = "Number of Trees")

observed_counts <- c(18, 22, 39, 21)
chisq.test(observed_counts,p=rep(1/4,4),correct=F)
pchisq(13.6, 8, lower.tail = FALSE)

# Define the matrix
CollegeHealth = matrix(c(69, 206, 294,25, 126, 170, 14, 111, 169), nrow = 3, byrow = TRUE,dimnames = list(FruitConsumption =c("Low", "Medium", "High"), PhysicalActivity=c("Low", "Moderate", "Vigorous")))
print(CollegeHealth)

results=chisq.test(CollegeHealth,correct=FALSE)
results
results$expected

mydata = data.frame(CollegeHealth)

ggplot(mydata,aes(fill=PhysicalActivity,y=Freq,x=Sex))+geom_bar(position='stack',stat='identity')

# Load required package
library(ggplot2)

# Create the CollegeHealth matrix
CollegeHealth <- matrix(c(69, 206, 294, 25, 126, 170, 14, 111, 169), nrow = 3, byrow = TRUE, dimnames = list(FruitConsumption = c("Low", "Medium", "High"), PhysicalActivity = c("Low", "Moderate", "Vigorous")))
CollegeHealth_df <- as.data.frame(as.table(CollegeHealth))

# Create a stacked bar chart
ggplot(CollegeHealth_df, aes(x = FruitConsumption, y = Freq, fill = PhysicalActivity)) +
  geom_bar(stat = "identity") +
  labs(title = "Fruit Consumption vs. Physical Activity",
       x = "Fruit Consumption Level",
       y = "Count",
       fill = "Physical Activity Level")

ggplot(CollegeHealth_df,aes(fill=PhysicalActivity,y=Freq,x=FruitConsumption))+geom_bar(position='fill',stat='identity')



FbTime = read.csv(file.choose())
head(FbTime)
contrasts(FbTime$Group)=c(.5,.5,0,.5,.5)

WRTR = read.csv(file.choose())
head(WRTR)
library(tidyverse)
data.summary=WRTR %>% group_by(Type) %>%
summarise(Count=n(),Mean=mean(DeathAge),Stdev=sd(DeathAge)) %>%
  as.data.frame()
data.summary
ggplot(WRTR,aes(x=Type,y=DeathAge))+geom_boxplot()
fit=aov(formula=DeathAge~factor(Type),data=WRTR)
summary(fit)

install.packages("DescTools")
library(DescTools)
PostHocTest(aov(WRTR$DeathAge~WRTR$Type),method="bonferroni")

power.anova.test(groups=4,n=10,between.var=5,within.var=20)
power.anova.test(groups=4,between.var=5,within.var=20,power=0.8)

summary.lm(aov(WRTR$DeathAge~WRTR$Type)) 
#19. Poems

WRTR$Type=factor(WRTR$Type)
contrasts(WRTR$Type)=c(1,-1,0) 
summary.lm(aov(WRTR$DeathAge~WRTR$Type)) 

#16. 1/2, reject, .26

Scores = read.csv(file.choose())
head(Scores)
library(tidyverse)
data.summary=Scores %>% group_by(Sex,Major) %>%
  summarise(Count=n(),Mean=mean(HSE),Stdev=sd(HSE)) %>% as.data.frame()
data.summary
data.summary=Scores %>% group_by(Sex,Major) %>%
  summarise(Count=n(),Mean=mean(HSS),Stdev=sd(HSS)) %>% as.data.frame()
data.summary
model=aov(HSS~Sex*Major,data=Scores)
summary(model)

SpeedLanguage = read.csv(file.choose())
head(SpeedLanguage)
data.summary=SpeedLanguage %>% group_by(Age,Lingual) %>%
  summarise(Count=n(),Mean=mean(Time),Stdev=sd(Time)) %>% as.data.frame()
data.summary
ggplot(SpeedLanguage,aes(x=Age,y=Time,fill=Lingual))+geom_boxplot()

ggplot(SpeedLanguage,aes(x=Age,y=Time,group=Lingual,color=Lingual))+stat_summary(fun=mean,geom="point")+stat_summary(fun=mean,geom="line")
model=aov(Time~Age*Lingual,data=SpeedLanguage)
summary(model)


cars = read.csv(file.choose())
head(cars)
library(ggplot2)
ggplot(cars,aes(x=Weight,y=MPG))+geom_point()
cor(cars$Weight,cars$MPG)

SLR = lm(MPG~Weight, data = cars)
summary(SLR)

ggplot(cars,aes(x=Weight,y=SLR$residuals))+
  geom_point()+geom_hline(yintercept=0)+
  ggtitle('Residual Plot')
qqnorm(SLR$residuals,pch=16)
qqline(SLR$residuals)

grades = read.csv(file.choose())
head(grades)
#Create a scatterplot
ggplot(grades,aes(x=Hours,y=Exam))+
  geom_point()+geom_smooth(method=lm,se=F)+
  ggtitle('ScatterPlot')
#SLR Summary
SLR=lm(Exam~Hours,data=grades)
summary(SLR)
#Residual Plot Format
ggplot(grades,aes(x=Exam,y=SLR$residuals))+
  geom_point()+geom_hline(yintercept=0)+
  ggtitle('Residual Plot')
#QQ Plot of Residual Format
qqnorm(SLR$residuals)
qqline(SLR$residuals)
#Correlation Coefficient
cor(grades$Hours,grades$Exam)
confint(SLR)

Home = read.csv(file.choose())
head(Home)
plot(Home[,-1],pch=16)
round(cor(Home[,-1]),2)

Home_Fit=lm(Price~Beds+Baths+SqFt,data=Home)
summary(Home_Fit)

library(ggplot2)

qqnorm(Home_Fit$residuals) #create Q-Q plot of residuals
qqline(Home_Fit$residuals) #add line to Q-Q plot
# create plots of residuals versus each predictor
ggplot(Home,aes(x=Beds,y=Home_Fit$residuals))+geom_point()
ggplot(Home,aes(x=Baths,y=Home_Fit$residuals))+geom_point()
ggplot(Home,aes(x=SqFt,y=Home_Fit$residuals))+geom_point()
# create plot of residuals versus fitted values
ggplot(Home,aes(x=Home_Fit$fitted.values,y=Home_Fit$residuals))+ geom_point()
# create Q-Q plot and residual plot for fitted model
plot(Home_Fit,pch=16)

Hotels = read.csv(file.choose())
head(Hotels)
plot(Hotels[,-1],pch = 16)
MLR2 = lm(Energy~Area+OccRate,data=Hotels)
MLR2
confint(MLR2,level=0.9)

2*pt(2,28,lower.tail = F)

TYear = read.csv(file.choose())
head(TYear)
TYear_Fit = lm(Tornadoes~Year,data = TYear)
summary(TYear_Fit)
model = lm(Tornadoes ~ Year, data = TYear)
means = predict(model, interval = "confidence")
ggplot(TYear,aes(x=TYear_Fit$fitted.values,y=TYear_Fit$residuals))+ geom_point()
plot(TYear_Fit, pch = 16)
confint(TYear_Fit, level = .95)

Tuition = read.csv(file.choose())
Tuition2=subset(Tuition,CalifSchool==0)
SLR2=lm(Y2014~Y2008,Tuition2)
summary(SLR2)

ggplot(data = tornado, aes(x = Year, y = resid(model))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residual Plot for Tornadoes by Year, with a line at 0",
       y = "Residuals")







# Load required packages
library(ggplot2)
library(broom)

# Load the tornado data
tornado <- read.csv(file.choose())

# Fit the regression model
SLR <- lm(Tornadoes ~ Year, data = tornado)

# Add fitted values and residuals to the data using broom
tornado_aug <- augment(SLR)

# Add confidence and prediction intervals
CI_PI <- predict(SLR, interval = "prediction")
CI_mean <- predict(SLR, interval = "confidence")

# Add intervals to augmented dataset
tornado_aug$fit <- CI_PI[, "fit"]
tornado_aug$lwr_pred <- CI_PI[, "lwr"]
tornado_aug$upr_pred <- CI_PI[, "upr"]
tornado_aug$lwr_conf <- CI_mean[, "lwr"]
tornado_aug$upr_conf <- CI_mean[, "upr"]

# Scatterplot with regression line, confidence and prediction intervals
ggplot(tornado_aug, aes(x = Year, y = Tornadoes)) +
  geom_point() +
  geom_line(aes(y = fit), size = 1) +  # regression line

tornado <- read.csv(file.choose())

# Step 2: Fit the regression model
SLR <- lm(Tornadoes ~ Year, data = tornado)

# Step 3: View the regression summary
summary(SLR)


Tuna = read.csv(file.choose())
head(Tuna)
plot(Tuna,pch=16)
cor(Tuna$Age,Tuna$Length)
SLR=lm(Length~Age,data=Tuna)
plot(SLR,pch=16)
QLR=lm(Length~poly(Age,2),Tuna)
summary(QLR)
plot(QLR,pch=16)

Movies = read.csv(file.choose())
Movies_Sub=Movies[,c(1,9,6,7,8,12)]
plot(Movies_Sub[,-1],pch=16)
round(cor(Movies_Sub[,-1]),3)
Movies_Fit=lm(USRevenue~Budget+Opening+Theaters+Ratings,data=Movies)
plot(Movies_Fit,pch=16)
summary(Movies_Fit)

#take the log because of fan shape.

Movies_Fit2=lm(log(USRevenue)~Budget+Opening+Theaters+Ratings,data=Movies)
plot(Movies_Fit2,pch=16)
summary(Movies_Fit2)

Movies_Fit3=lm(log(USRevenue)~Opening+Theaters+Ratings,data=Movies)
plot(Movies_Fit3,pch=16)
summary(Movies_Fit3)
anova(Movies_Fit3,Movies_Fit2)
NewMovie=data.frame(Opening=22.45,Theaters=2921,Ratings=7.7)
predict(Movies_Fit3,interval='predict',newdata=NewMovie)
exp(predict(Movies_Fit3,interval='predict',newdata=NewMovie))



College = read.csv(file.choose())
round(cor(College[, c("Admit", "GradRate", "InCostAid", "OutCostAid", "AvgDebt")]), 2)

model = lm(AvgDebt ~ Admit + GradRate + InCostAid + OutCostAid, data=College)
summary(model)

NewData=subset(College, Name!="Baruch College of the City University of New York")
model = lm(AvgDebt ~ Admit + GradRate + InCostAid + OutCostAid, data=NewData)
summary(model)

Gaming = read.csv(file.choose())
fit=glm(Gamer~Gender,family=binomial,data=Gaming)
summary(fit)

Heart = read.csv(file.choose())
fit = glm(CD~Age, family = binomial, data = Heart)
summary(fit)
round(exp(confint(fit)),3)


College_Fit = lm(AvgDebt ~ Admit + GradRate + InCostAid + OutCostAid, data=College)
plot(College_Fit,pch=16)

NewData <- subset(CollegeDebt, Name != "Baruch College of the City University of New York")

# Select only the numeric predictor and response variables
Data <- NewData[, c("Admit", "GradRate", "InCostAid", "OutCostAid", "AvgDebt")]

# Create scatterplot matrix
plot(Data, pch = 16)


FullModel <- lm(AvgDebt ~ Admit + GradRate + InCostAid + OutCostAid, data = NewData)

# Reduced model with only the significant predictor
ReducedModel <- lm(AvgDebt ~ InCostAid, data = NewData)

# Perform the partial F-test
anova(ReducedModel, FullModel)

model <- lm(Tornadoes ~ Year, data = TornadoesData)


# Load the dataset
TornadoesData <- read.csv(file.choose())

# Fit the linear regression model
model <- lm(Tornadoes ~ Year, data = TornadoesData)

# Calculate prediction intervals
PredictionData <- cbind(TornadoesData, predict(model, interval = "prediction"))

# Create the plot using code format from the slides
library(ggplot2)

ggplot(PredictionData, aes(x = Year, y = Tornadoes)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_line(aes(y = lwr), col = "red", lty = "dashed") +
  geom_line(aes(y = upr), col = "red", lty = "dashed") +
  ggtitle("Tornadoes by Year with Prediction Intervals")



Tornado = read.csv(file.choose())
head(Tornado)
plot(Tornado, pch = 16)

Crops = read.csv(file.choose())

SLR=lm(Yield~Temp,Crops)
summary(SLR)


QLR=lm(Yield~poly(Temp,2),Crops)
summary(QLR)


CLR=lm(Yield~poly(Temp,3),Crops)
summary(CLR)
