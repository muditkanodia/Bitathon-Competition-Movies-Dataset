library(car)
library(corrplot)
library(caret)
library(caTools)
library(stringr)
library(psych)
library(ggeffects)
library(splines)

#merging
f2001 <- read.csv("2001 Financials.csv")
f2002 <- read.csv("2002 Financials.csv")
f2003 <- read.csv("2003 Financials.csv")
f2004 <- read.csv("2004 Financials.csv")
f2005 <- read.csv("2005 Financials.csv")
f2006 <- read.csv("2006 Financials.csv")
f2007 <- read.csv("2007 Financials.csv")
f2008 <- read.csv("2008 Financials.csv")
f2009 <- read.csv("2009 Financials.csv")
f2010 <- read.csv("2010 Financials.csv")
f2011 <- read.csv("2011 Financials.csv")
f2012 <- read.csv("2012 Financials.csv")
f2013 <- read.csv("2013 Financials.csv")
f2014 <- read.csv("2014 Financials.csv")

moviedetail <- read.csv("BollywoodMovieDetail.csv")

#View(f2014)
#View(moviedetail)
#colnames(f2014)
#colnames(moviedetail)

#combined_df <- rbind(f2001,f2002,f2003,f2004,f2005,f2006,f2007,f2008,f2009,f2010,f2011,f2012,f2013,f2014)
View(combined_df)
#write.csv(combined_df,"Combined_financials.csv")
combined_df <- read.csv("Combined_financials.csv")

df1<-merge(x=combined_df,y=moviedetail,by.x = "ï..Movie.Name", by.y = "title")
View(df1)
summary(df1)
write.csv(df1,"NEWMASTER.CSV")
df<-df1[-8]
View(df)

ndf <- read.csv("NEWMASTER1.CSV")
View(ndf)
ndf1<-ndf[c("Opening.Day","Opening.Weekend","End.of.Week.1","Lifetime..India.Only..Net.Collection.Crores.in.Release.Year.","FirstWeekIMDBRating")]
colnames(ndf)


ndf1$Opening.Day<-ifelse(ndf1$Opening.Day=="0N.A",' ',ndf1$Opening.Day)
typeof(ndf1$Opening.Day)
typeof(ndf1$Opening.Weekend)
typeof(ndf1$End.of.Week.1)
typeof(ndf1$ Lifetime..India.Only..Net.Collection.Crores.in.Release.Year.)
ndf<-na.omit(ndf1)
str(df)
summary(df)
summary(ndf1)

ndf1$End.of.Week.1<-as.numeric(ndf1$End.of.Week.1)


#correlation matrix
library(corrplot)
cr<-cor(ndf1)
corrplot(cr,type="full")
write.csv(cr,"corr.csv")


#splitting
split<-sample.split(ndf1$FirstWeekIMDBRating,SplitRatio = 0.7)
training_data<-subset(ndf1,split=TRUE)
testing_data<-subset(ndf1,split=FALSE)


#model creation
model1<-lm(FirstWeekIMDBRating~.,data=training_data)
summary(model1)
model2<-lm((FirstWeekIMDBRating)^2~Opening.Weekend+End.of.Week.1+Lifetime..India.Only..Net.Collection.Crores.in.Release.Year.,data=training_data)
summary(model2)

qqPlot(model2$residuals)
plot(model2$fitted.values,model2$residuals)
pairs.panels(ndf1)

model3<-lm(log(FirstWeekIMDBRating)~Opening.Weekend+End.of.Week.1+Lifetime..India.Only..Net.Collection.Crores.in.Release.Year.,data=training_data)
summary(model3)

par(mfrow=c(2,2))
summary(model3)
plot(model3)
plot(model3$fitted.values,model3$residuals)

par(mfrow=c(1,1))
qqPlot(model3$residuals)


model4<-lm(sqrt(FirstWeekIMDBRating)~((Opening.Weekend))+(sqrt(End.of.Week.1))+(sqrt(Lifetime..India.Only..Net.Collection.Crores.in.Release.Year.)),data=training_data)
summary(model4)
par(mfrow=c(1,1))
plot(model4)
qqPlot(model4$residuals)


model5<-lm(sqrt(FirstWeekIMDBRating)~((Opening.Weekend))+(log(End.of.Week.1))+(sqrt(Lifetime..India.Only..Net.Collection.Crores.in.Release.Year.)),data=training_data)
summary(model5)
par(mfrow=c(1,1))
plot(model5)
qqPlot(model5$residuals)
plot(model5$fitted.values,model5$residuals)


model6<-lmrob((FirstWeekIMDBRating)~((Opening.Weekend))+(log(End.of.Week.1))+(sqrt(Lifetime..India.Only..Net.Collection.Crores.in.Release.Year.)),data=training_data)
summary(model6)
par(mfrow=c(1,1))
plot(model6)
qqPlot(model5$residuals)
plot(model5$fitted.values,model5$residuals)


#testing
prediction <- predict(model6,testing_data)
prediction <- round(prediction)
View(prediction)
View(testing_data)
plot(testing_data$FirstWeekIMDBRating,type = "l",col="red")
lines(prediction,type = "l",col="blue")

mean(testing_data$FirstWeekIMDBRating)
mean(prediction)
sd(testing_data$FirstWeekIMDBRating)
sd(prediction)
mean(training_data$FirstWeekIMDBRating)
sd(training_data$FirstWeekIMDBRating)
abline(model6)
ggpredict(model6,se=TRUE,interactive=TRUE)

vif(model6)

