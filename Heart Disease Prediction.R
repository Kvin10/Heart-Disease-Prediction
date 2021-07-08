library("ggplot2")
library(corrr)
library(dplyr)

#Importing dataset
df <- read.csv("C://Users//kkuri//Downloads//heart//heart.CSV")
str(df)

#Splitting to train and test sets
df1 = sort(sample(nrow(df), nrow(df)*.7))
train<-df[df1,]
test<-df[-df1,]

#Function to find correlation of different parameters to the output variable
df %>% correlate() %>% focus(target)

#Check for any blank spaces in the dataset
sum(is.na(df))

#Shapiro-Wilks test to check normality if independent variables
shapiro.test(df$thalach)
shapiro.test(df$oldpeak)

#Plotting the histogram of 2 parameters with highest correlation to the output variable 
ggplot(data = df) + geom_histogram(aes(x=thalach), bins = 15, col = "green", fill = "darkgreen")+ 
  ggtitle("Frequency plot of Max. Heart Rate achieved") + xlab("Max. Heart Rate achieved") + ylab("Frequency") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size = 20),axis.title = element_text(size = 15))

ggplot(data = df) + geom_histogram(aes(x = oldpeak), bins = 8, fill = "royalblue2", col = "cyan") +
  ggtitle("Frequncy Plot of ST depression") + xlab("ST depression induced related by exercise relative to rest") + 
  ylab("Frequency") +  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 20),
                                          axis.title = element_text(size = 15))

#Plotting the histogram of the output variable
ggplot(data = df) + geom_histogram(aes(x = target),bins = 2, fill = "darkolivegreen3", col = "cyan") + 
  theme_light() + ggtitle("Frequency plot of Heart Disease detected") + xlab("Heart disease target") + 
  ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5, size = 20),axis.title = element_text(size = 15))

#Converting the output variable to 'factor' type for creating models
train[,'target']<-factor(train[,'target'])
df[,'target']<-factor(df[,'target'])

#Plotting Condition Density plots of to show the relationship of the 
#two independent variables to the target variable
data("df",package="HSAUR3")
layout(matrix(1:2,ncol=2))
cdplot(target ~ oldpeak, data=df, border = "blue", main = "CDPlot - ST Depression induced by exercise relative to rest", 
       xlab = "ST Depression induced")
cdplot(target ~ thalach, data=df,border = "green", main = "CDPlot - Max. Heart Rate achieved", 
       xlab = "Max. Herart Rate achieved")

#GLM Model 1 is created using the first independent variable
model_1 <- glm(target ~ oldpeak, data = train,
               family = binomial())

#Generating the report of the model
summary(model_1)

#GLM Model 2 is created by using both variables
model_2 <- glm(target ~ oldpeak + thalach, data = train,
               family = binomial())

#Generating the summary of second model
summary(model_2)

#Generating a model 3 to check if the independent variables interact with each other to predict the target variable
model_3 <-glm(target ~ oldpeak*thalach, data = train,
              family = binomial())

#Summary report of the third model
summary(model_3)

#Running Anova test to determine the best model
anova(model_1, model_2, test = "Chisq")
anova(model_1, model_3, test = "Chisq")
anova(model_2, model_3, test = "Chisq")

#95% Confidence Interval of independent variables
confint(model_2, parm = "oldpeak")
confint(model_2, parm = "thalach")

#function to determine the odds-ratio between independent and target variables
exp(coef(model_2)["oldpeak"])
exp(coef(model_2)["thalach"])

#Predicting the responses of the model in a variable 
prob <- predict(model_2, test, type = "response")

#Creating a bubble plot between the independent variables in the testing dataset
ggplot(data = test, aes(x=thalach, y=oldpeak, size = prob)) + geom_point(col = "darkslateblue", alpha=0.8) +
  scale_size(range = c(1, 15), name = "Prediction") + xlab("Max. Heart Rate") +ylab("ST depression induced") + 
  ggtitle("Relation of independent variables on the model") + theme_light() +  
  theme(plot.title = element_text(hjust = 0.5))
