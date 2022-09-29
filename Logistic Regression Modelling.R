
#Logistic Regression Modelling
#To predict which telecom customers are more probable to turn away based on the attributes to the customer 

list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc","aod","BaylorEdPsych","ResourceSelection","pROC","ROCR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)

Path<-"desktop/neha/data"
getwd()


data<-read.csv("TelcoCustomer-Churn.csv",header = TRUE)
data1=data
head(data1)
str(data1)
summary(data1)
dim(data1)

#Changing into factor
cols_cat<-c("gender","SeniorCitizen","Partner","Dependents","PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod","TechSupport","Churn")
data1[cols_cat]<-lapply(data1[cols_cat],factor)
summary(data1)

data1$SeniorCitizen<-as.factor(data1$SeniorCitizen)
data1$Churn<-as.factor(data1$Churn)
str(data1)


#Missing Value Treatment 
data.frame(colSums(is.na(data1)))

data1[is.na(data1$TotalCharges),19]=mean(data1$TotalCharges,na.rm=T)
data.frame(colSums(is.na(data1)))

#Splitting the data 

set.seed(144)

spl = sample.split(data1$Churn, 0.7)
data.train = subset(data1, spl == TRUE)
str(data.train)
dim(data.train)


data.test = subset(data1, spl == FALSE)
str(data.test)
dim(data.test)


#Logistic Regression Model Building


model <- glm(Churn~., data=data.train, family=binomial())
summary(model)
## Remove the insignificant variable
model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService + MultipleLines
             + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV 
             + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges 
             + TotalCharges, data=data.train, family=binomial())
summary(model)

model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService 
             + I(MultipleLines=="Yes")+ InternetService + I(OnlineSecurity=="Yes")+ I(OnlineBackup=="Yes") 
             + I(DeviceProtection=="Yes") + I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove Partner, PhoneService, gender 
model <- glm(Churn~ 	SeniorCitizen  + tenure + Dependents
             + I(MultipleLines=="Yes")+ InternetService + I(OnlineSecurity=="Yes")+ I(OnlineBackup=="Yes") 
             + I(DeviceProtection=="Yes") + I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove  I(OnlineBackup=="Yes"), I(DeviceProtection=="Yes") , I(OnlineSecurity=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService   +  I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove   I(TechSupport=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + I(PaymentMethod=="Electronic check")+	tenure , data=data.train, family=binomial())
summary(model)

vif(model)


model <- glm(Churn~ 	SeniorCitizen  
              + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
              + I(PaymentMethod=="Electronic check")+tenure, data=data.train, family=binomial())
summary(model)
vif(model)


#using Wald Test
wald.test(b=coef(model), Sigma= vcov(model), Terms=1:12)

#Lagrange Multiplier or Score Test 
# Difference betweene null deviance and deviance
modelChi <- model$null.deviance - model$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chidf

chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)



#Lackfit Deviance for assessing wether the model where
residuals(model) 
residuals(model, "pearson")
sum(residuals(model, type = "pearson")^2)
deviance(model)

#Larger p value indicate good model fit
1-pchisq(deviance(model), df.residual(model))
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies



# Coefficients (Odds)
model$coefficients
# Coefficients (Odds Ratio)
exp(model$coefficients)#Interpret 
#if the customer has Internet Service = Fiber Optic, then he is 2.53 times more likely to churn

# Variable Importance of the model
varImp(model)

# Predicted Probabilities
prediction <- predict(model,newdata = data.train,type="response")#Classification Model
#prediction <- predict(model,newdata = data.train,probability=TRUE)

prediction

write.csv(prediction,"pred.csv")


rocCurve   <- roc(response = data.train$Churn, predictor = factor(prediction, ordered =TRUE), 
                  levels = rev(levels(data.train$Churn)))
data.train$Churn <- as.factor(data.train$Churn)

#Metrics - Fit Statistics

threshold<-as.numeric(coords(rocCurve,"best")[1])
#predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
predclass <-ifelse(prediction>threshold,1,0)

Confusion <- table(Predicted = predclass,Actual = data.train$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
#Gini Coefficient is the area under the Lorenz Curve 
Confusion
auc(rocCurve)
plot(rocCurve)

# KS statistics calculation
data.train$m1.yhat <- predict(model, data.train, type = "response")
m1.scores <- prediction(data.train$m1.yhat, data.train$Churn)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit 

$ git reset --hard HEAD~1
