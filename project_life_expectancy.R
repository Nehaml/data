Path<-"desktop/neha/data"
getwd()

data=read.csv("statedata.csv")
data1=data 

str(data1)
summary(data1)
dim(data1)

as.data.frame(colSums(is.na(data1)))

as.data.frame(colnames(data1))
data2=subset(data1,select = -c(state.division,state.name,state.area,x,y,state.abb,state.region))
str(data2)
summary(data2)

#outlier treatmeant for population and area
quantile(data2$Population,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data3=data2[data2$Population <12067.35,]
nrow(data2)
nrow(data3)
nrow(data2)-nrow(data3)
quantile(data3$Population,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.93,0.95,0.99,0.995,1))

quantile(data2$Area,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data3=data2[data2$Area <151512.70,]
nrow(data2)
nrow(data3)
nrow(data2)-nrow(data3)
quantile(data3$Area,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.93,0.95,0.99,0.995,1))
 
#final the data for ml
str(data3)
summary(data3)


write.csv(data3,"MLdata.csv",row.names = FALSE)

set.seed(123)

spl = sample.split(data3$Life.Exp , 0.7)
original1.data = subset(data3, spl == TRUE)
str(original1.data)
dim(original1.data)

test.data = subset(data3, spl == FALSE)
str(test.data)
dim(test.data)

options(scipen = 999)
Model=lm(Life.Exp ~.,data=original1.data)
summary(Model)


vif(Model)

fitted(Model)
par(mfrow=c(2,2))
plot(Model)

original1.data$pred <- fitted(Model)
Actual_pred =dplyr::select(original1.data,c(Life.Exp,pred))
Actual_pred$error = Actual_pred$Life.Exp-Actual_pred$pred
summary(Actual_pred$error)
write.csv(original1.data,"mape.csv")

attach(original1.data)
MAPE <- print((sum((abs(Life.Exp-pred))/Life.Exp))/nrow(original1.data))

durbinWatsonTest(Model)
dwt(Model)











