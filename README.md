# Predictive Modelling using Regression
#Reading Data in R:
> library(readr)
> Mydata <- read_csv("UIC/Spring 18/IDS 575 Adv Stat.s/Project/242119447_T_ONTIME.csv")
> View(Mydata)

#Cutting the table short in R:
> Mydata1=data.frame(Mydata$DAY_OF_MONTH,Mydata$UNIQUE_CARRIER,Mydata$TAIL_NUM,Mydata$ORIGIN,Mydata$DEST,Mydata$DEP_DELAY,Mydata$DEP_DEL15,Mydata$ARR_DELAY,Mydata$ARR_DEL15,Mydata$CANCELLED,Mydata$AIR_TIME,Mydata$DISTANCE,Mydata$CARRIER_DELAY,Mydata$WEATHER_DELAY,Mydata$NAS_DELAY,Mydata$SECURITY_DELAY,Mydata$LATE_AIRCRAFT_DELAY)
> View(Mydata1)

#Replacing Missing values with zero:
> Mydata1$Mydata.AIR_TIME[is.na(Mydata1$Mydata.AIR_TIME)]=0
> Mydata1$Mydata.DISTANCE[is.na(Mydata1$Mydata.DISTANCE)]=0
> Mydata1$Mydata.CARRIER_DELAY[is.na(Mydata1$Mydata.CARRIER_DELAY)]=0
> Mydata1$Mydata.WEATHER_DELAY[is.na(Mydata1$Mydata.WEATHER_DELAY)]=0
> Mydata1$Mydata.NAS_DELAY[is.na(Mydata1$Mydata.NAS_DELAY)]=0
> Mydata1$Mydata.SECURITY_DELAY[is.na(Mydata1$Mydata.SECURITY_DELAY)]=0
> Mydata1$Mydata.LATE_AIRCRAFT_DELAY[is.na(Mydata1$Mydata.LATE_AIRCRAFT_DELAY)]=0
> Mydata1$Mydata.DEP_DELAY[is.na(Mydata1$Mydata.DEP_DELAY)]=0
> Mydata1$Mydata.DEP_DEL15[is.na(Mydata1$Mydata.DEP_DEL15)]=0
> Mydata1$Mydata.ARR_DEL15[is.na(Mydata1$Mydata.ARR_DEL15)]=0
> Mydata1$Mydata.ARR_DELAY[is.na(Mydata1$Mydata.ARR_DELAY)]=0

#Ordering Data by DAY_OF_MONTH,UNIQUE_CARRIER & TAIL_NUM:
> Mydata2=Mydata1[order(Mydata1$Mydata.DAY_OF_MONTH,Mydata1$Mydata.UNIQUE_CARRIER,Mydata1$Mydata.TAIL_NUM),]
> View(Mydata2)

#Creating ArrivalLag & DepartureLag variables:
> Mydata2$ArrDelLag=ave(Mydata2$Mydata.ARR_DELAY,Mydata2$Mydata.TAIL_NUM,Mydata2$Mydata.DAY_OF_MONTH,FUN=cumsum)
> Mydata2$DepDelLag=ave(Mydata2$Mydata.DEP_DELAY,Mydata2$Mydata.TAIL_NUM,Mydata2$Mydata.DAY_OF_MONTH,FUN=cumsum)

#For calculating cumulative distance & airtime in a Sequence of flights:
> Mydata2$cumdist=ave(Mydata2$Mydata.DISTANCE,Mydata2$Mydata.TAIL_NUM,Mydata2$Mydata.DAY_OF_MONTH,FUN=cumsum)
> Mydata2$cumairtime=ave(Mydata2$Mydata.AIR_TIME,Mydata2$Mydata.TAIL_NUM,Mydata2$Mydata.DAY_OF_MONTH,FUN=cumsum)

# Linear Regression for all Flights (to check p-value):
> fit=lm(Mydata.DEP_DELAY~ Mydata.ARR_DELAY + Mydata.AIR_TIME + Mydata.DISTANCE + Mydata.CARRIER_DELAY + Mydata.WEATHER_DELAY + Mydata.NAS_DELAY + Mydata.SECURITY_DELAY + Mydata.LATE_AIRCRAFT_DELAY + Mydata.DAY_OF_MONTH, data = Mydata2)
> Summary(fit)

# Codes for partitioning data for All Flights:
> library(h2o)
> h2o.init(nthreads=-1)
> Flightdata.hex=as.h2o(Mydata2)
> Flightdata.split = h2o.splitFrame(data=Flightdata.hex,ratios = 0.7)
> Flighdata.train = Flightdata.split[[1]]
> Flighdata.test = Flightdata.split[[2]]

# Codes for linear model All Flights:
> Flight.glm = h2o.glm(y="Mydata.DEP_DELAY",x=c("Mydata.ARR_DELAY","Mydata.AIR_TIME" , "Mydata.DISTANCE","Mydata.CARRIER_DELAY","Mydata.WEATHER_DELAY","Mydata.NAS_DELAY", "Mydata.SECURITY_DELAY","Mydata.LATE_AIRCRAFT_DELAY","Mydata.DAY_OF_MONTH"),training_frame=Flighdata.train,family="gaussian", lambda = 0, remove_collinear_columns=TRUE,compute_p_values=TRUE,solver="IRLSM",nfolds = 10)
> h2o.coef(Flight.glm)
> h2o.performance(Flight.glm,newdata=Flightdata.split[[1]])
> h2o.performance(Flight.glm,newdata=Flightdata.split[[2]]) (to check model performance)
> summary(Flight.glm)

# Codes for linear model All Flights for LASSO:
> Flight.glm = h2o.glm(y="Mydata.DEP_DELAY",x=c("Mydata.ARR_DELAY","Mydata.AIR_TIME" , "Mydata.DISTANCE","Mydata.CARRIER_DELAY","Mydata.WEATHER_DELAY","Mydata.NAS_DELAY", "Mydata.SECURITY_DELAY","Mydata.LATE_AIRCRAFT_DELAY","Mydata.DAY_OF_MONTH"),training_frame=Flighdata.train,family="gaussian", lambda = 0.1,alpha = 1, remove_collinear_columns=TRUE,solver="IRLSM",nfolds = 10)
> h2o.performance(Flight.glm,newdata=Flightdata.split[[2]]) (to check model performance-not preventing overfitting as same RMSE)

# Codes for linear model All Flights for Ridge:
> Flight.glm = h2o.glm(y="Mydata.DEP_DELAY",x=c("Mydata.ARR_DELAY","Mydata.AIR_TIME" , "Mydata.DISTANCE","Mydata.CARRIER_DELAY","Mydata.WEATHER_DELAY","Mydata.NAS_DELAY", "Mydata.SECURITY_DELAY","Mydata.LATE_AIRCRAFT_DELAY","Mydata.DAY_OF_MONTH"),training_frame=Flighdata.train,family="gaussian", lambda = 0.1,alpha = 0, remove_collinear_columns=TRUE,solver="IRLSM",nfolds = 10)
> h2o.performance(Flight.glm,newdata=Flightdata.split[[2]]) (to check model performance-not preventing overfitting as same RMSE)

# Linear Regression for all Flights by Airline:
> library(lme4)
> fit=lmList(Mydata.DEP_DELAY~ Mydata.ARR_DELAY + Mydata.AIR_TIME + Mydata.DISTANCE + Mydata.CARRIER_DELAY + Mydata.WEATHER_DELAY + Mydata.NAS_DELAY + Mydata.SECURITY_DELAY + Mydata.LATE_AIRCRAFT_DELAY + Mydata.DAY_OF_MONTH | Mydata.UNIQUE_CARRIER,data = Mydata2)
> fit

# keeping flights which are dealyed by more than 15 minutes only:
> Mydata3=Mydata2[which (Mydata2$Mydata.DEP_DEL15==1),]

# Removing Lag variables from Mydata3:
> Mydata3$ArrDelLag=NULL
> Mydata3$DepDelLag=NULL

#For making a Sequence of Delayed flights:
> Mydata3$SeqID=ave(Mydata3$Mydata.DEP_DEL15,Mydata3$Mydata.TAIL_NUM,Mydata3$Mydata.DAY_OF_MONTH,FUN=cumsum)

#For calculating cumulative distance & airtime in a Sequence of Delayed flights:
> Mydata3$cumdist=ave(Mydata3$Mydata.DISTANCE,Mydata3$Mydata.TAIL_NUM,Mydata3$Mydata.DAY_OF_MONTH,FUN=cumsum)
> Mydata3$cumairtime=ave(Mydata3$Mydata.AIR_TIME,Mydata3$Mydata.TAIL_NUM,Mydata3$Mydata.DAY_OF_MONTH,FUN=cumsum)

# Codes for partitioning data for flights with delay more than 15 minutes:
> library(h2o)
> h2o.init(nthreads = -1)
> Flightdata.hex=as.h2o(Mydata3)
> Flightdata.split = h2o.splitFrame(data=Flightdata.hex,ratios = 0.7)
> Flighdata.train = Flightdata.split[[1]]
> Flighdata.test = Flightdata.split[[2]]

# Codes for linear model for Flights greater than 15 minutes of delay
> Flight.glm = h2o.glm(y="Mydata.DEP_DELAY",x=c("Mydata.ARR_DELAY","Mydata.AIR_TIME" , "Mydata.DISTANCE","Mydata.CARRIER_DELAY","Mydata.WEATHER_DELAY","Mydata.NAS_DELAY", "Mydata.SECURITY_DELAY","Mydata.LATE_AIRCRAFT_DELAY","Mydata.DAY_OF_MONTH","cumdist","cumairtime"),training_frame=Flighdata.train,family="gaussian", lambda = 0, remove_collinear_columns=TRUE,compute_p_values=TRUE,solver="IRLSM",nfolds = 10)
> h2o.coef(Flight.glm)
> h2o.performance(Flight.glm,newdata=Flightdata.split[[1]])
> h2o.performance(Flight.glm,newdata=Flightdata.split[[2]]) (to check model performance)

# Codes for partitioning data for logistic regression on all flights:
> library(h2o)
> h2o.init(nthreads=-1)
> Flightdata.hex=as.h2o(Mydata2)
> Flightdata.split = h2o.splitFrame(data=Flightdata.hex,ratios = 0.7)
> Flighdata.train = Flightdata.split[[1]]
> Flighdata.test = Flightdata.split[[2]]

# Logistic Regression for all Flights:
> Flight.glm = h2o.glm(y="Mydata.DEP_DEL15",x=c("Mydata.ARR_DELAY","Mydata.AIR_TIME" , "Mydata.DISTANCE","Mydata.CARRIER_DELAY","Mydata.WEATHER_DELAY","Mydata.NAS_DELAY", "Mydata.SECURITY_DELAY","Mydata.LATE_AIRCRAFT_DELAY","Mydata.DAY_OF_MONTH","ArrDelLag","DepDelLag","cumdist","cumairtime"),training_frame=Flighdata.train,family="binomial", lambda = 0, remove_collinear_columns=TRUE,compute_p_values=TRUE,solver="IRLSM",nfolds = 10)
> pred=h2o.predict(object=Flight.glm,newdata = Flighdata.test)
> summary(pred$p1,exact_quantiles=TRUE) #giving same as 1 as the number in original dataset which means a good prediction model
> h2o.coef(Flight.glm)

#Decision Trees
# Building Prediction Tree in R:
> library(rpart)
> Flight1=data.frame(Mydata2$Mydata.DEP_DEL15,Mydata2$Mydata.DAY_OF_MONTH,Mydata2$Mydata.UNIQUE_CARRIER,Mydata2$Mydata.DEP_DELAY,Mydata2$Mydata.ARR_DELAY,Mydata2$Mydata.ARR_DEL15,Mydata2$Mydata.CANCELLED,Mydata2$Mydata.AIR_TIME,Mydata2$Mydata.DISTANCE,Mydata2$Mydata.CARRIER_DELAY,Mydata2$Mydata.WEATHER_DELAY,Mydata2$Mydata.NAS_DELAY,Mydata2$Mydata.SECURITY_DELAY,Mydata2$Mydata.LATE_AIRCRAFT_DELAY)
> table(Flight1$Mydata2.Mydata.DEP_DEL15)
> hist(Flight1$Mydata2.Mydata.DEP_DELAY) random guess of common occurence is flight is on time (81% of time)

> cols=c('Mydata2.Mydata.DEP_DEL15','Mydata2.Mydata.ARR_DEL15','Mydata2.Mydata.CANCELLED')
> Flight1[cols] <- lapply(Flight1[cols], as.factor)
> set.seed(1)
> train <- sample(1:nrow(Flight1), 0.75 * nrow(Flight1))
> FlightTree=rpart(Mydata2.Mydata.DEP_DEL15~.- Mydata2.Mydata.DEP_DELAY,data=Flight1[train,],method='class')
> plot(FlightTree)
> text(FlightTree,pretty=0)
> library(maptree)
> draw.tree(FlightTree,cex=1)
> summary(FlightTree)
> Dep_Del15Pred=predict(FlightTree,Flight1[-train,],type='class')
> table(Dep_Del15Pred,Flight1[-train,]$Mydata2.Mydata.DEP_DEL15) 94% accuracy achieved

# Using Random Forest for Prediction Tree:
> require(randomForest)
> train=sample(1:nrow(Flight1),1000)
> Flight.rf=randomForest(Flight1$Mydata2.Mydata.DEP_DEL15 ~.,data = Flight1,subset = train)
> Flight.rf
> plot(Flight.rf)
> Dep_Del15Pred=predict(Flight.rf,Flight1[-train,],type='class')
> table(Dep_Del15Pred,Flight1[-train,]$Mydata2.Mydata.DEP_DEL15) 100% accuracy after random forest

# Building Regression Tree:
> library(tree)
> FlightRegressionTree=tree(Mydata2.Mydata.DEP_DELAY~ Mydata2.Mydata.ARR_DELAY + Mydata2.Mydata.AIR_TIME + Mydata2.Mydata.DISTANCE + Mydata2.Mydata.CARRIER_DELAY +Mydata2.Mydata.WEATHER_DELAY + Mydata2.Mydata.NAS_DELAY + Mydata2.Mydata.SECURITY_DELAY + Mydata2.Mydata.LATE_AIRCRAFT_DELAY + Mydata2.Mydata.DAY_OF_MONTH, data = Flight1)
> plot(FlightRegressionTree)
> text(FlightRegressionTree, cex=.65)
> summary(FlightRegressionTree)

