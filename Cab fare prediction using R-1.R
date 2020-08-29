library("ggplot2")
library("Scale")
library("psych")
library("gplots")
library("DMwR")
library("corrgram")
library("dummies")
library("usdm")
library("rpart")
library("randomForest")

rm(list=ls())
setwd("C:/Users/hp/Desktop/Project cap fare prediction")
getwd()
train=read.csv("train_cab.csv",header = T)
test=read.csv("test.csv",header = T)
test_copy=test
##_______________DATA EXPLORATION AND DATA CLEANING_______________##

# 1.passenger_count
length(unique(train$passenger_count))
#Lets see distribution plot for passenger_count
train$passenger_count=round(train$passenger_count)
ggplot(train, aes_string(x=train$passenger_count))+geom_histogram(fill="red",color="black", binwidth = 100)+
  xlab("passenger_count")+ylab("freq")+ggtitle("distribution for passenger_count")
# As from above we can say that our passenger_count have negative and very high values which is not making sense
# from test data we can see that passenger_count is integers between(1,6),Lets see how many values except these lies in our train dataset  
#nrow(train[which(train$passenger_count<1),]) + nrow(train[which(train$passenger_count>6),]) 
# total 78 rows with unsensible values for passenger_count
# we can remove these values or we can replace them with NA then impute them using missing value imputattion methods
#Removing these Observations
#dim(train)
#train = train[-which(train$passenger_count < 1 ),]
#train = train[-which(train$passenger_count > 6),]
#dim(train)
#replacing Observations with NA
sum(is.na(train["passenger_count"]))
train$passenger_count[train$passenger_count>6]=NA
train$passenger_count[train$passenger_count<1]=NA
sum(is.na(train["passenger_count"]))
unique(train$passenger_count)
table(train$passenger_count)
train$passenger_count=round(train$passenger_count)
train$passenger_count=as.factor(train$passenger_count)
test$passenger_count=as.factor(test$passenger_count)

# 2) fare_amount
#Converting to Data type
train$fare_amount = as.numeric(as.character(train$fare_amount))
length(unique(train$fare_amount))
#Lets see distribution plot for fare_amount
ggplot(train, aes_string(x=train$fare_amount))+geom_histogram(fill="red",color="black", binwidth = 1000)+
  xlab("fare_amount")+ylab("freq")+ggtitle("distribution for fare_amount")
# As we can see from plot our fare_amount have abnormally high values and negative values lets again plot more clear visualization by settin limit on fare_amount values
ggplot(train, aes_string(x=train$fare_amount))+geom_histogram(fill="red",color="black")+xlab("fare_amount")+ylab("freq")+
  ggtitle("distribution for fare_amount")+ scale_x_continuous(breaks = seq(0,200, 20), lim = c(0, 200))
#Dealing with unsensible values of fare_amount
nrow(train[which(train$fare_amount<1),]) + nrow(train[which(train$fare_amount>200),])
# total 9 such rows
#Removing the values
#dim(train)
#train = train[-which(train$fare_amount < 1 ),]
#train = train[-which(train$fare_amount > 200),]
#dim(train)
#replacing Observations with NA
sum(is.na(train$fare_amount))
train$fare_amount[train$fare_amount>200]=NA
train$fare_amount[train$fare_amount<1]=NA
sum(is.na(train$fare_amount))
# 3)latitude and longitude
length(unique(train$pickup_longitude))
length(unique(train$pickup_latitude))
length(unique(train$dropoff_longitude))
length(unique(train$dropoff_latitude))
#lets see dributuions of these
par(mfrow=c(2,2))
plot(density(train$pickup_longitude),main="Distribution for pickup_longitude")
plot(density(train$pickup_latitude),main="Distribution for pickup_latitude")
plot(density(train$dropoff_longitude),main="Distribution for dropoff_longitude")
plot(density(train$dropoff_latitude),main="Distribution for dropoff_latitude")
# As we can see the most of the values of latitude lies between (39,41) and longitude lies between (-75,-72)
# from latlong.net we find out these data is for Newyork city.
# from general understanding latitude cannot be greater than 90, but some values for pickup_latitude are greater than that lets see how many such values
nrow(train[which(train$pickup_latitude>90),])
#only one such value
#Removing this value
#dim(train)
#train=train[-which(train$pickup_latitude>90),]
#dim(train)
#replacing with na
sum(is.na(train$pickup_latitude))
train$pickup_latitude[train$pickup_latitude>90]=NA
sum(is.na(train$pickup_latitude))
# Now lets see how many values of latitude and longitude lies away from latitude longitude values for Newyork city
nrow(train[which(train$dropoff_latitude < 39 | train$dropoff_latitude >42 |
                   train$pickup_latitude < 39 | train$pickup_latitude >42 |
                   train$dropoff_longitude < -75 | train$dropoff_longitude >-72|
                      train$pickup_longitude < -75 | train$pickup_longitude >-72),])
#Total 337 such observation, what we can do is we can relace them with NA or we can remove these observations
#Removing the observation
#dim(train)
#train=train[-which(train$dropoff_latitude < 39 | train$dropoff_latitude >42 |
#                    train$pickup_latitude < 39 | train$pickup_latitude >42 |
#                    train$dropoff_longitude < -75 | train$dropoff_longitude >-72|
#                    train$pickup_longitude < -75 | train$pickup_longitude >-72),]
#dim(train)         
#Replacing with NA
sum(is.na(train$pickup_latitude))
sum(is.na(train$pickup_longitude))
sum(is.na(train$dropoff_longitude))
sum(is.na(train$dropoff_latitude))
train$pickup_latitude[train$pickup_latitude < 39 ]=NA
train$pickup_latitude[train$pickup_latitude >42]=NA
train$dropoff_latitude[train$dropoff_latitude < 39]=NA
train$dropoff_latitude[train$dropoff_latitude >42]=NA
train$pickup_longitude[train$pickup_longitude < -75]=NA
train$pickup_longitude[train$pickup_longitude >-72]=NA
train$dropoff_longitude[train$dropoff_longitude < -75]=NA
train$dropoff_longitude[train$dropoff_longitude >-72]=NA
sum(is.na(train$pickup_latitude))
sum(is.na(train$pickup_longitude))
sum(is.na(train$dropoff_longitude))
sum(is.na(train$dropoff_latitude))


# 4) pickup_datetime
# pickup_datetime variable contains date and time of trio we can use this variable to create new features.
#we convert its data type in that step

#_______________Missing value Analysis_______________#
missing_value=data.frame(apply(train,2,function(x)(sum(is.na(x)))))
missing_value$variables=row.names(missing_value)
row.names(missing_value)= NULL
names(missing_value)[1]="missing_%"
missing_value=missing_value[,c(2,1)]
missing_value$`missing_%`=(missing_value$`missing_%`/nrow(train)*100)
missing_value=missing_value[order(-missing_value$`missing_%`),]

#What we can do is we can drop these values or we can imputate them using missing value imputation method
df1=train
#Removing missing values
#train=na.omit(train)
#sum(is.na(train))

#Imputing Missing values using Missing value imputation methods
# We first create missing value than we impute it using different method, than we compair it to actual value

# 1) passenger_count
# passenger_count is factor type datatype, we use mode and KNN method to impute missing values.
# Actual value= 1
# Mode= 1 
# KNN= 1
df=df1
df$passenger_count[1000]
#replacing it with NA
df$passenger_count[1000]=NA
df$passenger_count[1000]
table(df$passenger_count)
# from this we can see the mode is 1
#imputing using KNN
df$passenger_count[1000]=NA
df = knnImputation(df, k = 9)
df$passenger_count[1000]
sum(is.na(df))
# 2) fare_amount
# it is numeric type variable we use mean. median and KNN method to impute it
# Actual Value= 5.7
# mean= 11.31
# median= 8.5
# KNN=5.43
df=df1
sum(is.na(df))
df$fare_amount[100]
df$fare_amount[100]=NA
df$fare_amount[100]
summary(df$fare_amount)
#imputing using KNN
df = knnImputation(df, k = 9)
df$fare_amount[100]

# 3) latitude and longitudes
#               pickup_latitude      pickup_longitude     dropoff_latitude    dropodd_longitude
# Actual_value    40.75843             -73.98846            40.73015            -73.98382
# Mean            40.75092             -73.97482            40.75141            -73.97385
# Median          40.75332             -73.98205            40.75424            -73.98058  
# KNN             40.74677             -73.98412            40.76076            -73.97385
# As we see most of the Missing value for latitude and longitude variables lies together, we will use KNN by creating missing values in same Row
#Mean
df=df1
df[1000,]
df$pickup_latitude[1000]=NA
df$pickup_longitude[1000]=NA  
df$dropoff_latitude[1000]=NA
df$dropoff_longitude[1000]=NA
df[1000,]
df$pickup_latitude[is.na(df$pickup_latitude)]=mean(df$pickup_latitude , na.rm=T)
df$pickup_longitude[is.na(df$pickup_longitude)]=mean(df$pickup_longitude, na.rm=T)
df$dropoff_latitude[is.na(df$dropoff_latitude)]=mean(df$dropoff_latitude , na.rm=T)
df$dropoff_longitude[is.na(df$dropoff_longitude)]=mean(df$dropoff_longitude , na.rm=T)
df[1000,]
#Median
df=df1
df[1000,]
df$pickup_latitude[1000]=NA
df$pickup_longitude[1000]=NA
df$dropoff_latitude[1000]=NA
df$dropoff_longitude[1000]=NA
df[1000,]
df$pickup_latitude[is.na(df$pickup_latitude)]=median(df$pickup_latitude , na.rm=T)
df$pickup_longitude[is.na(df$pickup_longitude)]=median(df$pickup_longitude, na.rm=T)
df$dropoff_latitude[is.na(df$dropoff_latitude)]=median(df$dropoff_latitude , na.rm=T)
df$dropoff_longitude[is.na(df$dropoff_longitude)]=median(df$dropoff_longitude , na.rm=T)
df[1000,]
#KNN
df=df1
df[1000,]
df$pickup_latitude[1000]=NA
df$pickup_longitude[1000]=NA
df$dropoff_latitude[1000]=NA
df$dropoff_longitude[1000]=NA
df[1000,]
df = knnImputation(df, k = 9)
df[1000,]

#MIssing value imputation

#Imputing missing values in pickup_latitude,dropoff_longitude we use median and for dropoff_latitude we use mean
sum(is.na(train$pickup_latitude))
sum(is.na(train$dropoff_latitude))
sum(is.na(train$dropoff_longitude))

train$pickup_latitude[is.na(train$pickup_latitude)]=median(train$pickup_latitude, na.rm=T)
train$dropoff_longitude[is.na(train$dropoff_longitude)]=median(train$dropoff_longitude, na.rm = T)
train$dropoff_latitude[is.na(train$dropoff_latitude)]=mean(train$dropoff_latitude, na.rm =T)

sum(is.na(train$pickup_latitude))
sum(is.na(train$dropoff_latitude))
sum(is.na(train$dropoff_longitude))

#Imputing missing values in passenger_count, fare_amount, pickuo_longitude using KNN
sum(is.na(train$pickup_longitude))
sum(is.na(train$passenger_count))
sum(is.na(train$fare_amount))

train=knnImputation(train, k=9)
sum(is.na(train))

#Now after imputing all missing values lets see there distribution for latitude and longitude 
summary(train$pickup_longitude)
summary(train$pickup_latitude)
summary(train$dropoff_longitude)
summary(train$dropoff_latitude)
par(mfrow=c(2,2))
plot(density(train$pickup_longitude),main="Distribution for pickup_longitude")
plot(density(train$pickup_latitude),main="Distribution for pickup_latitude")
plot(density(train$dropoff_longitude),main="Distribution for dropoff_longitude")
plot(density(train$dropoff_latitude),main="Distribution for dropoff_latitude")
#Now we can see all the values are in range and all these 4 curves are not normally distributed

#Lets plot the scatter plot for pickup location and dropoff location to understand the data more clearly
ggplot(train,aes_string(x=train$pickup_latitude,y=train$pickup_longitude))+geom_point(color="blue")+xlab("pickup_latitude")+
  ylab("pickup_longitude")+theme_bw()+ggtitle("pickup_location")
ggplot(train,aes_string(x=train$dropoff_latitude,y=train$dropoff_longitude))+geom_point(color="red")+xlab("dropoff_latitude")+
  ylab("dropoff_longitude")+theme_bw()+ggtitle("dropoff_location")
#most of pickup and dropoff location are concentrated.
#lets also see how fare_amount is varying with latitude and longitude
ggplot(train,aes_string(x=train$pickup_latitude,y=train$fare_amount))+geom_point(color="blue")+xlab("pickup_latitude")+
  ylab("fare_amount")+theme_bw()+ggtitle("variation in fare_amount with pickup_latitude")
ggplot(train,aes_string(x=train$pickup_longitude,y=train$fare_amount))+geom_point(color="blue")+xlab("pickup_longitude")+
  ylab("fare_amount")+theme_bw()+ggtitle("variation in fare_amount with pickup_longitude")
ggplot(train,aes_string(x=train$dropoff_latitude,y=train$fare_amount))+geom_point(color="red")+xlab("dropoff_latitude")+
  ylab("fare_amount")+theme_bw()+ggtitle("variation in fare_amount with dropoff_latitude")
ggplot(train,aes_string(x=train$dropoff_longitude,y=train$fare_amount))+geom_point(color="red")+xlab("dropoff_longitude")+
  ylab("fare_amount")+theme_bw()+ggtitle("variation in fare_amount with dropoff_longitude")
#from these plots we can see that fare_amount os varying more because of pickup_longitude and dropoff_longitude in comparesion to pickup_latitude and dropoff_latitude.
#______________Feature Engineering_______________#

# 1)year,month,Dayofweek and hour variables using variable pickup_datetime
#train data
train$pickup_datetime= strptime(train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train$year=as.factor(format(train$pickup_datetime,"%Y"))
train$month=as.factor(format(train$pickup_datetime,"%m"))
train$dayofweek = as.factor(format(train$pickup_datetime,"%u"))# Monday = 1
train$Hour=as.factor(format(train$pickup_datetime,"%H"))

#test data
test$pickup_datetime= strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test$year=as.factor(format(test$pickup_datetime,"%Y"))
test$month=as.factor(format(test$pickup_datetime,"%m"))
test$dayofweek = as.factor(format(test$pickup_datetime,"%u"))# Monday = 1
test$Hour=as.factor(format(test$pickup_datetime,"%H"))

#lets check for missing values
sum(is.na(train))
#there is one observation with missing value, lets drop this location
train=train[-which(is.na(train$pickup_datetime)),]
sum(is.na(train))
table(train$year)
table(train$month)
table(train$dayofweek)
table(train$Hour)
#Dropping the variable we used to create features
train = subset(train,select = -c(pickup_datetime))
test = subset(test,select = -c(pickup_datetime))

#Lets plot some visualization to find how the No. of trips is varing with these new features
barplot(table(train$year),xlab = "year", ylab = "No. of Trips", main = "Trips per year")
barplot(table(train$month),xlab = "month", ylab = "No. of Trips",main = "Trips per month")
barplot(table(train$Hour),xlab = "hour",ylab = "No. of Trips", main = "Trips per hour")
barplot(table(train$dayofweek),xlab = "dayofweek",ylab = "No. of Trips", main = "Trips per dayofweek")
#As we can see from plot number of trips are higher from month 1 to 6 and trips are less at late night hours i.e. after 12:00 am till 6:00 am

#Lets plot some visualization to find how the fare_amount is varing with these new features
# A) avg fare_amount with year 
ggplot(train,aes_string(x=train$year, y=train$fare_amount, fill=train$year))+geom_bar(stat = "summary", fun.y="mean")+
  xlab("year")+ylab("avg_fare")+ggtitle("variation in avg_fare with year")
# we can see from the above bar plot that avg_fare is increasing with year
# B) avg fare_amount with month 
ggplot(train,aes_string(x=train$month, y=train$fare_amount, fill=train$month))+geom_bar(stat = "summary", fun.y="mean")+
  xlab("month")+ylab("avg_fare")+ggtitle("variation in avg_fare with month")
# from month 2 to 5 avgfare_amount is higher and it is again higher from month 8 to 12
# c) avg fare_amount with hour 
ggplot(train,aes_string(x=train$Hour, y=train$fare_amount, fill=train$Hour))+geom_bar(stat = "summary", fun.y="mean")+
  xlab("Hour")+ylab("avg_fare")+ggtitle("variation in avg_fare with Hour")
# we can see from plot that fare is higher for rides between 12AM to 5AM and 2PM to 4PM.
# D) avg fare_amount with dayofweek 
ggplot(train,aes_string(x=train$dayofweek, y=train$fare_amount, fill=train$dayofweek))+geom_bar(stat = "summary", fun.y="mean")+
  xlab("dayofweek")+ylab("avg_fare")+ggtitle("variation in avg_fare with dayofweek")
# As we can see there is no such variation in fare_amount on different days of week

# 2) Trip Distance
#train data
for (i in 1: nrow(train)){
  radius=6378.1
  diff_lat_train=(train$dropoff_latitude-train$pickup_latitude)*pi/180
  diff_long_train=(train$dropoff_longitude-train$pickup_longitude)*pi/180
  a_train= (sin(diff_lat_train/2)*sin(diff_lat_train/2))+ 
    (cos((train$pickup_latitude)*pi/180)*cos((train$dropoff_latitude)*pi/180)*
       sin(diff_long_train/2)*sin(diff_long_train/2))
  c_train=2*atan2((a_train^0.5),((1-a_train)^0.5))
  train$trip_distance= radius*c_train
}
dim(train)
train$trip_distance=round(train$trip_distance, digits = 3)
  
#test_data
for (i in 1: nrow(test)){
  radius=6378.1
  diff_lat=(test$dropoff_latitude-test$pickup_latitude)*pi/180
  diff_long=(test$dropoff_longitude-test$pickup_longitude)*pi/180
  a= (sin(diff_lat/2)*sin(diff_lat/2))+ (cos((test$pickup_latitude)*pi/180)*cos((test$dropoff_latitude)*pi/180)*
                                             sin(diff_long/2)*sin(diff_long/2))
  c=2*atan2((a^0.5),((1-a)^0.5))
  test$trip_distance= radius*c
}  
dim(test)
test$trip_distance=round(test$trip_distance, digits = 3)

#Lets see how the trip_distance is distribued
par(mfrow=c(1,1))
plot(density(train$trip_distance),main="Distribution for trip_distance")
summary(train$trip_distance)
#as we can see our trip_distance id left skewed
#lets see how fare_amount is varing with trip_distance by ploting a scatter plot
ggplot(train,aes_string(x=train$trip_distance,y=train$fare_amount))+geom_point(color="blue")+xlab("trip_distance")+
  ylab("fare_amount")+theme_bw()+ggtitle("fare_amount vs trip_distance")
#As we can see most of the time fare_amount is increasing with trip_distance


#_______________Feature Selection_______________#

#Lets plot correlation plot for numeric variables
numeric_index=sapply(train,is.numeric)
numeric_data=train[,numeric_index]
numeric_index
numeric=colnames(numeric_data)
corrgram(train[,numeric],order=F,upper.panel = panel.pie,main="correlation plot")
#From this we can say that no independent numeric variable is higher correlated with any other independent numeric variable

#Lets do Chi-square test for Categorical variables
print(chisq.test(train$passenger_count,train$year))
print(chisq.test(train$passenger_count,train$month))
print(chisq.test(train$passenger_count,train$Hour))
print(chisq.test(train$passenger_count,train$dayofweek))
print(chisq.test(train$year,train$month))
print(chisq.test(train$year,train$Hour))
print(chisq.test(train$year,train$dayofweek))
print(chisq.test(train$month,train$Hour))
print(chisq.test(train$month,train$dayofweek))
print(chisq.test(train$Hour,train$dayofweek))
#we can see there is )passenger_count and month),(year and hour), (year and dayofweek), (month and hour) and 
# (month and dayofweek) have p-value greter than 0.05, we will remove them after anova tes between fare_amount and these variable.

#Lets do Anova test for categorical variable and Target numeric variable
#Hypothesis: (Null Hypothesis: Mean of the target variable same for all class of independent variable
#             Alternate Hypothesis: Mean of target variable is varying with class of independent variable)
# if our p_value is greater than 0.05 we accept our null hypothesis
aov_results = aov(fare_amount ~ passenger_count + Hour +dayofweek + month +year,data = train)
summary(aov_results)
#As we can see variable dayofweek does not affecting our target variable fare_amount significantly, so we can drop this variable

#dropiing dayofweek
train = subset(train,select=-dayofweek)
test = subset(test,select=-dayofweek)

#_______________Feature scaling_______________#

#lets plot distribution cureve for our numeric variable to find out ther distribution is normally distributed or not
par(mfrow=c(2,3))
plot(density(train$pickup_longitude),main="Distribution for pickup_longitude")
plot(density(train$pickup_latitude),main="Distribution for pickup_latitude")
plot(density(train$dropoff_longitude),main="Distribution for dropoff_longitude")
plot(density(train$dropoff_latitude),main="Distribution for dropoff_latitude")
plot(density(train$fare_amount),main="Distribution for fare_amount")
plot(density(train$trip_distance),main="Distribution for trip_distance")

#As we can see all out 5 independent numeric variable are not normally distributed, so we use normalization method for feature scaling
cnames=list("pickup_latitude","pickup_longitude","dropoff_latitude","dropoff_longitude","trip_distance")
df2=train
for (i in cnames){
  print(i)
  train[,i] = (train[,i] - min(train[,i]))/(max(train[,i] - min(train[,i])))
}

for (i in cnames){
  print(i)
  test[,i] = (test[,i] - min(test[,i]))/(max(test[,i] - min(test[,i])))
}


#_______________Dividing in Train and Test data_______________#
train_index=sample(1:nrow(train),0.8*nrow(train))
x_train=train[train_index,]    
x_test=train[-train_index,]


#________________Liner Regression_______________#

#multicollinearity check
vif(x_train[,-1])
vifcor(x_train[,numeric],th=0.9)

#Liner Regression model
lm_model=lm(fare_amount~.,data = x_train)
summary(lm_model)

lm_predictions = predict(lm_model,x_test[,2:10])
regr.eval(x_test[,1],lm_predictions)
#       mae         mse          rmse           mape 
#      3.3125     39.9365       6.3195         0.3671

#_______________Decision Tree_________________#
DT_model = rpart(fare_amount ~ ., data = x_train, method = "anova")
DT_model
rsq.rpart(DT_model)
DT_predictions = predict(DT_model, x_test[,2:10])
regr.eval(x_test[,1],DT_predictions)
#       mae          mse           rmse          mape 
#      2.7335      23.6895        4.8671        0.2727 


#______________Random Forest_______________#
RF_model = randomForest(fare_amount ~.,data=x_train, ntree=300)
#Lets see importance of variable by ploting some visualIZATIONS
par(mfrow=c(1,1))
varImpPlot(RF_model,sort = TRUE)

RF_predictions = predict(RF_model,x_test[,2:10])
R2=cor(RF_predictions, x_test[, 1])^2
regr.eval(x_test[,1],RF_predictions)
#    mae        mse       rmse       mape 
#   2.0719    14.2777    3.7785     0.22859

#Error matrix
#As we can see from above model summaries that maximum value of R-square we can get is 0.826 is in Random forest model and apart from 
# R-square value the value of MAE and RMSE is alos low for this model, Hence we select model we build using Random forest
#prediction on actual test data using Random forest model.
test_copy$fare_amount=predict(RF_model,test)
test_copy[1:10,]
test[1:10,]
#Writting csv file for predicted values
write.csv(test_copy,"test_predicted_R.csv",row.names = F)
