##--------------------------- Lib Initialization----------------------------##
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(gridExtra)
library(grid)
library(glmnet)
library(ggplot2)
library(glmnet)
library(Rmisc)
library(lubridate)
library(dplyr)
library(geosphere)
library(lubridate)
library(Metrics)
install.packages("lubridate") #to handle date objects
install.packages("stringi") #lubridate dependency
install.packages("dplyr") #for mutate function
install.packages("geosphere") #for Geographical distance
install.packages("ggplot2") #for better graph plots
install.packages("Metrics") #RMSLE function
install.packages("Rmisc") #multiplot function
install.packages("grid")

##--------------------------Input Data Files--------------------------------##
#Check names of input data files
list.files("../IDS575_Project/inputdata")

#read training data
train<-read.csv("../IDS575_Project/inputdata/train.csv")
#read testing data
test<-read.csv("../IDS575_Project/inputdata/test.csv")

#check for missing values
colSums(is.na(train))
colSums(is.na(test))
#--no missing values in train or test--#


##-------------------Exploratory Data Analysis---------------------------##
#analyze summary and structure of the data 
summary(train)
str(train)
str(test)

#graphs : visualizing the data

#trip_durations distribution | identify outliers
ggplot(data=train, aes(x= trip_duration)) +
geom_histogram(bins = 150,fill="salmon") +
scale_x_log10() +
scale_y_sqrt() +
theme_bw() +
theme(axis.title = element_text(size=16),axis.text = element_text(size=14),
      plot.title = element_text(hjust = 0.5)) +
labs(x = 'Trip Duration', y = 'Count', title = 'Trip Duration Distribution') 

#passenger_count distribution |  identify outliers
ggplot(data=train, aes(x=passenger_count)) +
  geom_bar(fill="salmon") +
   scale_y_log10() +
   theme_bw() +
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = 'Passenger Count', y = 'Count', title = 'Passenger Count Distribution') 


#trips over the year
p1 <- train %>%
  ggplot(aes(ymd_hms(pickup_datetime)) )+
  geom_histogram(fill = "salmon", bins = 120) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Pickup dates",title="Pickups & Dropoffs over the year")

p2 <- train %>%
   ggplot(aes(ymd_hms(dropoff_datetime))) +
   geom_histogram(fill = "#00BFC4", bins = 120) +
  labs(x = "Dropoff dates")

layout <- matrix(c(1,2),2,1,byrow=FALSE)
multiplot(p1, p2, layout=layout)

#plots to explore temporal distribution
#plotting pickups v/s hour of the week
p3 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick) %>%
  count() %>%
  ggplot(aes(hpick, n, color ="salmon" )) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

#plotting pickups v/s day of the week
p4 <- train %>%
  mutate(wday = wday(pickup_datetime, label = T)) %>%
  group_by(wday) %>%
  count() %>%
  ggplot(aes(wday, n, colour = "salmon")) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")


layout <- matrix(c(1,2),2,1,byrow=FALSE)
multiplot(p3, p4, layout=layout)

##--------------------Data Cleaning & Preprocessing------------------------##

#--Removing outliers--

#outliers in trip durations: trip_duration>24hours
train= subset(train,train$trip_duration < (60*60*24) )
#outliers in no. of passengers : passenger < 1
train= subset(train,train$passenger_count >0)


#--Creating useful features--

#extract temporal and spatial data features
train <- train %>%
mutate(pickup_week_day = wday(pickup_datetime, label = T),
      pickup_month = month(pickup_datetime, label = T),
       pickup_hour = hour(pickup_datetime),
       pickup_date = as_date(pickup_datetime),
      vendor_id=factor(vendor_id)
      )

#calculate distance between pickup&dropoff coordinates 
#we use distGeo function which is the closest to GPS distance (in meters)
pickup_coord <- as.matrix(data.frame(train$pickup_longitude, train$pickup_latitude))
dropoff_coord <- as.matrix(data.frame(train$dropoff_longitude, train$dropoff_latitude))
#we usedefault values for WGS84 system
train$trip_distance<-distGeo(pickup_coord,dropoff_coord) 

#plot of trip_duration v/s distance
ggplot(train,aes(x=trip_distance,y=trip_duration))+
  geom_point(color="#00BFC4")+
  scale_y_log10() +
  scale_x_log10() +
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  xlab("Distance")+
  ylab("Duration")


# Creating same features on the Test set

test <- test %>%
  mutate(pickup_week_day = wday(pickup_datetime, label = T),
         pickup_month = month(pickup_datetime, label = T),
         pickup_hour = hour(pickup_datetime),
         pickup_date = as_date(pickup_datetime),
         vendor_id=factor(vendor_id)
  )
pickup_coord_test <- as.matrix(data.frame(test$pickup_longitude, test$pickup_latitude))
dropoff_coord_test <- as.matrix(data.frame(test$dropoff_longitude, test$dropoff_latitude))
test$trip_distance<-distGeo(pickup_coord_test,dropoff_coord_test) 


##-----------------Model Building & Prediction------------------------##

##--Split 50% training data into validation set--##

set.seed(3)
split <- sample(1:nrow(train), nrow(train) / 2)
train.split <- train[split , ]
validn.split <- train[-split, ]


##--Least square regression model--##

#Build model on training set
train.lm= lm(trip_duration~vendor_id+passenger_count+
               store_and_fwd_flag+pickup_week_day+
               pickup_month+pickup_hour+
               pickup_date+trip_distance,data=train.split)
#Fit model on validation set
predict.lm<-predict(train.lm,validn.split)
#validation error obtained from OLS
ols_rmsle<-round(rmsle(validn.split$trip_duration,predict.lm),digits=6)

##--LASSO regression model--##

#Build model on training set
#Parameters chosen: 
#s=0.01 as lambda
#alpha=1 is the default LASSO penalty
x.train<-model.matrix(trip_duration~vendor_id+passenger_count+
                        store_and_fwd_flag+pickup_week_day+
                        pickup_month+pickup_hour+
                        pickup_date+trip_distance,train.split)

x.valid<-model.matrix(trip_duration~vendor_id+passenger_count+
                       store_and_fwd_flag+pickup_week_day+
                       pickup_month+pickup_hour+
                       pickup_date+trip_distance, validn.split)
y<-train.split$trip_duration
train.lasso<-glmnet(x.train,y,alpha=1) 

#Fit model on validation set
predict.lasso<-predict(train.lasso,s=0.01,x.valid)

#validation error obtained from LASSO
lasso_rmsle<-round(rmsle(validn.split$trip_duration,predict.lasso),digits=6)

##--Ridge regression model--##

#Build model on training set
#Parameters chosen: 
#s=0.01 as lambda
#alpha=0 is the default ridge penalty
train.ridge<-glmnet(x.train,y,alpha=0) 

#Fit model on validation set
predict.ridge<-predict(train.ridge,s=0.01,x.valid)

#validation error obtained from LASSO
ridge_rmsle<-round(rmsle(validn.split$trip_duration,predict.ridge),digits=6)

#Display the three regression methods v/s their RMSLEs
Reg.method<-c("OLS", "LASSO","Ridge")
Reg.mse<-matrix(c(ols_rmsle,lasso_rmsle,ridge_rmsle),ncol=1,byrow = T)
rownames(Reg.mse)<-c("OLS", "LASSO","Ridge")
colnames(Reg.mse)<-c("Validation RMSLE")
rmsle.table<-as.table(Reg.mse)
rmsle.table

#plot of the table
t <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[2:5], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)),
  rowhead=list(fg_params=list(col="salmon", fontface=4L)))
grid.table(Reg.mse,theme=t)


##--------------------Prediction on test data--------------------------------##
#Fit model on test data set using ols model
ols_test<-predict(train.lm,test)
test_prediction<- data.frame(id = test$id,
                              trip_duration = ols_test)
write.csv(test_prediction, "test_prediction.csv", row.names=T)
##--------------------------End of Code--------------------------------##
