# Remove all objects from R
rm(list = ls())

#set current working directory
setwd("D:/Edwisor/project/Bike-Rental-Prediction-master")

#Current working directory
getwd()

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", 
      "e1071", "Information","MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Install  Require libraries
install.packages(c("caret", "randomForest", "unbalanced", "C50", "dummies", 
                   "e1071", "Information","gbm", "ROSE", "sampling", "DataCombine", "inTrees" ))




# Load other Required libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")


# read the csv file for analysis
df_data = read.csv("day.csv", header = T) 

# changing the column names (#hr column not present)

colnames(df_data) = c('Record index' , 'Date' , 'Season' , 'Year' , 'Month'  , 'Holiday' , 
                      'Weekday' , 'Working Day' , 'WeatherSituation' , 'Temperature' , 'Atemperature' , 'Humidity' , 'Windspeed' , 'CasualUsers' , 'RegisteredUsers' , 'Count')
colnames(df_data)

#Verify first five rows of data
head(df_data)


#Our target variable is 'Count' which is a dependent variable rest all the variabales are Independent variables

#Verify  structure of data
str(df_data)

# this shows that variables like season, year, month, holiday, weekday, Working day, Weather situation, casual users, registered users are integers.
# and temperature, atemperature, humidity, windspeed are numeric 
# four variables 'month',holiday','weekday','weathersit' have to convert to factor

# conversion of variables into factor.
df_data$Month = as.factor(as.character(df_data$Month))
df_data$Holiday = as.factor(as.character(df_data$Holiday))
df_data$Weekday = as.factor(as.character(df_data$Weekday))
df_data$WeatherSituation = as.factor(as.character(df_data$WeatherSituation))
df_data$Season = as.factor(as.character(df_data$Season))
df_data$Year = as.factor(as.character(df_data$Year))
df_data$`Working Day` = as.factor(as.character(df_data$`Working Day`))
df_data$`Record index` = as.factor(as.character(df_data$`Record index`))


#######################carrying out univaiate analysis#####################################

# function to create univariate distribution of numeric  variables
univariate_numeric <- function(num_x) {
  
  
  ggplot(df_data)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
}

# analyze the distribution of  target variable 'Count'
univariate_numeric(df_data$Count)

# analyse the distrubution of  independence variable 'Temperature'
univariate_numeric(df_data$Temperature)

# analyse the distrubution of  independence variable 'Atemperature'
univariate_numeric(df_data$Atemperature)

# analyse the distrubution of  independence variable 'Humidity'
univariate_numeric(df_data$Humidity)

# analyse the distrubution of  independence variable 'Windspeed'
univariate_numeric(df_data$Windspeed)

# analyse the distrubution of  independence variable 'Casual Users'
univariate_numeric(df_data$CasualUsers)

# analyse the distrubution of  independence variable 'Registered Users'
univariate_numeric(df_data$RegisteredUsers)

# Visualize categorical Variable 'Month' with target variable 'Count'

ggplot(df_data, aes(x=Month, y=Count),fill="grey") + 
stat_summary(fun.y="mean", geom="bar")

# Visualize categorical Variable 'Holiday' 

ggplot(df_data) +
geom_bar(aes(x=Holiday),fill="green")

# it is showing that almost all the  cycle rentals are happening  on holidays

# Visualize categorical Variable 'weekday' 

ggplot(df_data) +
geom_bar(aes(x=Weekday),fill="grey") 

# it is showing  counts are same on all weekdays
# Visualize categorical Variable 'weathersit' 

ggplot(df_data) +
geom_bar(aes(x=WeatherSituation),fill="blue") 

# Above graph shows count is more when  whether is " Clear, Few clouds, Partly cloudy, 
#Partly cloudy"


#######################carring out Bivariate Analysis##################################
#check the relationship between 'Temperatue' and 'Atemperature' variable

ggplot(df_data, aes(x=Temperature,y=Atemperature)) +
  geom_point()+
  geom_smooth()

#This  graph shows a very strong correlation between "Temperature and Atemperature".

#check the relationship between 'Temperature' and 'Humidity' variable

ggplot(df_data, aes(x= Temperature,y=Humidity)) +
  geom_point()+
  geom_smooth()

#it shows  Humidity increases  till temparature is 0.7 and it is decreasing  gradually

#check the relationship between 'Temperature' and 'Windspeed' variable

ggplot(df_data, aes(x= Temperature,y=Windspeed)) +
  geom_point()+
  geom_smooth()

#it shows that very less negative   correlation between  Temperature and Windspeed

#check the relationship between all numeric variable using pair plot

ggpairs(df_data[,c('Atemperature','Temperature','Humidity','Windspeed','Count')])

# this plot shows that there is less +ve correlation between Count-Humidity and less -ve corelation between  Windspeed and Count
#and there is strong positive relationship between Temperature- Count and  Atemperature-Count

# Realtionship of our target variable Count with other categoical variables
# Count and Season

relation_cs= table(df_data$Count,df_data$Season)
barplot(relation_cs)

# this shows that almost equal Count in every Season

# Count and Weekday

relation_cw = table(df_data$Count,df_data$Weekday)
barplot(relation_cw)

#Count is almost same on all Weekdays.

#Count vs WeatherSituation

relation_cws = table(df_data$Count,df_data$WeatherSituation)
relation_cws
barplot(relation_cws)

#It shows that the count is maximum in WeatherSituation:1 followed by WeatherSituation:2 and then WeatherSituation:3 ; no rides in season 

#Count and Holiday

relation_ch = table(df_data$Count,df_data$Holiday)
relation_ch

barplot(relation_ch)

# it shows that maximum rides occur on holiday:0 


##########################Missing Values Analysis#############################################

missing_val = data.frame(apply(df_data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

# there are no missing values in the data.
#Hence no need of any analysis we will proceed with outlier process.



########################### Outlier Analysis ###################################
#we will start with the outlier analysis only on numerical variables.
numeric_index = sapply(df_data,is.numeric) #selecting only numeric
numeric_data = df_data[,numeric_index]
cnames = colnames(numeric_data)

#Creating loop for boxplot of the numercial variables.

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Count"), data = subset(df_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Count")+
           ggtitle(paste("Box plot of count for",cnames[i])))
  }
   

### Plotting plots together 
 gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
 gridExtra::grid.arrange(gn6,gn7,ncol=2)
 gridExtra::grid.arrange(gn3,gn4,ncol=2)

 

 # # #loop to remove from all variables
 for(i in cnames){
 print(i)
  val = df_data[,i][df_data[,i] %in% boxplot.stats(df_data[,i])$out]
  print(length(val))
  df_data = df_data[which(!df_data[,i] %in% val),]
 }


 ##################################Feature Selection################################################
 
 ## Correlation Plot 
 
 corrgram(df_data[,numeric_index], order = F,
   upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
 
 # shows high dependence of registered users with count and temperature with  atemperature.
 # since count is the target variable we will accept both registered user and count but out of temperature and  atemperature we can select any one of them.
 # and there is no  relationship between 'humidity' and 'count' therefore we will drop Humidity as well 
 
 
  ## Chi-squared Test of Independence
 
 factor_index = sapply(df_data,is.factor)
 
 factor_data = df_data[,factor_index]

 for (i in 0:9) 
 {
   print(names(factor_data)[i])
   print(chisq.test(table(factor_data$Count,factor_data[,i])))
 }
 
 
 # this shows an error because the target variable here is not categorical variable it is a continuous variable.
 
 ## even when we convert the count to categorical using 
 #df_data$`Count` = as.factor(as.character(df_data$`Count`))
 ## all the p values are greater than 0.05 therefore we neglect the chi square test.
 
 #  dimensional  reduction
 
 df_data_deleted = subset(df_data,select=-c(Atemperature,Humidity)) # removed humidity and A temperature from our model.
 
 ##################################Feature Scaling################################################
 
 # since the Temperature and Windspeed is alread normalized we need to scale only Causal user and Registerd Users
 
 #Normality check
 qqnorm(df_data_deleted$CasualUsers)
 hist(df_data_deleted$CasualUsers)
 
 qqnorm(df_data_deleted$RegisteredUsers)
 hist(df_data_deleted$RegisteredUsers)
 
 cnames_1 = c("CasualUsers","RegisteredUsers")
 
 for(i in cnames_1){
   print(i)
   df_data_deleted[,i] = (df_data_deleted[,i] - min(df_data_deleted[,i]))/
     (max(df_data_deleted[,i] - min(df_data_deleted[,i])))
 }
 # we need not to go for standardization as our variables are not normally distributed.
 df_data_deleted$RegisteredUsers
 df_data_deleted$CasualUsers
 ###################################Model Development#######################################
 
 #Divide data into train and test using stratified sampling method
 
 set.seed(1234)
 library(caret)
 train.index = createDataPartition(df_data_deleted$Count, p = .80, list = FALSE)
 train = df_data_deleted[ train.index,]
 test  = df_data_deleted[-train.index,]
 
 train_feature = train[,c("Season" ,"Year" ,"Month" ,"Holiday","Weekday","Working Day","WeatherSituation","Temperature","Windspeed","CasualUsers","RegisteredUsers","Count")]
 
 train_feature
 

 test_features = test[,c("Season" ,"Year" ,"Month" ,"Holiday","Weekday","Working Day","WeatherSituation","Temperature","Windspeed","CasualUsers","RegisteredUsers","Count")]

 
 ##  develop Decision tree model 
 
 # ##rpart for regression
 fit = rpart(Count ~ ., data = train_feature, method = "anova")
 
 #Predict for new test cases
 predictions_DT = predict(fit, test_features[,-12])
 
 print(fit)
 #  plotting decision tree
 
 
 plot(fit)
 text(fit) 
 
 # Evaluation of Decision tree algoithm
 #MAPE
 #calculate MAPE
 MAPE = function(y, yhat){
   mean(abs((y - yhat)/y))
 }
 
 MAPE(test_features[,12], predictions_DT)
 
 #Error Rate: 0.1474599
 #Accuracy: 85.25%
 
 ###Evaluate  Model using RMSE
 
 RMSE <- function(y_test,y_predict) {
   
   difference = y_test - y_predict
   root_mean_square = sqrt(mean(difference^2))
   return(root_mean_square)
   
 }
 
 
 RMSE(test_features[,12], predictions_DT)
 
 #RMSE = 637.1391
#####################################################################################################

 
 
 
 ###develop Random Forest model 
 
 Rental_rf=randomForest(Count ~ . , data = train_feature)
 
 RF_cnt
 
 plot(RF_cnt)
 
 #Predict for new test cases
 predictions_rf = predict( RF_cnt , test_features[,-12])
 
 ##MAPE 
 #calculate MAPE
 MAPE(test_features[,12], predictions_DT_two)
 
 #Error Rate: 0.078
 #Accuracy: 92.2
 
 ###Evaluate  Model using RMSE
 
 RMSE(test_features[,12], predictions_DT_two)
 
 #RMSE = 270
 
 ########################################################################################################
 

 
 #### Develop  Linear Regression Model 
 
 #check multicollearity
 install.packages('usdm')
 library(usdm)
 vif(train_feature[,-12])
 
 vifcor(train_feature[,-12], th = 0.9)
 # Correleation between two variables is 'Season' and 'Month' is 0.82 so, removing one variable from the model
 
 
 
 train_feature_1 = train[,c("Year" ,"Month" ,"Holiday","Weekday","Working Day","WeatherSituation",
                            "Temperature","Windspeed","CasualUsers","RegisteredUsers","Count")]
 
 
 
 
 test_features_1 = test[,c("Year" ,"Month" ,"Holiday","Weekday","Working Day","WeatherSituation",
                           "Temperature","Windspeed","CasualUsers","RegisteredUsers","Count")]

 # Linear Regression  model
 #run regression model
 lm_model = lm(Count ~., data = train_feature_1)
 
 #Summary of the model
 summary(lm_model)
 
 # Predict  the Test data 
 #Predict
 predictions_LR = predict(lm_model, test_features_1[,-11])
 
 # Evaluate Linear Regression Model
 
 
 MAPE(test_features_1[,11], predictions_LR)
 
 #Error Rate: 1.054427e-16
 #Accuracy: 99.9 + accuracy
 
 RMSE(test_features_1[,11], predictions_LR)
 
 #RMSE = 5.104411e-13
 
 
 # COnclusion  For this Dataset  Linear Regression is  Accuracy  is '99.9'
 # and RMSE = 5.104411e-13
 
 ##### THE BEST MODEL FOR THIS ANALYSIS IS LINEAR REGRESSION #######################
 
 