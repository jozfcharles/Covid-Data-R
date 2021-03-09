library(ggplot2)
library(tidyverse)
data=read.csv("covid.csv", header = TRUE)
data

summary(data)


apply(data, 2, function(x) any(is.na(x)))

#colSums(is.na(data))

data$deaths[is.na(data$deaths)]<-median(data$deaths,na.rm=TRUE)
data$deathrate[is.na(data$deathrate)]<-median(data$deathrate,na.rm=TRUE)
data$confirmed_per_100000[is.na(data$confirmed_per_100000)]<-median(data$confirmed_per_100000,na.rm=TRUE)
data$total_population[is.na(data$total_population)]<-median(data$total_population,na.rm=TRUE)


apply(data, 2, function(x) any(is.na(x)))

 

############################################################################################################



scale2 <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
data_scaled <- data %>% mutate_if(is.numeric, scale2)

data_scaled

km <- kmeans(data$deathrate, centers = 3, nstart = 10)
km

centroids <- as_tibble(km$centers, rownames = "cluster")
centroids

  
################################################################################################################

data$classification = ifelse(data$deathrate<=70,'low',
                     ifelse(data$deathrate >71&data$deathrate< 150 ,'medium','high')
                      ) 
                            
par(mfrow=c(1,1))
boxplot(data$deathrate)


data$classification=as.factor(data$classification)
data$NCHS_urbanization=as.factor(data$NCHS_urbanization)
x=data$classification
y=data$confirmed
z=data$NCHS_urbanization

par(mfrow=c(1,1))
plot(data$total_population,data$confirmed,xlab="population",ylab="confirmed cases") 
    
plot(data$NCHS_urbanization,data$total_population,xlab=" county Type",ylab="population")
plot(z,y,xlab=" county Type", ylab="confirmed cases")

plot(x,y,xlab="death rate",ylab="cases")

########################################################
library(caret)
data2= data.frame(data$classification,data$confirmed,data$total_population,data$NCHS_urbanization)
data3= data.frame(data$classification,data$confirmed,data$total_population)
names(data2)

train <- createFolds(data$classification, k = 10)

as.factor(data2$data.classification)


rulesFit <- data2 %>% train(data.classification ~ .,
                            method = "PART",
                            data = .,
                            tuneLength = 5,na.action = na.pass,
                            trControl = trainControl(method = "cv", indexOut = train))
rulesFit
rulesFit$results


library(RWeka)
C45Fit <- data2 %>% train( data.classification~ .,
                           method = "J48",
                           data = .,
                           tuneLength = 5,na.action = na.pass,
                           trControl = trainControl(method = "cv", indexOut = train))
C45Fit
C45Fit$finalModel

randomForestFit <- data3 %>% train(data.classification~ .,
                                   method = "rf",
                                   data = .,
                                   tuneLength = 5,na.action = na.pass,
                                   trControl = trainControl(method = "cv", indexOut = train))
randomForestFit
randomForestFit$finalModel


ctreeFit <- data3 %>% train(data.classification~ .,
                            method = "ctree",
                            data = .,
                            tuneLength = 5,na.action = na.pass,
                            trControl = trainControl(method = "cv", indexOut = train))
ctreeFit
plot(ctreeFit$finalModel)
#########################################################################################
library(caret)
resamps <- resamples(list(
  C45 = C45Fit,
  rules = rulesFit,
  randomForest = randomForestFit
))
resamps
summary(resamps)

difs <- diff(resamps)
difs
summary(difs)

##########################################################################################
library(C50)


vars <- c("NCHS_urbanization","total_population","confirmed")
str(data[, c(vars, "classification")])

set.seed(2020)
in_train <- sample(1:nrow(data), size = 3196)
train_data <- data[ in_train,]
test_data  <- data[-in_train,]
levels(train_data$NCHS_urbanization)[1] = "missing"
str(train_data)
rulemod <- C5.0(x = train_data[,vars], y = train_data$classification, rules = TRUE)

rulemod
summary(rulemod)

predict(rulemod, newdata = train_data[1:20, vars])

####################################################################################

tree_data <- C5.0(x = train_data[, vars], y = train_data$classification, trials = 3)
summary(tree_data)

plot(tree_data)

predict(tree_data, newdata = train_data[1:10, vars], type = "prob")

predict(tree_data, newdata = test_data[1:10, vars], type = "prob")
  








