
library(ggplot2)
library(tidyverse)
data=read.csv("E:/ufv/431.r/covid.csv", header = TRUE)
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

par(mfrow=c(2,2))
plot(data$total_population,data$confirmed,xlab="population",ylab="confirmed cases") 
    
plot(data$NCHS_urbanization,data$total_population,xlab=" county Type",ylab="population")
plot(z,y,xlab=" county Type", ylab="confirmed cases")

plot(x,y,xlab="infection rate",ylab="cases")

########################################################
install.packages("C50")

library(C50)


vars <- c("NCHS_urbanization","total_population","deathrate")
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


##########################################################################

tree_data <- C5.0(x = train_data[, vars], y = train_data$classification, trials = 3)
summary(tree_data)

plot(tree_data)

predict(tree_data, newdata = train_data[1:10, vars], type = "prob")

predict(tree_data, newdata = test_data[1:10, vars], type = "prob")

############################################################################

resamps <- resamples(list(rulemod=rulesFit,tree_data=ctreeFit))
resamps
summary(resamps)

difs <- diff(resamps)
difs
summary(difs)








































