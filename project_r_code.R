library(ggplot2)
library(tidyverse)
data=read.csv("covid.csv", header = TRUE)
data

# Removing columns that are not useful for analysis

data = subset(data, select = -c(1,2,3,4,5,9,10))
data
summary(data)

# Checking empty sets

apply(data, 2, function(x) any(is.na(x)))


#colSums(is.na(data))

data$total_population[is.na(data$total_population)]<-median(data$total_population,na.rm=TRUE)
data$confirmed[is.na(data$confirmed)]<-median(data$confirmed,na.rm=TRUE)
data$deathrate[is.na(data$deathrate)]<-median(data$deathrate,na.rm=TRUE)
#data$death[is.na(data$death)]<-median(data$death,na.rm=TRUE)
#data$confirmed_per_100000[is.na(data$confirmed_per_100000)]<-median(data$confirmed_per_100000,na.rm=TRUE)


apply(data, 2, function(x) any(is.na(x)))

############################################################################################################

# Split data
library(caTools)
set.seed(123)
split = sample.split(data$deathrate, SplitRatio = 0.9)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)


###################################################################################################
training_set[4] = scale(training_set[4])
test_set[4] = scale(test_set[4])

range(training_set$deathrate)


training_set$deathrate = ifelse(training_set$deathrate <= -1,'low',
                       ifelse(training_set$deathrate >-1&training_set$deathrate< 1 ,'medium','high')
                       )

test_set$deathrate = ifelse(test_set$deathrate <= -1,'low',
                       ifelse(test_set$deathrate >-1&test_set$deathrate< 1 ,'medium','high')
)

summary(training_set)

# converting variable into factor variable

training_set$deathrate=as.factor(training_set$deathrate)
training_set$deathrate=as.factor(training_set$deathrate)
training_set$NCHS_urbanization=as.factor(training_set$NCHS_urbanization)
training_set$NCHS_urbanization=as.factor(training_set$NCHS_urbanization)

library(rpart)
classifier = rpart(formula = deathrate ~ .,
                   data = training_set)

y_pred = predict(classifier, newdata = test_set[-4], type = 'class')

cm = table(test_set[, 4], y_pred)











