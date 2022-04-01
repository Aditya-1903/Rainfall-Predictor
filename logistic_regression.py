options(warn=-1)
library(ggplot2)
library(caret)
## Loading required package: lattice

library(mltools)
library(klaR)
## Loading required package: MASS

library(caTools)
library(cowplot)
library(corrplot)
## corrplot 0.92 loaded

library(dplyr)
library(plyr)

df = read.csv("weatherAUS.csv",header=TRUE)
head(df)

colnames(df)

summary(df)

class(df$Date)

df$Date <- as.Date(df$Date)
class(df$Date)

train_df = df[df$Date < "2016-01-01", ]
test_df = df[df$Date >= "2016-01-01", ]
nrow(test_df)

nrow(train_df)

input_cols = colnames(df)[1:length(colnames(df))-1]
length(input_cols)

target_col = 'RainTomorrow'
train_inputs = train_df[input_cols]
train_targets = train_df[target_col]
test_inputs = test_df[input_cols]
test_targets =test_df[target_col]


#Partitioning the dataset columns into numerical and categorical columns.

numeric_cols = select_if(train_inputs,is.numeric)
numeric_cols = colnames(numeric_cols)
categorical_cols = c("Location","WindGustDir","WindDir3pm","WindDir9am","RainToday")
length(numeric_cols)+length(categorical_cols)

summary(train_inputs[,c(numeric_cols)])

head(train_inputs[,c(numeric_cols)])


#Mean Imputation of Multiple Columns

for(i in 1:ncol(train_inputs))
{
    train_inputs[ , i][is.na(train_inputs[ , i])] <- mean(train_inputs[ , i], na.rm = TRUE)
    test_inputs[ , i][is.na(test_inputs[ , i])] <- mean(test_inputs[ , i], na.rm = TRUE)
}


#Min-Max Scaling

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

for(i in numeric_cols)
{
    train_inputs[,i] = range01(train_inputs[,i])
    test_inputs[,i] = range01(test_inputs[,i])
}


#One Hot Encoder function
one_hot_encoding = function(df,columns){
  # create a copy of the original data.frame for not modifying the original
  df = cbind(df)
  # convert the columns to vector in case it is a string
  columns = c(columns)
  # for each variable perform the One hot encoding
  for (column in columns){
    unique_values = sort(unique(df[column])[,column])
    non_reference_values  = unique_values[c(-1)] # the first element is going 
                                                 # to be the reference by default
    for (value in non_reference_values){
      # the new dummy column name
      new_col_name = paste0(column,'_',value)
      # create new dummy column for each value of the non_reference_values
      df[new_col_name] <- with(df, ifelse(df[,column] == value, 1, 0))
    }
    # delete the one hot encoded column
    df[column] = NULL

  }
  return(df)
}


#Encoding train and test inputs with our one hot encoder function.

train_inputs = one_hot_encoding(train_inputs,c(categorical_cols))
test_inputs = one_hot_encoding(test_inputs,c(categorical_cols))
train_inputs$Date = NULL
test_inputs$Date = NULL
colnames(train_inputs)


#Encoding train and test targets with our one hot encoder function.

train_targets = one_hot_encoding(train_targets,c("RainTomorrow"))
test_targets = one_hot_encoding(test_targets,c("RainTomorrow"))
head(train_targets)

train_targets[is.na(train_targets)] = 0
test_targets[is.na(test_targets)] = 0
test_inputs[is.na(test_inputs)] = 0
sum(is.na(train_targets$RainTomorrow_Yes))

sum(is.na(test_targets$RainTomorrow_Yes))

train_inputs$RainTomorrow_Yes = train_targets$RainTomorrow_Yes
test_inputs$RainTomorrow_Yes = test_targets$RainTomorrow_Yes
train_inputs[is.na(train_inputs)] = 0
test_inputs[is.na(test_inputs)] = 0


#Simple logistic regression model
logistic <- glm(train_inputs$RainTomorrow_Yes ~ train_inputs$RainToday_Yes,data=train_inputs,family="binomial")
logistic


#Training our final logistic regression model
logistic <- glm(train_inputs$RainTomorrow_Yes ~ . ,data=train_inputs,family="binomial")
summary(logistic)


#Predicting the train inputs

predicted_data = data.frame(
  probability_of_rain_tmw = logistic$fitted.values,
  rain_tmw = train_inputs$RainTomorrow_Yes
)
predicted_data = predicted_data[order(predicted_data$probability_of_rain_tmw,decreasing=FALSE),]
predicted_data$rank = 1:nrow(predicted_data)
ggplot(data=predicted_data,aes(x=rank,y=probability_of_rain_tmw)) + 
geom_point(aes(color=train_inputs$RainTomorrow_Yes),alpha=1,shape=4,stroke=2) +
xlab("Index") +
ylab("Predicted probability of getting rain")


#Fitting our logistic regression model
predict_reg <- predict(logistic, test_inputs, type = "response")
# Changing probabilities
predict_reg <- ifelse(predict_reg >0.5, 1, 0)
table(test_inputs$RainTomorrow_Yes, predict_reg)


missing_classerr <- mean(predict_reg != test_inputs$RainTomorrow_Yes)
Model Accuracy
print(paste('Accuracy =', 1 - missing_classerr))
