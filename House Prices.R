library(caret)
library(tidyverse)
library(skimr)
library(ggplot2)
library(GGally)

df <- read.csv('housing(1).csv')
skim(df)

summary(df)

df$chas <- as.factor(df$chas)

# outliers detection

boxplot(df$crim, xlab = 'df$crim')
boxplot(df$zn, xlab = 'df$zn')
boxplot(df$rm, xlab = 'df$rm')
boxplot(df$dis, xlab = 'df$dis')
boxplot(df$medv, xlab = 'df$medv')


boxplot(df$medv ~ df$chas)



histogram(df$medv, main = "Histogram of House Prices", xlab = "House Price", col = "lightblue")
labs(title = "Median House Price")

boxplot(df$medv, main = "Boxplot of House Prices", xlab = "median value of price ", col = "lightblue", border = "black")
text(x = 1, y = median(df$medv), labels = paste("Median =", median(df$medv)), pos = 1, offset = 2, col = "red")


# the density plot of house age

hist(df$age, prob=T,
     main="Histogram of Density of House Age",
     xlab="House Age")
lines(density(df$age), lwd=2) #lwd means line width

# Add a vertical line that indicates the average
abline(v=mean(df$age),
       lty=2,
       lwd=1.5) #lty means line type

# scatter plot 

ggpairs(df, columns = c(1:3, 5:13)
) +
  theme(legend.position = "bottom") 

# Regression model steps

# step 1 :

set.seed(99)
index <- createDataPartition(df$medv, p = .8,list = FALSE)
df_train <-df[index,]
df_test <- df[-index,]

# step 2 :

housing_model <- train(medv ~ .,
                       data = df_train,
                       method = 'lm',
                       trControl =trainControl(method = "none"))
#Get results                             
summary(housing_model)

# step 3

medv_pred<-predict(housing_model, df_test)

# step 4

MSE <- mean((medv_pred - df_test$medv)^2)



