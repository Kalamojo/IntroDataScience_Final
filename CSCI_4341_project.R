# CSCI 4341 - Intro to Data Science 
# Project EDA

library(tidyquery)
library(stats)
library(dplyr)
library(ggplot2)
library(gmodels)
library(car)
library(caret)

setwd("C:/Users/nowre/Desktop/Spring 2022 Classes/CSCI 4341 - Intro to Data Science/project")

games <- read.csv("games_of_all_time_original.csv", header = TRUE, na.string = c("", "NA"))

quantile(games$meta_score)
w <- 2 * (80 - 63) / (length(games$meta_score)^(1/3))
nb <- ceiling((max(games$meta_score) - min(games$meta_score))/w)
p <- ggplot(data = games, aes(x = meta_score)) + 
     geom_histogram(aes(y = ..density..), color = "black", fill = "steelblue2", bins = nb) +
     geom_density(color = "black", fill = "red", alpha = 0.5) +
     labs(
       title = "Histogram of Meta Score with Density",
       x = "Meta Score",
       y = "Density"
     )
p

quantile(games$user_score)
w2 <- 2 * (79 - 63) / (length(games$user_score)^(1/3))
nb2 <- ceiling((max(games$user_score) - min(games$user_score))/w)
p2 <- ggplot(data = games, aes(x = user_score)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "darkseagreen2", bins = nb) +
  geom_density(color = "black", fill = "red", alpha = 0.5) +
  labs(
    title = "Histogram of User Score with Density",
    x = "User Score",
    y = "Density"
  )
p2

games$meta_score <- (games$meta_score - min(games$meta_score)) / 
                    (max(games$meta_score) - min(games$meta_score))

games$user_score <- (games$user_score - min(games$user_score)) / 
                    (max(games$user_score) - min(games$user_score))

cor(games$user_score, games$meta_score, method = "pearson")

# model <- lm(meta_score ~ user_score, data = games)
# plot(games$user_score, games$meta_score, pch = 16, col = "blue")
# abline(model)
# summary(model)
# plot(model$residuals, pch = 16, col = "red")

model2 <- lm(user_score ~ meta_score, data = games)
plot(games$meta_score, games$user_score, xlab = "Meta Score", ylab = "User Score",
     pch = 16, col = "blue", main = "Scatter Plot with Regression Line of Meta Score verus User Score")
abline(model2, col = "red", lw = 2)
summary(model2)
plot(model2$residuals, pch = 16, col = "red", main = "Resudial Plot from the Model")
qqPlot(model2$residuals)
rhist <- ggplot(data = as.data.frame(model2$residuals), aes(x = model2$residuals)) + 
         geom_histogram(aes(y = ..density..), color = "black", fill = "yellow") +
         geom_density(color = "black", fill = "red", alpha = 0.5) +
         labs(
           title = "Histogram of Residuals from the Model",
           x = "Residuals",
           y = "Density"
         )
rhist