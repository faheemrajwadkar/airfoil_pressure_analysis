# library(dplyr)
# library(caret)
# library(ggplot2)

# Setting Seed
set.seed(2806)

libs()

# Importing Data
airfoil <- read.csv("airfoil_self_noise.csv", 
                    col.names = c("Freq", "Angle", "Chord", "Velocity", "Thick", "Pres"))

# Data Understanding
summ(airfoil)

# EDA
# Test for multicollinearity using a correlation plot
corr.matrix = cor(airfoil[, -6])
corrplot(corr.matrix, method = "number", mar = c(0, 0, 3, 0),
         title = "Correlation plot to test for Multicollinearity")

# Observations - 
# As can be seen, there is multi collinearity as the two variables - thickness and angle - are
# correlated with r = 0.75. That needs to be dealt with.
 

# Distributions
# Let's first create a theme variable for all ggplot statements
gg_theme <- theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                  axis.title = element_text(size = 10),
                  axis.text = element_text(size = 8))

names(airfoil)

# Now, we will take a look at how each variable is distributed
# Freq
table(airfoil$Freq)
# Total number of unique values
length(unique(airfoil$Freq))

ggplot(data = airfoil, aes(x = Freq)) +
  geom_histogram(fill = "#EC7014", color = "#8C2D04", bins = 20) +
  labs(x = "Frequency", y = "Count", title = "Distribution plot for Frequency") +
  gg_theme

# Observations -
# 1 The frequency has got a predefined set of values so we can try treating them as individual classes. 
# 2 Secondly, we find that the distribution is skewed to the left so we can try taking the logarithmic transformation of this variable (which may prove more beneficial than transforming the numeric variable into categorical)
# Nevertheless, we will try both the ideas after the base model is built

# Angle
table(airfoil$Angle)
# Total number of unique values
length(unique(airfoil$Angle))

ggplot(data = airfoil, aes(x = (log(Angle + 1)))) +
  geom_histogram(fill = "#EC7014", color = "#8C2D04", bins = 20) +
  labs(x = "Angle", y = "Count", title = "Distribution plot for Angle") +
  gg_theme

# Observations -
# 1 The distribution is skewed to the left so we can again deal that with some transformation of the feature
# 2 The feature again assumes one of the predefined set of values - 27 in this case so we can transofrm this variable into a category as well.

# Chord
table(airfoil$Chord)

ggplot(data = airfoil, aes(x = Chord)) +
  geom_histogram(fill = "#EC7014", color = "#8C2D04", bins = 10) +
  labs(x = "Chord Length", y = "Count", title = "Distribution plot for Chord Length") +
  gg_theme


# Observations -
# 1 The distribution is almost uniform in this case so we will try transforming it
# 2 There are 6 unique values in this variable and they are almost uniformly distributed so it may
# be beneficial to actually convert this into a categorical variable 

# Velocity
table(airfoil$Velocity)

ggplot(data = airfoil, aes(x = Velocity)) +
  geom_histogram(fill = "#EC7014", color = "#8C2D04", bins = 4) +
  labs(x = "Velocity", y = "Count", title = "Distribution plot for Velocity") +
  gg_theme

# Observations -
# 1 There are only 4 unique values in this variable and they are almost uniformly distributed so it
# may be beneficial to actually convert this into a categorical variable 

# Thick
table(airfoil$Thick)

ggplot(data = airfoil, aes(x = Thick)) +
  geom_histogram(fill = "#EC7014", color = "#8C2D04", bins = 10) +
  labs(x = "Thickness", y = "Count", title = "Distribution plot for Thickness") +
  gg_theme

# This is a classic skewed distribution and can be easily normalized using a log transformation

# Pres (Target Variable)
summary(airfoil$Pres)

ggplot(data = airfoil, aes(x = Pres)) +
  geom_histogram(fill = "#EC7014", color = "#8C2D04", bins = 20) +
  labs(x = "Pressure", y = "Count", title = "Distribution plot for Pressure") +
  gg_theme

# This is very close to a normal distribution and may not need to be worked or transformed at all

# Let us now split the data into training and testing data
split <- createDataPartition(y = airfoil$Pres, p = 0.7, list = F)
training <- airfoil[split, ]
testing <- airfoil[-split, ]
dim(training)
dim(testing)


# Let's now build the base model
mod_base <- lm(Pres ~ ., data = training)
summary(mod_base)
# 0.53

# That's a terrible base model with r-sq at around 50%

# First Iteration - removing angle and build some models without it
mod_1 <- lm(Pres ~ . - Angle, data = training)
summary(mod_1)
# 0.48

# Second Iteration - log(Freq)
mod_2 <- lm(Pres ~ . + log(Freq) - Angle - Freq, data = training)
summary(mod_2)
# 0.46

# Third iteration - factor(Freq)
mod_3 <- lm(Pres ~ . + factor(Freq) - Angle - Freq, data = training)
summary(mod_3)
# 0.52

# Fourth iteration - factor (Chord)
mod_4 <- lm(Pres ~ . + factor(Freq) + factor(Chord) - Angle - Freq - Chord, data = training)
summary(mod_4)
# 0.53

# Fifth iteration - log(Chord)
mod_5 <- lm(Pres ~ . + factor(Freq) + log(Chord) - Angle - Freq - Chord, data = training)
summary(mod_5)
# 0.51

# Sixth iteration - factor(Velocity)
mod_6 <- lm(Pres ~ . + factor(Freq) + factor(Chord) + factor(Velocity) - Angle - Freq - Chord - Velocity, data = training)
summary(mod_6)
# 0.53

# Seventh iteration - log(Velocity)
mod_7 <- lm(Pres ~ . + factor(Freq) + factor(Chord) + log(Velocity) - Angle - Freq - Chord - Velocity, data = training)
summary(mod_7)
# 0.53

# Eighth iteration - log(Thick)
mod_8 <- lm(Pres ~ . + factor(Freq) + factor(Chord) + log(Velocity) + log(Thick) - ., data = training)
summary(mod_8)
# 0.54

# Ninth iteration - removing Thick instead of angles and building models on them
mod_9 <- lm(Pres ~ . - Thick, data = training)
summary(mod_9)
# 0.47

# Tenth Iteration - log(Freq)
mod_10 <- lm(Pres ~ . + log(Freq) - Thick - Freq, data = training)
summary(mod_10)
# 0.44

# Eleventh iteration - factor(Freq)
mod_11 <- lm(Pres ~ . + factor(Freq) - Thick - Freq, data = training)
summary(mod_11)
# 0.51

# Twelfth iteration - factor (Chord)
mod_12 <- lm(Pres ~ . + factor(Freq) + factor(Chord) - Thick - Freq - Chord, data = training)
summary(mod_12)
# 0.53

# Thirteenth iteration - log(Chord)
mod_13 <- lm(Pres ~ . + factor(Freq) + log(Chord) - Thick - Freq - Chord, data = training)
summary(mod_13)
# 0.53

# Fourteenth iteration - factor(Velocity)
mod_14 <- lm(Pres ~ . + factor(Freq) + factor(Chord) + factor(Velocity) - Thick - Freq - Chord - Velocity, data = training)
summary(mod_14)
# 0.53

# Fourteenth iteration - log(Velocity)
mod_15 <- lm(Pres ~ . + factor(Freq) + factor(Chord) + log(Velocity) - Thick - Freq - Chord - Velocity, data = training)
summary(mod_15)
# 0.53

# Fifteenth iteration - log(Angle)
mod_16 <- lm(Pres ~ . + factor(Freq) + factor(Chord) + log(Velocity) + log(Angle + 1) - ., data = training)
summary(mod_16)
# 0.47

# Clearly, linear regression is not giving us satisfactory results. So, let's now take a look at how a CART model - Random Forest - performs on this data. Note that we can use Decision Trees to create a regression model as well but we would probably be wasting our time doing so as Decision Tree is usually not recommended for Regression due to it not being a model that generalizes well (a very important behavioral property of a good regression model). Anyway, we will still create one decision tree and see how it fares.

# Decision Trees -
mod_tree_1 <- train(Pres ~ ., data = training, method = "rpart")
predictions <- predict(mod_tree_1, data = training[, -6])
rmse_mod_tree_1 <- RMSE(predictions, training$Pres)
rmse_mod_tree_1

# As can be seen, the RMSE is higher than that for all the models greated using Linear Regression
# Let's now create a Random Forest and see how it fares - although the hopes here aren't particularly high either. 

# Random Forest -
mod_rf <- randomForest(Pres ~ ., data = training, ntree = 500)

predictions <- predict(mod_rf, data = training[, -6])
rmse_mod_rf <- RMSE(predictions, training$Pres)
rmse_mod_rf

# let's now get RMSE for all the above linear models as well and then compare with this model to see if it has decreased in the case of Deision Trees.

rmse_mod_base <- RMSE(pred = predict(mod_base, data = training), obs = training$Pres)

# Tip - instead of writing this code 16 more times, I'll use the string library of Python to
# generate them automatically
# 
# Here's the code -
# 
# a = "rmse_mod_base <- RMSE(pred = predict(mod_base, data = training), obs = training$Pres)"
# target = []
# for i in range(16):
#   target.append(a.replace("base", str(i+1)))
#   
# We will use the output of this code for generating RMSEs of all models

rmse_mod_1 <- RMSE(pred = predict(mod_1, data = training), obs = training$Pres)
rmse_mod_2 <- RMSE(pred = predict(mod_2, data = training), obs = training$Pres)
rmse_mod_3 <- RMSE(pred = predict(mod_3, data = training), obs = training$Pres)
rmse_mod_4 <- RMSE(pred = predict(mod_4, data = training), obs = training$Pres)
rmse_mod_5 <- RMSE(pred = predict(mod_5, data = training), obs = training$Pres)
rmse_mod_6 <- RMSE(pred = predict(mod_6, data = training), obs = training$Pres)
rmse_mod_7 <- RMSE(pred = predict(mod_7, data = training), obs = training$Pres)
rmse_mod_8 <- RMSE(pred = predict(mod_8, data = training), obs = training$Pres)
rmse_mod_9 <- RMSE(pred = predict(mod_9, data = training), obs = training$Pres)
rmse_mod_10 <- RMSE(pred = predict(mod_10, data = training), obs = training$Pres)
rmse_mod_11 <- RMSE(pred = predict(mod_11, data = training), obs = training$Pres)
rmse_mod_12 <- RMSE(pred = predict(mod_12, data = training), obs = training$Pres)
rmse_mod_13 <- RMSE(pred = predict(mod_13, data = training), obs = training$Pres)
rmse_mod_14 <- RMSE(pred = predict(mod_14, data = training), obs = training$Pres)
rmse_mod_15 <- RMSE(pred = predict(mod_15, data = training), obs = training$Pres)
rmse_mod_16 <- RMSE(pred = predict(mod_16, data = training), obs = training$Pres)


# Here's a summary of RMSE values for all the models created 
rmse_df <- data.frame(rmse_mod_base, rmse_mod_1, rmse_mod_2, rmse_mod_3, rmse_mod_4, rmse_mod_5, rmse_mod_6, rmse_mod_7, rmse_mod_8, rmse_mod_9, rmse_mod_10, rmse_mod_11, rmse_mod_12, rmse_mod_13, rmse_mod_14, rmse_mod_15, rmse_mod_16, rmse_mod_tree_1, rmse_mod_rf)

rmse_df <- reshape::melt(rmse_df)
class(rmse_df)
rmse_df <- rename(rmse_df, model_name = variable, RMSE = value)
rmse_df

# Sorting them in ascending order
rmse_df <- arrange(rmse_df, RMSE)
rmse_df

# As can be seen Random Forest has proven to be the best model so far. We will now work on it to try and improve the RMSE value further.

# TBC...


