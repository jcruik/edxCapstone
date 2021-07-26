###Vinho Verde Quality Predictor
##Load required packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(rpart)) install.packages("rpart")
if(!require(randomForest)) install.packages("randomForest")

library(tidyverse)
library(caret)
library(rpart)
library(randomForest)

##Wrangle wine quality data
##data located here: http://www3.dsi.uminho.pt/pcortez/wine/winequality.zip

#download zip file
dl <- tempfile()
download.file("http://www3.dsi.uminho.pt/pcortez/wine/winequality.zip", dl)

#read csv files
reds <- read.csv(unzip(dl,"winequality/winequality-red.csv"), sep = ";")
whites <- read.csv(unzip(dl,"winequality/winequality-white.csv"), sep = ";")

#add categorical data
reds <- reds %>% mutate(colour = as.factor("red"))
whites <- whites %>% mutate(colour = as.factor("white"))

#merge data
wine_quality <- bind_rows(reds, whites)

#remove temp files
remove(dl, reds, whites)

##Explore data
#summary stats
summary(wine_quality)

#observe distributions
wine_quality %>%
  keep(is.numeric) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  facet_wrap(~ name, scales = "free") +
  geom_histogram()

#quality (low prevalence of low/high quality wines)
#total sulphur bimodal
#density uniform
#long tails on many

#calculate correlations between predictors
correlations <- cor(wine_quality %>% keep(is.numeric))

#plot correlations on heatmap
col<- colorRampPalette(c("blue", "white", "red"))(20) #set heatmap colour palette
heatmap(x = correlations, col = col, symm = TRUE)

#alcohol, density, and volatile.acid appear most correlated with quality

#Group quality into high, med, and low
wine_quality$quality.lvl <- fct_collapse(as.factor(wine_quality$quality),
                                         low = c("3","4"),
                                         med = c("5","6"),
                                         high = c("7","8","9"))

#Plot density plots by colour across predictors. We expect colour to be important to classify.
wine_quality %>%
  pivot_longer(cols = 1:12) %>%
  ggplot(aes(value, colour = colour)) +
  facet_wrap(~ name, scales = "free") +
  geom_density()

#Plot density plots by assigned quality level across predictors for red wine
wine_quality %>%
  select(-quality) %>%
  filter(colour == "red") %>%
  pivot_longer(cols = 1:11) %>%
  ggplot(aes(value, colour = quality.lvl)) +
  facet_wrap(~ name, scales = "free") +
  geom_density() +
  ggtitle("Red Wines")

#Plot density plots by assigned quality level across predictors for white wine
wine_quality %>%
  select(-quality) %>%
  filter(colour == "white") %>%
  pivot_longer(cols = 1:11) %>%
  ggplot(aes(value, colour = quality.lvl)) +
  facet_wrap(~ name, scales = "free") +
  geom_density() +
  ggtitle("White Wines")

#density. higher alcohol means lower density
#alcohol (long tail)
#residual sugar (long tail)

##Train model
#repeatable randomness
set.seed(1, sample.kind = "Rounding")
#create data partition index to split 20% of the edx set into a test set and 80% into a training set
train_index <- createDataPartition(y = wine_quality$quality.lvl,
                                   p = 0.8,
                                   list = FALSE)

#create partitioned data sets using index
train_set <- wine_quality[train_index,] %>% select(-quality) #remove quality as a predictor
test_set <- wine_quality[-train_index,] %>% select(-quality) #remove quality as a predictor

#fit decision tree algorithm. Using Kappa because of the low prevalence of high and low quality wines
fit_dt <- train(quality.lvl ~ .,
                method = "rpart",
                metric = "Kappa",
                data = train_set)

#plot the decision tree
plot(fit_dt$finalModel, margin = 0.1)
text(fit_dt$finalModel, cex = 0.75)

#not identifying low quality wines is a problem for a winemaker. increasing weights of low quality wines based on their prevelance
positiveWeight = 1.0 / (nrow(subset(train_set, quality.lvl == "low")) / nrow(train_set))
negativeWeight = 1.0 / (nrow(subset(train_set, quality.lvl %in% c("med", "high"))) / nrow(train_set))

#create weighting index
weights_dt <- ifelse(train_set$quality.lvl == "low", positiveWeight, negativeWeight)

#create weighted decision tree model with many complexity parameters
fit_dt_weighted <- train(quality.lvl ~ .,
                         method = "rpart",
                         metric = "Kappa",
                         tuneGrid = data.frame(cp = seq(0.02, 0.05, len = 10)),
                         weights = weights_dt,
                         data = train_set)

#plot decision tree created with weighted observations
plot(fit_dt_weighted$finalModel, margin = 0.1)
text(fit_dt_weighted$finalModel, cex = 0.75)

#print results of cross validation
fit_dt_weighted
ggplot(fit_dt_weighted)

##Evaluate on test set
#compute modeled predictions
pred_dt_weighted <- predict(fit_dt_weighted,test_set)

#print confusion matrix for model evaluation
cm_dt_weighted <- confusionMatrix(pred_dt_weighted, test_set$quality.lvl)
cm_dt_weighted[["byClass"]][ , "F1"]

#weighted model might be more useful to a winemaker looking to avoid a poor product, where the non-weighted model would be better for a high quality producer looking for their best wine

##Use random forest to overcome the inflexibility of a single tree
#set number of trees to reduce computation time by 80%
fit_rf <- train(quality.lvl ~ .,
                method = "rf",
                metric = "Kappa",
                ntree = 100,
                data = train_set)

#plot the tuning of mtry parameter
ggplot(fit_rf)

#plot decision tree created with weighted observations
plot(fit_rf$finalModel)

#print results of cross validation
fit_rf

##Evaluate on test set
#compute modeled predictions
pred_rf <- predict(fit_rf,test_set)

#print confusion matrix for model evaluation
cm_rf_weighted <- confusionMatrix(pred_rf, test_set$quality.lvl)
cm_rf_weighted[["byClass"]][ , "F1"]

#plot variable importance of predictors
ggplot(varImp(fit_rf))

#here we see the most correlated factors having the largest importance within the model
       