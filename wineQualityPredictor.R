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

#calculate correlations between features
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

#Plot density plots by colour across features. We expect colour to be important to classify.
wine_quality %>%
  pivot_longer(cols = 1:12) %>%
  ggplot(aes(value, colour = colour)) +
  facet_wrap(~ name, scales = "free") +
  geom_density()

#Plot density plots by assigned quality level across features for red wine
wine_quality %>%
  select(-quality) %>%
  filter(colour == "red") %>%
  pivot_longer(cols = 1:11) %>%
  ggplot(aes(value, colour = quality.lvl)) +
  facet_wrap(~ name, scales = "free") +
  geom_density() +
  ggtitle("Red Wines")

#Plot density plots by assigned quality level across features for white wine
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
set.seed(1, kind = "Rounding")
#create data partition index to split 20% of the edx set into a test set and 80% into a training set
train_index <- createDataPartition(y = wine_quality$quality.lvl,
                                   p = 0.8,
                                   list = FALSE)

#create partitioned data sets using index
train_set <- wine_quality[train_index,] %>% select(-quality) #remove quality as a predictor
test_set <- wine_quality[-train_index,] %>% select(-quality) #remove quality as a predictor

#fit decision tree algorithm. Using Kappa because of the low prevelance of high and low quality wines
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

#create weighted decision tree model
fit_dt_weighted <- train(quality.lvl ~ .,
                         method = "rpart",
                         metric = "Kappa",
                         weights = weights_dt,
                         data = train_set)

#plot decision tree created with weighted observations
plot(fit_dt_weighted$finalModel, margin = 0.1)
text(fit_dt_weighted$finalModel, cex = 0.75)

#print results of cross validation
fit_dt_weighted

##Evaluate on test set
#compute modeled predictions
pred_dt_weighted <- predict(fit_dt_weighted,test_set)

#calculate confusion matrix


#print confusion matrix for model evaluation
cm_dt_weighted <- confusionMatrix(pred_dt_weighted, test_set$quality.lvl)
cm_dt_weighted[["byClass"]][ , "F1"]

#random forest classification


#tune with cross validation

#varImp

