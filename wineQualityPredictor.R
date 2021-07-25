###Vinho Verde Quality Predictor
##Load required packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(Rborist)) install.packages("Rborist")

library(tidyverse)
library(caret)
library(Rborist)


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
#partition data

#decision tree algorithm

#random forest classification

