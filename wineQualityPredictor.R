###Vinho Verde Quality Predictor
##Load required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

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
hist(wine_quality)

#observe distributions
sweep(wine_quality,)
ggplot(stack(wine_quality), aes(x = ind, y = values)) +
  geom_boxplot()

#normalize using z-transform and observe
wine_quality_scaled <- wine_quality %>%
  select(-colour) %>%
  scale(.) %>%
  as.data.frame(.)
#plot
ggplot(stack(wine_quality_scaled), aes(x = ind, y = values, colour = ind)) +
  geom_boxplot(show.legend = FALSE)

##histograms of interesting features
#quality (low prevalence of low/high quality wines)
wine_quality %>% ggplot(aes(quality)) +
  geom_histogram()

#Group quality into high, med, and low
wine_quality$quality.lvl <- fct_collapse(as.factor(wine_quality$quality),
                                         low = c("3","4"),
                                         med = c("5","6"),
                                         high = c("7","8","9"))

#density. higher alcohol means lower density
wine_quality %>% ggplot(aes(density, colour = quality.lvl)) +
  geom_density()

#alcohol (long tail)
wine_quality %>% ggplot(aes(alcohol, colour = quality.lvl)) +
  geom_density()

#residual sugar (long tail)
wine_quality %>%
  ggplot(aes(residual.sugar, colour = quality.lvl)) +
  geom_density()

#correlations
correlations <- cor(wine_quality %>% select(-colour))
#plot correlations on heatmap
col<- colorRampPalette(c("blue", "white", "red"))(20) #set heatmap colour palette
heatmap(x = correlations, col = col, symm = TRUE)

#Add box plots by quality on correlated features (alcohol, citric acid, pH, sulphur dioxide)

##Train model
#partition data

#decision tree algorithm

#random forest classification

