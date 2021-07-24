###Vinho Verde Quality Predictor
##Load required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)


##Wrangle wine quality data

#data located at the following URL
#http://www3.dsi.uminho.pt/pcortez/wine/winequality.zip

#download zip file
dl <- tempfile()
download.file("http://www3.dsi.uminho.pt/pcortez/wine/winequality.zip", dl)

#read csv files
reds <- read.csv(unzip(dl,"winequality/winequality-red.csv"), sep = ";")
whites <- read.csv(unzip(dl,"winequality/winequality-white.csv"), sep = ";")

#add categorical data
reds <- reds %>% mutate(colour = as.factor("red"))
whites <- whites %>% mutate(colour = as.factor("white"))

#merge data into one dataframe
wine_quality <- bind_rows(reds, whites)

#remove temp files
remove(dl, reds, whites)

##Explore data
#summary stats
summary(wine_quality)
hist(wine_quality)

#correlations
correlations <- cor(wine_quality %>% select(-c(quality,colour)))
#plot correlations on heatmap
col<- colorRampPalette(c("blue", "white", "red"))(20) #set heatmap colour palette
heatmap(x = correlations, col = col, symm = TRUE)


