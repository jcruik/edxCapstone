###Vinho Verde Quality Predictor

##Wrangle wine quality data

#data located at the following url
#http://www3.dsi.uminho.pt/pcortez/wine/winequality.zip

#download zip file
dl <- tempfile()
download.file("http://www3.dsi.uminho.pt/pcortez/wine/winequality.zip", dl)

#read csv files
reds <- read.csv(unzip(dl,"winequality/winequality-red.csv"), sep = ";")
whites <- read.csv(unzip(dl,"winequality/winequality-white.csv"), sep = ";")

#add catagorical data
reds <- reds %>% mutate(colour = as.factor("red"))
whites <- whites %>% mutate(colour = as.factor("white"))

#merge data into one dataframe
wine_quality <- bind_rows(reds, whites)