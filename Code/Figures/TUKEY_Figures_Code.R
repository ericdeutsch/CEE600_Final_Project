#Install packages and load them into project
install.packages(c("ggplot2", "ggpubr", "tidyverse", 
                   "broom", "AICcmodavg", 
                   "plotly","multcompView", "dplyr"))

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(dplyr)
library(multcompView)

#read in csv's to list
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

#Change Column name from EVI to dB
names(myfiles)[names(myfiles) == 'EVI'] <- 'dB'


#uncount data. (original c1 ->c3 = frequency, value, variable)
#              (now c1, c2 = value, variable)
new_files <- lapply(myfiles, function(x) {
  names(x) <- c("dB", "Frequency", "Transition")
  tidyr::uncount(x, Frequency)
})

#take the list of histogram and concatenate them
data_c <- do.call("rbind", new_files)
head(data_c)


#remove unneeded data for RAM conservation
rm(myfiles, new_files)

#Conduct ANOVA and subsequent Tukey Test
data_c$EVI<-as.numeric(data_c$dB)
aov_m1<-aov(EVI~Transition, data=data_c)
(TUKEY<-TukeyHSD(aov_m1, "Transition"))
plot(TUKEY)

#Generate letters to indicate distinct value distributions
exp_letters1 <- multcompLetters(TUKEY$Transition[,4])
exp_letters1

library(MASS)
multcompBoxplot(dB~Transition, data=data_c)

# Apply the function on my dataset
LABELS <- generate_label_df(TUKEY , "data$treatment")


# A panel of colors to draw each group with the same color :
my_colors <- c( 
  rgb(143,199,74,maxColorValue = 255),
  rgb(242,104,34,maxColorValue = 255), 
  rgb(111,145,202,maxColorValue = 255)
)

# Draw the basic boxplot
a <- boxplot(data$value ~ data$treatment , ylim=c(min(data$value) , 
                                                  1.1*max(data$value)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="value" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(data$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )
