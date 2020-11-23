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

#remove unneeded data for RAM conservation
rm(myfiles, new_files)

#Conduct ANOVA and subsequent Tukey Test
data_c$EVI<-as.numeric(data_c$dB)
aov_m1<-aov(EVI~Transition, data=data_c)
(TUKEY<-TukeyHSD(aov_m1, "Transition"))

#Generate letters to indicate distinct value distributions
exp_letters1 <- multcompLetters(TUKEY$Transition[,4])
exp_letters1