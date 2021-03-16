#Sectionof the analysis for pyriproxyfen
library(patchwork)
library(MASS)
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(drc)
library(lmtest)
library(sandwich)
library(multcomp)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(epiDisplay)

df1 <- read_excel("Pyriproxyfen_V1.1_JGJ_20210305.xlsx", 
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "date", "numeric", 
                                                                  "text"))

#Lets plot the data of the Adult emergence comparing them by Strain
emer <- filter(df1, HPE == 288 & Treatment ==	"Pyriproxyfen") #here we are selecting for final emergence count and Pyriproxyfen

f <- ggplot(emer, aes(Dose, Adult)) 

f + geom_boxplot() + facet_wrap(~Strain)

#Analyzing the the count data of Adult emergence from the pyriproxyfen
#We need to add the total number of dead mosquitoes to the data frame

df1$Dead <- 20 - df1$Adult

df1$Dose <- df1$Dose * 10000

df1$Dose <- as.factor(df1$Dose)

df2 <- filter(df1, HPE == 288 & Treatment ==	"Pyriproxyfen") # We are filtering the data since we only need to know what happend for the adult emergence for the pyriproxyfen

xtabs(Adult ~ Strain + Dose, data = df2) #this shows us how the data should be presented

#Now we need to expand however many counts for each tabulation into a data frame

df2.edit <- dplyr::select(df2, Dose, Adult, Strain) # lets get rid of the other variables we dont need.

df2.expand <- lapply(df2.edit, function(x) rep(x, df2.edit$Adult)) #apply the number of counts

df2.expand.df <- as.data.frame(df2.expand) #let's ensure it's a data frame

df2.new <- df2.expand.df[,-2] #Now let's remove the counts

with(df2.new, table(Strain, Dose)) #we should have ended up with the original table form xtable

#To add the margin counts

addmargins(with(df2.new, table(Strain, Dose)))

#Lets calculate the row and column percentages

with(df2.new, tabpct(Strain, Dose, graph = FALSE)) #tabpct function if from the package epiDisplay

#Let's calculate the mean and standard deviation of the variable Strain for each level of the variable Dose

df2.new$newcase <- as.numeric(df2.new$Strain) - 1

aggregate(df2.new$newcase, by = list(df2.new$Dose), FUN = mean)

aggregate(df2.new$newcase, by = list(df2.new$Dose), FUN = sd)

#Chi-Squared tests of General Association

df3 <- filter(df2.new, Dose ==	0.0125 |  Dose == 0 |  Dose == 0.0063) #We remove the doses that have 0

with(df3, chisq.test(table(Strain, Dose))) #Due to the nature of the dataset this just gives us an error

#Now we proceed to evaluate each dose by its proportion by comparing the Strains

Dose1 <- c(76, 78)

trial1 <- c(80,80)

prop.test(Dose1, trial1)

Dose2 <- c(0, 15)

trial2 <- c(80,80)

prop.test(Dose2, trial2)

Dose3 <- c(2, 61)

trial3 <- c(80,80)

prop.test(Dose3, trial3)

#Lets plot this with a different perspective
f <- ggplot(emer, aes(Dose, Adult, color = Strain)) 

f + geom_boxplot()