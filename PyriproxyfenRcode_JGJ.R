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









#Filtering the different data frames to analyze 

Control <- filter(df1, Dose == 0 & HPE == 288)

MCF5 <- filter(df1, HPE == 288 & Strain == "MCF5" & Treatment == "Pyriproxyfen")

#Evaluating the Control samples for the Strains at Dose=0

t.test(Adult ~ Strain, data = Control) #There is no difference between the control groups by Strain
shapiro.test(Control$Adult)

#Now we compare the difference observed for the MCF5 strain

one.way <- aov(Adult ~ Dose, data = MCF5)
summary(one.way)

#Checking homoscedasticity

par(mfrow = c(2,2))
plot(one.way) 

#Checking normality
shapiro.test(MCF5$Adult) #Data is not normally distributed so we shift to nonparametric analysis

#Nonparametric analysis of multiple categories
kruskal.test(Adult ~ Dose, data = MCF5) #Here we are testing a general difference by dose

pairwise.wilcox.test(MCF5$Adult ~ MCF5$Dose, p.adjust.method = "BH") 






Control <- filter(df2, Dose == "0")

Control <- dplyr::select(Control, Dose, Adult, Dead, Rep, Strain)

aggregate(x = Control$Adult, 
          by = list(Control$Strain),
          FUN = sum)

success <- c(76, 78)

trial <- c(80,80)

prop.test(success, trial)




round(prop.table(Control1, margin = 1), 2)

MCF5 <- filter(df1, HPE == 288 & Strain == "MCF5" & Treatment == "Pyriproxyfen")








df2 <- filter(df1, Dose != "0" & HPE == 288 & Treatment ==	"Pyriproxyfen")

df3 <- filter(df1, HPE == 288 & Treatment ==	"Pyriproxyfen")

df2$Dose <- df2$Dose * 10000

df2$Dose <- as.factor(df2$Dose)

myprobit <- glm(Adult ~ Dose + Strain, family = poisson(link = "log"), data = df2)

summary(myprobit)

df1$TotalDead <- df1$Larva_Dead + df1$Pupa_Dead

f <- ggplot(df2, aes(Dose, Adult)) 

f + geom_count() + facet_wrap(~Strain)

f + geom_bar(stat = "identity") + facet_wrap(~Strain)

f + geom_boxplot() + facet_wrap(~Strain)


df3 <- filter(df1, HPE == 288 & Treatment ==	"Pyriproxyfen")

df3$Dose <- df3$Dose * 10000

df3$Dose <- as.factor(df3$Dose)

f <- ggplot(df3, aes(Dose, Adult)) 

f + geom_boxplot() + facet_wrap(~Strain)

Reps <- aov(Adult ~ Dose + Rep, data = df3)

summary(Reps)

Two.way <- aov(Adult ~ Dose + Strain, data = df3)
summary(Two.way)

model.set <- list(Two.way, Reps)
model.names <- c("Two.way", "Reps")

aictab(model.set, modnames= model.names)

#Checking homoscedasticity

par(mfrow = c(2,2))
plot(Two.way)
par(mforw = c(1,1))

#Cheching for normality

shapiro.test(df3$Adult)

#The data is not normally distributed but that is not a surprise, now we select a nonparametric tests 
#to evaluate the differences we will use a independent 2-group Mann-Whitney U test

wilcox.test(Adult ~ Dose, data = df3)

wilcox.test(Adult ~ Strain, data = df3)

#Kruskal Wallis Test One way Anova by Ranks
levels(df3$Dose)

group_by(df3, Strain, Dose) %>%
  summarise(
    count = n(), 
    mean = mean(Adult, na.rm =TRUE),
    sd = sd(Adult, na.rm = TRUE),
    median = median(Adult, na.rm = TRUE),
    IQR = IQR(Adult, na.rm = TRUE)
    
  )

df4 <- filter(df3, Strain == "Liverpool")
df4$Dose <- as.factor(df4$Dose)

KW <- kruskal.test(Adult ~ Dose, data = df4)
summary(KW)

pairwise.wilcox.test(df4$Adult, df4$Dose, p.adjust.method = "BH")





