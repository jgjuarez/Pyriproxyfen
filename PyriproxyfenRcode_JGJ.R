library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(drc)
library(lmtest)
library(sandwich)
library(multcomp)
library(ggplot2)

df1 <- read_excel("Pyriproxyfen_V1.1_JGJ_20210305.xlsx", 
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "date", "numeric", 
                                                                  "text"))
df2 <- dplyr::filter(df1, HPE==288)

df1$TotalDead <- df1$Larva_Dead + df1$Pupa_Dead

ggplot(df1, aes(x = HPE, y= Pupa_Dead)) + geom_point()


ggplot(df1, aes(x = HPE, y= Larva_Dead)) + geom_point()

ggplot(df1, aes(x = HPE, y= TotalDead)) + geom_point()
