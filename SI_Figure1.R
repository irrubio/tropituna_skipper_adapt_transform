#### Description: this script produces SI Figure 1 
#### INPUT: "database.xlsx" 
#### OUTPUT: SI Figure 1
#### Date: 20/07/2020
#### Author: Iratxe Rubio
###############################################################

library(readxl) #read_xlsx function
library(tidyverse) 
source("function_multiplot.R")

#1.Open data####
d <- read_xlsx("data/database.xlsx", sheet = 1)[,c(2,7)]

#2.Prepare data and plots####
#age
d$age <- as.factor((d$age))
levels(d$age) <- c("35-44", "45-54", "55-64")
p1 <- ggplot(d, aes(age)) +
        geom_bar() +
        xlab("Age") +
        ylab("Number of skippers") +
        theme(axis.text = element_text(size = 20),
              axis.title = element_text (size = 20))

#ocean
d$ocean <- as.factor((d$ocean))
p2 <- ggplot(d, aes(ocean)) +
        geom_bar() +
        xlab("Ocean") +
        ylab("Number of skippers") +
        theme(axis.text = element_text(size = 20),
              axis.title = element_text (size = 20))

#3.Save SI Figure1####
jpeg("SI_Figure1.jpeg", 
    width = 12, height = 4, units = 'in', res = 300)
multiplot(p1, p2, cols = 2)
dev.off()
