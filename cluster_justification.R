#Cluster justification
library(cluster)#daisy and silhouete functions
library(readxl) #read_xlsx function
library(tidyverse)
library(magrittr) #for the %<>% operator
library(cluster) 


#1.Prepare data####
d <- read_xlsx("data/database.xlsx", sheet = 3)
d$response[is.na(d$response)] <- "Not assessed"

#d$variable <- as.factor(d$variable)
resp_levels <- c("Remain", "Adapt", 
                 "Transform", "Exit", "Not assessed")
d$response[d$response == "No change"] <- "Remain" 
d$response <- ordered(d$response, levels = resp_levels)

d$variable[d$variable == 15] <- "15%"
d$variable[d$variable == 30] <- "30%"
d$variable[d$variable == 50] <- "50%"
d$variable[d$variable == 70] <- "70%"
d$variable[d$variable == 90] <- "90%"

d$variable <- ordered(d$variable, levels = c("15%", "30%", "50%", "70%", "90%"))
var_levels <- levels(d$variable)


#3.Cluster analysis####
data <- spread(d, variable, response)
m <- data[,-c(1:2)]

#change class of columns
m <- m %<>% lapply(function(x) as.numeric(x))
#The %<>% operator pipes and reassigns
#Now the list apply function is much easier to read,
#by only specifying the function you wish to apply.
m <- as.matrix(as.data.frame(m))
names <- paste("sk", 1:dim(m)[1], sep = "")
rownames(m) <- names
colnames(m) <- c("15%", "30%", "50%", "70%", "90%")


## Silhouette for a hierarchical clustering:
means <- NA
for (i in 2:10) {
  cl <- hclust(daisy(m, metric = "gower"), method = "ward.D2")
  sil_cl <- silhouette(cutree(cl, k = i) , daisy(m, metric = "gower"), title = title(main = 'Good'))
  rownames(sil_cl) <- rownames(m)
  plot(sil_cl, main = "Silhouette plot") #for interpreting: https://www.stat.berkeley.edu/~spector/s133/Clus.html
  
  means[i] <-summary(sil_cl)$avg.width
  
}

d <- as.data.frame(cbind(2:10, means[2:10]))
colnames(d) <- c("number", "mean")
ggplot(d, aes(number, mean)) +
  geom_line() +
  xlab("Number of clusters") +
  ylab("Average Silhouette Width") +
  theme_bw() #+ expand_limits(x = 0, y = 0)
