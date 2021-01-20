#### Description: this script produces manuscript associations 
#### between variables. GLM and Fisher test. and SI Figure2
#### INPUT: "database.xlsx" and "clusters.csv" 
#### OUTPUT: associations between variables and SI Figure2
#### Date: 13/05/2020
#### Author: Iratxe Rubio
###############################################################

library(readxl) #read_xlsx function
library(tidyverse)

#1.Read response variable####
response <- as.data.frame(read.csv("data/clusters.csv")[,-2])
colnames(response) <- "cluster"
response$cluster[response$cluster == 2] <- 0
response$cluster <- factor(response$cluster, levels = c(0,1))

#2.Read independent variables####
indep_vars <- read_xlsx("data/database.xlsx", sheet = 1)[,-1]

#3.Association analysis####
#Dependence among 1 binomial and 1 categorical variable. Fisher test
#create table with all variables and p_values
fisher_vars <- c("age", "town", "ocean", "change", "adapt_new", "sampling_strategy")
fisher_data <- indep_vars[,fisher_vars]
fisher_p <- data.frame(fisher_vars)
fisher_p$p_value <- NA

for (i in 1:length(fisher_vars)) {#Loop to fill the table with results
  f <- fisher.test(table(response$cluster, as.matrix(fisher_data[,i])))[["p.value"]]
  fisher_p[i,2] <- round(f, 3)
}

fisher_p #Table with p-values

#Dependence among 1 binomial and 1 numeric/ordinal variable. Glm
glm_data <- indep_vars[,!names(indep_vars) %in% fisher_vars]
glm_vars <- colnames(glm_data)
glm_p <- data.frame(glm_vars)
glm_p$p_value <- NA
data <- cbind(response, glm_data)

for (i in 1:length(glm_vars)) {#Loop to fill the table with results
  mod <- as.formula(sprintf("cluster ~ %s", glm_vars[i]))
  m <- glm(mod, family = "binomial", data = data)
  a <- summary(m)
  if(length(a[["coefficients"]]) == 4){
    glm_p[i,2] <- NA
    next
  }
  glm_p[i,2] <- round(a[["coefficients"]][[8]],3)
}

#bind results
colnames(fisher_p)[1] <- "var"
colnames(glm_p)[1] <- "var"
p_results <- rbind(glm_p, fisher_p)

#Significant variables. GLM results
#binomial versus 3numeric(years_job, intergen_knowledge and importance_abundance), glm
data <- cbind(response, indep_vars)

m <- glm(cluster ~ year_job, family = "binomial", data = data)
summary(m)
plot(response$cluster, indep_vars$year_job, xlab = " ", ylab = "number of years in current job")
response_num <- as.data.frame(as.numeric(as.character(response$cluster)))
colnames(response_num) <- "cluster"
cor.test(response_num$cluster, indep_vars$year_job)

m <- glm(cluster ~ intergen_knowld, family = "binomial", data = data)
summary(m)
plot(response$cluster, indep_vars$intergen_knowld, xlab = "", ylab = "Inter. knowledge importance")
cor.test(response_num$cluster, indep_vars$intergen_knowld)

m <- glm(cluster ~ change_abundance, family = "binomial", data = data)
summary(m)
plot(response$cluster, indep_vars$change_abundance, xlab = "", ylab = "Perception of abundance changes")
cor.test(response_num$cluster, indep_vars$change_abundance)


#4.Create and save SI Figure 2####
var_ls <- read_xlsx("data/database.xlsx", sheet = 4) 
colnames(var_ls) <- c("var", "cinner")
var_ls2 <- left_join(var_ls, p_results, by = "var")
var_ls2$sig <- "no"  
var_ls2$sig[var_ls2$p_value < 0.05] <- "yes"

var_ls3 <- full_join(var_ls, p_results, by = "var")

var_ls2$cinner[var_ls2$cinner == "Social organization"] <- "Social org."
var_ls2$cinner[var_ls2$cinner == "Socio-cognitive"] <- "Soco-cog."

jpeg("SI_Figure2.jpeg", 
    width = 6, height = 6, units = 'in', res = 300)
ggplot(var_ls2, aes(cinner, var, color = sig)) +
  geom_point(size = 4) +
  xlab("") +
  ylab("Variable (SI Table3)")+
  labs(color = "Significant?")+
  theme_bw()
dev.off()