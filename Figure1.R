#### Description: this script produces manuscript Figure1,
#### Kruskal-Wallis test and mean(SD) of adaptation actions per skipper
#### INPUT: "database.xlsx" 
#### OUTPUT: manuscript Figure1
#### Date: 27/05/20
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(tidyverse)

#1.Prepare data####
original_data <- read_xlsx("data/database.xlsx", sheet = 1)[,c(1,7)]
adapt<- read_xlsx("data/database.xlsx", sheet = 2)

d <- left_join(adapt, original_data, by = "ID")

#Treat oceans together?
kruskal.test(change_react ~ ocean, d) #p>0.05 non-significant differences between groups.

#combine "some"other" adaptation actions together
d$change_react[d$change_react %in% c(8,9,11,12)] <- 9 #9"Other adaptations"
#8"Adapting to new regulations"
#9"Changing effort"
#11"Adjusting costs"
#12"Sustainable and selective fishing"

d$change_react <- as.factor(as.character(d$change_react))

levels(d$change_react) <- c("Fishing more frequently", #1
                            "No change", #10
                            "Fishing period change", #2
                            "Searching new ports", #3
                            "Fishing area expansion", #5
                            "Using new tech to search tunas", #6
                            #"Fishing 'unusual species'", #7
                            "Other adaptations" #9
                            ) 
#summary(d$change_react)

#number of adaptation actions per skipper
tbl <- d %>% 
        group_by(ID) %>%
        summarise(total = n())
round(mean(tbl$total),0)
round(sd(tbl$total),0)

#group levels
total <- d %>% 
          group_by(change_react) %>% 
          summarise(total = n(),
                    perc = round((total*100)/29, 0))####change sample size!!!

total$change_react <- factor(total$change_react, levels = total$change_react[order(total$perc)])

#2.Save Figure1####
jpeg("Figure1.jpeg", 
    width = 11, height = 6, units = 'in', res = 300)
ggplot(total, aes(x = change_react,
                 y = perc)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("% of skippers") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 23),
        legend.key.height = unit(0.7, "cm"),
        axis.text = element_text(size = 23, color = "black"),
        axis.title = element_text(size = 23),
        strip.text = element_text(size = 23),
        plot.title = element_text(size = 23)) 
dev.off()