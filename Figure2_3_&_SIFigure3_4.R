#### Description: this script produces manuscript Figures2 and 3 
#### "clusters.csv" used in script "Associations", Kruskal-Wallis 
#### test and cluster number choice justification-SI Figure 3&4
#### INPUT: "database.xlsx" 
#### OUTPUT: "clusters.csv" and manuscript Figures2 and 3
#### Date: 25/05/2020
#### Author: Iratxe Rubio
################################################################

library(readxl) #read_xlsx function
library(tidyverse)
library(magrittr) #for the %<>% operator
library(ComplexHeatmap) #Heatmap function
library(cluster) #daisy and silhouette functions
library(dendextend) #color_branches function
library(RColorBrewer)#brewer.pal

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

#Treat oceans together?
kruskal.test(response ~ ocean, d) #p>0.05 non-significant differences between groups.

num <- 1
l <- list()
for (i in var_levels) {
  database <- filter(d, variable == i) %>%
              group_by(variable, response) %>%
              summarise(n = n())
  database$variable <- i
  
  #code in https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
  # Compute percentages
  database$fraction <- database$n / sum(database$n)
  
  # Compute the cumulative percentages (top of each rectangle)
  database$ymax <- cumsum(database$fraction)
  
  # Compute the cumulative percentages (top of each rectangle)
  database$ymax <- cumsum(database$fraction)
  
  # Compute the bottom of each rectangle
  database$ymin <- c(0, head(database$ymax, n=-1))
  
  # Compute label position
  database$labelPosition <- (database$ymax + database$ymin) / 2
  
  # Compute a good label
  database$label <- database$n
  
  l[[num]] <- database
  
  num <- num + 1
}

database <- do.call(rbind, l)
database$response <- factor(database$response, levels = resp_levels)

#2.Save Figure2####
jpeg("Figure2.jpeg", 
    width = 12, height = 4, units = 'in', res = 300)
ggplot(database, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = response)) +
  geom_rect() +
  geom_text(x = 3.5, aes(y = labelPosition, label = label), size = 5) +
  scale_fill_brewer(palette = 4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(strip.text = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)) +
  guides(fill = guide_legend(title = "Response:")) +
  facet_wrap(~variable, ncol = 5)
dev.off()



#3.Cluster analysis####
data <- spread(d, variable, response)
m <- data[,-c(1:2)]

#change class of columns
m <- m %<>% lapply(function(x) as.numeric(x))
#The %<>% operator pipes and reassigns
#Now the list apply function is much easier to read,
#by only specifying the function you wish to apply.
m <- as.matrix(as.data.frame(m))
names <- paste("Skipper", 1:dim(m)[1], sep = "")
rownames(m) <- names
colnames(m) <- c("15%", "30%", "50%", "70%", "90%")

#Clustering
meth <- "ward.D2"

dend_r <- hclust(daisy(m, metric = "gower"), method = meth)
dend_r <- color_branches(dend_r, k = 2, col = c("plum4", "plum")) #OR 3


#dend_c <- hclust(daisy(t(m), metric = "gower"), method = meth)
dend_r <- set(dend_r, "branches_lwd", 3)

myP <- brewer.pal(n = 5, name = 'GnBu')

#4.Save Figure3####
jpeg("Figure3.jpeg", 
    width = 8, height = 6, units = 'in', res = 600)
h <- Heatmap(m, name = "mat", 
        cluster_columns = F, #dend_c,
        cluster_rows = dend_r, 
        row_split = 2,
        #column_dend_reorder = c(1:5),
        col = myP,
        heatmap_legend_param = list(legend_direction = "horizontal",
                                    labels = c(resp_levels), 
                                    title = "Response",
                                    labels_gp = gpar(fontsize = 15),
                                    title_gp = gpar(fontsize = 16),
                                    border = "black"),
        column_names_gp = gpar(fontsize = 13),
        row_names_gp = gpar(fontsize = 12),
        column_names_rot = 360, column_names_centered = T,
        column_title = 'Hypothetical cenarios',
        column_title_side = "bottom",
        row_names_side = "left"
        )
draw(h, heatmap_legend_side = "right")
dev.off()

#5.Create "cluster" variable####
#add cluster to the data and save it to posteriorly use the data in the network analysis
# Cut tree into 2 groups
sub_grp <- cutree(hclust(daisy(m, metric = "gower"), method = meth), k = 2)

m2 <- as.data.frame(m)
m2 <- m2 %>%
  mutate(cluster = sub_grp)

#skipper names!
m2$skipper <- c(1:29)#n = 29
d$skipper <- rep(1:29, each = 5)#n = 29
skips <- d %>%
          group_by(ID, skipper) %>%
          summarise(n = n())
m3 <- left_join(m2, skips, by = "skipper")

write.csv(m3[, c(6,8)], "data/clusters.csv", row.names = F)


#6. Silhouette for clustering####
means <- NA
for (i in 2:10) {
  cl <- hclust(daisy(m, metric = "gower"), method = "ward.D2")
  sil_cl <- silhouette(cutree(cl, k = i) , daisy(m, metric = "gower"), title = title(main = 'Good'))
  rownames(sil_cl) <- rownames(m)
  plot(sil_cl, main = "Silhouette plot")
  means[i] <-summary(sil_cl)$avg.width
}

d <- as.data.frame(cbind(2:10, means[2:10]))
colnames(d) <- c("number", "mean")

ggplot(d, aes(number, mean)) +
  geom_line() +
  xlab("Number of clusters") +
  ylab("Average Silhouette Width") +
  theme_bw() #+ expand_limits(x = 0, y = 0)