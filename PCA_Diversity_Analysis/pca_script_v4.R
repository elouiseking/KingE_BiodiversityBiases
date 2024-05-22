#Principal Component Analysis of Review Data
#Need to perform a PCA of both Literature data and EIAR data seperately
#Then measure the Euclidean distance between the points to see how similar/dissimilar they are
#v4
#1 - Set up & Load Packages
getwd()
library(ggplot2)
library(dplyr)
library(vegan)
library(stats)
library(gridExtra)

# EIAR PCA #

eiar_data <- read.csv("eiar_matrix_v2.csv")

#PCA using prcomp()
#we don't scale data because it is proportion data -> scaling would be inappropriate
eiar_pca <- prcomp(eiar_data[, -1])
summary(eiar_pca)
print(eiar_pca)
biplot(eiar_pca)

#plot pc1 & pc2 
plot(eiar_pca$x[,1], eiar_pca$x[,2])

#make a scree plot
eiar_pca_var <- eiar_pca$sdev^2
eiar_pca_var_per <- round(eiar_pca_var/sum(eiar_pca_var)*100, 1)

barplot(eiar_pca_var_per, main = "Scree Plot", xlab = "Principal Component", ylab = "Percent Variation")

# plot the PCA
eiar_scores <- as.data.frame(eiar_pca$x[,1:2])
colnames(eiar_scores) <- c("PC1", "PC2")

eiar_plot <- data.frame(PC1 = eiar_scores$PC1,
                        PC2 = eiar_scores$PC2,
                        Year = eiar_data$year)

ggplot(data= eiar_plot, aes(x = PC1, y = PC2, label = Year)) +
  geom_text() +
  xlab(paste("PC1 - ", eiar_pca_var_per[1], "%", sep = "")) +
  ylab(paste("PC2 - ", eiar_pca_var_per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("PCA Plot")

# LIT PCA #
# read in the data
lit_data <- read.csv("literature_matrix_v3.csv")

#PCA using prcomp()
lit_pca <- prcomp(lit_data[, -1])
summary(lit_pca)
print(lit_pca)
biplot(lit_pca)

#plot pc1 and pc2
plot(lit_pca$x[,1], lit_pca$x[,2])

#make a scree plot
lit_pca_var <- lit_pca$sdev^2
lit_pca_var_per <- round(lit_pca_var/sum(lit_pca_var)*100,1)

barplot(lit_pca_var_per, main = "Scree Plot", xlab = "Principal Component", ylab = "Percent Variation" )

#plot the pca
lit_scores <-as.data.frame(lit_pca$x[,1:2])
colnames(lit_scores) <- c("PC1", "PC2")

lit_plot <- data.frame(PC1 = lit_scores$PC1,
                       PC2 = lit_scores$PC2,
                       Year = lit_data$year)

ggplot(data= lit_plot, aes(x = PC1, y = PC2, label = Year)) +
  geom_text() +
  xlab(paste("PC1 - ", lit_pca_var_per[1], "%", sep = "")) +
  ylab(paste("PC2 - ", lit_pca_var_per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("PCA Plot")

#the top three pcas make up almost 86% of the data - so going to add the third PC to the plot
lit_scores_3pc <-as.data.frame(lit_pca$x[,1:3])
colnames(lit_scores_3pc) <- c("PC1", "PC2", "PC3")

lit_plot_3pc <- data.frame(PC1 = lit_scores_3pc$PC1,
                           PC2 = lit_scores_3pc$PC2,
                           PC3 = lit_scores_3pc$PC3,
                           Year = lit_data$year)

ggplot(data= lit_plot_3pc, aes(x = PC1, y = PC2, label = Year)) +
  geom_text() +
  xlab(paste("PC1 - ", eiar_pca_var_per[1], "%", sep = "")) +
  ylab(paste("PC2 - ", eiar_pca_var_per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("PCA Plot")

#Calculate Euclidean distance between the two PCAs for EIARs & Lit
eiar_points <- eiar_pca$x
lit_points <- lit_pca$x

euclidean_distances <- sqrt(rowSums((lit_scores-eiar_scores))^2)

euclidean_distances_years <- data.frame(Distance = euclidean_distances,
                                        Year = eiar_data$year)

#visualise the data:
distance_data <- data.frame(
  Distance = euclidean_distances,
  Year = eiar_data$year)

ggplot(data = distance_data, aes(x = Year, y = Distance, label = Year)) +
  geom_text() +
  xlab("Year") +
  ylab("Euclidean Distance") +
  ggtitle("Euclidean Distance")

ggplot(data = distance_data, aes(x = Year, y = Distance, label = Year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Year") +
  ylab("Euclidean Distance") +
  ggtitle("Euclidean Distance")

#run a linear regression model to assess whether there is a significant linear trend in the Euclidean distances over time

m1 <- lm(Distance ~ Year, data = distance_data)
summary(m1)

#plotting euclidean distances
ggplot(data = distance_data, aes(x = Year, y = Distance)) +
  geom_line(alpha =1, size = 1, color = "#636363") +
  geom_point(size = 2) +
  xlab("Year") +
  ylab("Euclidean Distance") +
  ggtitle("Euclidean Distance") 

ggplot(data = distance_data, aes(x = Year, y = Distance)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "#636363") +
  xlab("Year") +
  ylab("Euclidean Distance") +
  ggtitle("Euclidean Distance")

ggplot(data = distance_data, aes(x = Year, y = Distance, label = Year)) +
  geom_text() +
  xlab("Year") +
  ylab("Euclidean Distance") +
  ggtitle("Euclidean Distance") 

ed_plot <- ggplot(data = distance_data, aes(x = Year, y = Distance)) +
  geom_point(size = 2) +
  xlab("Year") +
  ylab("Euclidean Distance") +
  theme_bw() +
  ggtitle("Euclidean Distance") 

#ggsave("euclidean_distance_scatterplot.png", plot = ed_plot, dpi = 1000)

bar_plot <- ggplot(data = distance_data, aes(x = Year, y = Distance)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Euclidean Distance") +
  theme_bw() +
  ggtitle("Euclidean Distance")

smooth_ed_plot <- ggplot(data = distance_data, aes(x = Year, y = Distance)) +
  geom_smooth(color = "#39393A", fill = "#A6A8AD") +
  geom_point(size = 2) +
  labs(x = "Year",
       y = "Euclidean Distance") +
  theme_light() +
  theme(axis.text.x = element_text(size = 14, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),   
        axis.line = element_line(color = "black"),   
        plot.title = element_text(size = 16, face = "bold", color = "black")) +
  ggtitle("(a)")

#ggsave("smooth_ed_plot.png", plot = smooth_ed_plot, dpi = 1000)


