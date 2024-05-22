#Alluvial plots
#v1

#1 - Set up
getwd()

#load packages
library(ggplot2)
library(dplyr)
library(ggalluvial)
library(RColorBrewer)
library(ggrepel)
library(ggfittext)
library(alluvial)
library(cowplot)

#2 - Load in the data
lit_taxa <- read.csv("literature_taxa_v3.csv", header = T)
lit_year_impacts <- read.csv("literature_taxa_year_categories_v3.csv", header = T)

eiar_surveys <- read.csv("eiar_data_long.csv", header = T)

#3 - Check the structure and change to categorical variables
#(for the year category data)
#lit_year_impacts$year_cat <- as.factor(lit_year_impacts$year_cat)
lit_year_impacts$year_cat <- factor(lit_year_impacts$year_cat, levels = rev(unique(lit_year_impacts$year_cat)))
lit_year_impacts$Year <- as.factor(lit_year_impacts$Year)
lit_year_impacts$Taxa <- as.factor(lit_year_impacts$Taxa)
lit_year_impacts$Impact <- as.factor(lit_year_impacts$Impact)
lit_year_impacts$Effect <- as.factor(lit_year_impacts$Effect)
str(lit_year_impacts)

#eiar_surveys$Year_Cat <- as.factor(eiar_surveys$Year_Cat)
eiar_surveys$Year_Cat <- factor(eiar_surveys$Year_Cat, levels = rev(unique(eiar_surveys$Year_Cat)))
eiar_surveys$Year <- as.factor(eiar_surveys$Year)
eiar_surveys$Taxa <- as.factor(eiar_surveys$Taxa)
eiar_surveys$Effort <- as.factor(eiar_surveys$Effort)
str(eiar_surveys)

#ordering for grouping



#4 - Literature alluvial plot
#Year Categories
lit_year_taxa_by_effect <- ggplot(data = lit_year_impacts,
                                 aes(axis1 = year_cat,   # First variable on the X-axis
                                     axis2 = Taxa, 
                                     y = Papers)) +
  geom_alluvium(aes(fill = Effect, group = Effect), curve_type = "cubic", width = 0.5) +
  geom_stratum(width = 1/8) +
  # geom_text(stat = "stratum",
  #aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Years", "Taxa"),
                   expand = c(0.15, 0.2)) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme_bw()+
  theme(legend.position = "bottom") 

#fixing the text problems:
lit_year_taxa_by_effect <- lit_year_taxa_by_effect +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 4, direction = "y", nudge_x = -.6) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x)  == 2, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 4, direction = "y", nudge_x = .6
  ) +
  scale_fill_brewer(type = "qual", palette = "Dark2", name = "Impact", 
                    labels=c("Trophic Level Alteration", "Behavioural Change", "Collision",
                             "Habitat Alteration", "Increased Stress", "Proliferation of Invasive Spp.", 
                             "Altered Reproductive Output"))


#5 - EIAR alluvial plot
eiar_alluvial <- ggplot(data = eiar_surveys,
                        aes(axis1 = Year_Cat,   # First variable on the X-axis
                            axis2 = Taxa, 
                            y= Frequency)) +
  geom_alluvium(aes(fill = Effort)) +
  geom_stratum(width = 1/8) +
  # geom_text(stat = "stratum",
  #aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Years", "Taxa"),
                   expand = c(0.15, 0.2)) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme_bw()+
  theme(legend.position = "bottom") 

#fixing the text problem
eiar_alluvial <- eiar_alluvial +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 4, direction = "y", nudge_x = -.6) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x)  == 2, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 4, direction = "y", nudge_x = .6
  ) +
  scale_fill_brewer(type = "qual", palette = "Dark2", name = "Nature of survey",
                    labels=c("Fieldbased", "Opportunistic", "Scoping")) 

#6 Combining into one plot
#removing taxa labels
#literature
lit_year_taxa_by_effect <- ggplot(data = lit_year_impacts,
                                  aes(axis1 = year_cat,   # First variable on the X-axis
                                      axis2 = Taxa, 
                                      y = Papers)) +
  geom_alluvium(aes(fill = Effect, curve_type = "cubic")) +
  geom_stratum(width = 1/8) +
  # geom_text(stat = "stratum",
  #aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Years", "Taxa"),
                   expand = c(0.15, 0.2)) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme_bw()+
  theme(legend.position = "right") 

lit_year_taxa_by_effect <- lit_year_taxa_by_effect +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 4, direction = "y", nudge_x = -.6) +
 
  scale_fill_brewer(type = "qual", palette = "Dark2", name = "Impact",
                    labels=c("Trophic level alteration", "Behavioural change", "Collision",
                             "Habitat alteration", "Increased stress", "Proliferation of invasive species", "Altered reproductive output"))

#eiar
eiar_alluvial <- ggplot(data = eiar_surveys,
                        aes(axis1 = Year_Cat,   # First variable on the X-axis
                            axis2 = Taxa, 
                            y= Frequency)) +
  geom_alluvium(aes(fill = Effort)) +
  geom_stratum(width = 1/8) +
  # geom_text(stat = "stratum",
  #aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Years", "Taxa"),
                   expand = c(0.15, 0.2)) +
  scale_y_continuous(
    sec.axis = sec_axis(~., name = "Frequency"),
    position = "right") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme_light()+
  theme(legend.position = "bottom",
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank()) 

eiar_alluvial <- eiar_alluvial +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 4, direction = "y", nudge_x = -.6) +
  scale_fill_brewer(type = "qual", palette = "Dark2", name = "Nature of survey",
                    labels=c("Fieldbased", "Opportunistic", "Scoping"))


####

eiar_alluvial_flipped <- ggplot(data = eiar_surveys,
                        aes(axis1 = Taxa,   # First variable on the X-axis
                            axis2 = Year_Cat, 
                            y= Frequency)) +
  geom_alluvium(aes(fill = Effort)) +
  geom_stratum(width = 1/8) +
  # geom_text(stat = "stratum",
  #aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Taxa", "Years"),
                   expand = c(0.15, 0.2)) +
  scale_y_continuous(
    sec.axis = sec_axis(~., name = "Frequency"),
    position = "right") +
  scale_fill_brewer(type = "qual", palette = "Dark2", ) +
  theme_light()+
  theme(legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),   # Set size of legend text
        axis.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 14, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"), 
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        panel.border = element_blank(),
       axis.line.y.right = element_line(color = "black"),
       axis.line.x.bottom = element_line(color = "black"),
       plot.title = element_text(size = 16, face = "bold", color = "black")) +
  labs(y = "Number of surveys") +
  ggtitle("(b)")

#ggsave("eiar_alluvial_flipped.png", plot = eiar_alluvial_flipped)
###
lit_year_taxa_by_effect <- ggplot(data = lit_year_impacts,
                                  aes(axis1 = year_cat,   # First variable on the X-axis
                                      axis2 = Taxa, 
                                      y = Papers)) +
  geom_alluvium(aes(fill = Effect, curve_type = "cubic")) +
  geom_stratum(width = 1/8) +
  # geom_text(stat = "stratum",
  #aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Years", "Taxa"),
                   expand = c(0.15, 0.2)) +
  scale_fill_brewer(type = "qual", palette = "Dark2", name = "Impact",
                    labels=c("Trophic Level Alteration", "Behavioural Change", "Collision",
                             "Habitat Alteration", "Increased Stress", "Proliferation of Invasive Spp.", "Altered Reproductive Output")) +
  theme_light()+
  theme(legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),   # Set size of legend text
        axis.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 14, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"), 
        panel.border = element_blank(),
        axis.line.y.left = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        plot.title = element_text(size = 16, face = "bold", color = "black")) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(y = "Number of research articles") +
  ggtitle("(a)")

ggsave("lit_year_taxa_by_effect.png", plot = lit_year_taxa_by_effect)


#combined
combined_alluvials <- plot_grid(lit_year_taxa_by_effect, eiar_alluvial_flipped, 
                                 align = "v", hjust = -1)

ggsave("combined_alluvials.png", plot = combined_alluvials, width = 18, height = 8)

#Make Stacked Bar Plots Using the same data used for the alluvial plots

eiar_surveys$Frequency <- as.numeric(eiar_surveys$Frequency)
ggplot(eiar_surveys, aes(x = Year_Cat, y = Frequency) + 
         geom_bar() +
         labs(title = "EIAR",
              x = "Year",
              y = "Frequency") +
         theme_light())

post2008 <- lit_taxa %>%
  filter(year >= 2008 & year <= 2021)
sum(post2008$papers)

result <- lit_taxa %>%
  filter(year >= 2008 & year <=2021 & effect == "collision")
total_collision <- sum(result$papers)

result2 <- lit_taxa %>%
  filter(year >= 2008 & year <= 2021 & effect == "habitat_alteration")
total_ha <- sum(result2$papers)

post2016  <- lit_taxa %>% 
  filter(year >= 2016 & year <= 2021)
sum(post2016$papers)

collisionpost2016 <- lit_taxa %>% 
  filter(year >= 2016 & year <= 2021 & effect == "collision")
sum(collisionpost2016$papers)

batcollisionpost2016 <- lit_taxa %>% 
  filter(year >= 2016 & year <= 2021 & effect == "collision" & taxa == "bats")
sum(batcollisionpost2016$papers)


early <- lit_taxa %>%
  filter(year >=2007)
sum(early$papers)
sum(lit_taxa$papers)

lit <- read.csv("Literature.csv")
lit_counts <- lit %>%
  group_by(taxa) %>%
  summarise(count = n())
