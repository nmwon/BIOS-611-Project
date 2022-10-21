#BIOS611 Project
#Reading in Data
library(tidyverse);
#Age Data
md_age <- read_csv("Data/MD/NIBRS_Age.csv");
nc_age <- read_csv("Data/NC/NIBRS_Age.csv");

#Offender Data
md_off <- read_csv("Data/MD/NIBRS_offender.csv");
nc_off <- read_csv("Data/NC/NIBRS_offender.csv");

#Merging Age Data
merge <- md_off %>% left_join(md_age, by = "AGE_ID");

#Creating Graphs
bar_age_graph <- ggplot(md_off, aes(AGE_NUM)) + geom_bar();
bar_sex_graph <- ggplot(md_off, aes(x = SEX_CODE)) + geom_bar();

ggsave("Figures/bar_age_graph.png", bar_age_graph);
ggsave("Figures/bar_sex_graph.png", bar_sex_graph);