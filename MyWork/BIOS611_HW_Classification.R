library(tidyverse);
#Removing rare elements to other
genders <- read_csv("storage/lectures/12-classification/source_data/prime_earth_characters.csv") %>%
  filter(property_name=="gender") %>% select(-property_name, -universe) %>%
  rename(gender=value);
powers <- read_csv("storage/lectures/12-classification/source_data/prime_earth_powers.csv") %>%
  select(power, character);

power_counts <- powers %>% group_by(power) %>% tally() %>% arrange(desc(n));
common <- power_counts %>% pull(power) %>% head(20);
uncommon <- power_counts %>% filter(!(power %in% common)) %>% pull(power);

powers_wide <- powers %>% mutate(power = {
  p <- power;
  p[p %in% uncommon] <- "other";
  p
}) %>% distinct() %>%
  mutate(dummy=1) %>%
  pivot_wider(id_cols="character", names_from="power", values_from="dummy", values_fill=list(dummy=0));

data <- genders %>%
  inner_join(powers_wide, by="character") %>%
  mutate(across(everything(), factor)) %>% filter(gender %in% c("male", "female")) %>%
  mutate(gender=1*(gender=="male"));

data2 <- select(data, -character);

#Reducing to 6 Dimensions
reduced <- kmeans(data2, centers = 6);

data3 <- data %>% left_join(data %>%
                             mutate(cluster=reduced$cluster) %>%
                             select(character, cluster),
                             by= "character");

data4 <- data3 %>% select(cluster, gender);

data5 <- data4 %>% mutate(cluster1 = ifelse(cluster == 1, 1, 0), cluster2 = ifelse(cluster == 2, 1, 0),
                          cluster3 = ifelse(cluster == 3, 1, 0), cluster4 = ifelse(cluster == 4, 1, 0),
                          cluster5 = ifelse(cluster == 5, 1, 0), cluster6 = ifelse(cluster == 6, 1, 0));

data5;
#Logistic Regression
r <- glm(gender ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, data=data5, family=binomial);
summary(r);

#ROC Curve


