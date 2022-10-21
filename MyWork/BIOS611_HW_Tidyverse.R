library(tidyverse);
df <- read_csv("storage/lectures/06-tidy-data-and-ggplot/source_data/character-page-data.csv");

## Function to fix strings
simplify_strings <- function(s){
  str_replace_all(str_trim(str_to_lower(s)), "[^a-z]+","_");
}

## Function to remove duplicates
deduplicated <- df %>% mutate(across(everything(), simplify_strings)) %>%
  distinct();
print(sprintf("Before simplification and deduplication: %d, after %d (%0.2f %% decrease)",
              nrow(df),
              nrow(deduplicated),
              100-100*nrow(deduplicated)/nrow(df)));

## Function to remove observations with less than 20
value_counts <- deduplicated %>% group_by(character) %>% tally() %>%
  arrange(n);

ok_values <- value_counts %>% filter(n>=20);

joined <- deduplicated %>% inner_join(ok_values, by= "character");
joined;

#Creating dataset with n and k values
x <- 1;
n = c();
k = c();
while(x <= 20) {
  temp <- value_counts %>% filter(n>=x);
  
  n = c(n, x);
  k = c(k, nrow(value_counts) - nrow(temp));
  
  x <- x + 1;
}
data <- tibble(n, k);

#Creating Graph
ggplot(data , aes(n,k)) + geom_point();

