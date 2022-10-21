library(tidyverse);
library(reticulate);

data <- read_csv("storage/lectures/10-dimensionality-reduction/homework_data.csv");

#Principle Component Analysis
r <- prcomp(data);
imagesc(r$x);
summary(r);

#Answer to 1: We need 2 Principle Components to account for at least 90% of variance
#Plotting
ggplot(r$x %>% as_tibble() %>% select(PC1, PC2), aes(PC1, PC2)) +
  geom_point();

spikes <- data %>% group_by(trial) %>% filter(V >= 0 & lag(V) < 0) %>% ungroup();
ggplot(spikes, aes(time, trial)) + geom_point(shape="|")

#TSNE
use_python("/usr/bin/python3");
manifold <- import("sklearn.manifold");

tsne_instance <- manifold$TSNE(n_components=as.integer(2));
results <- tsne_instance$fit_transform(data %>% select(-trial, -label) %>% as.matrix()) %>%
  as_tibble();

ggplot(results, aes(V1, V2)) + geom_point(aes(color=factor(fn_datav$label)));