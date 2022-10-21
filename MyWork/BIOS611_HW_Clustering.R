library(tidyverse);
library(gridextra);
#Q1
data <- rbind(tibble(x=rnorm(100, 3, 1),
                     y=rnorm(100, 3, 1)),
              tibble(x=rnorm(100, -3, 1),
                     y=rnorm(100, 3, 1)),
              tibble(x=rnorm(100, 0, 1),
                     y=rnorm(100, -3, 1)));

ggplot(data, aes(x,y)) + geom_point() + coord_equal();

results <- kmeans(data, centers = 3);


#Q2
mutinf <- function(a,b){
  sa <- shannon(a);
  sb <- shannon(b);
  sab <- shannon(sprintf("%d:%d", a, b));
  sa + sb - sab;
}

normalized_mutinf <- function(a,b){
  2*mutinf(a,b)/(shannon(a)+shannon(b));
}

cluster2_1 <- kmeans(data, centers = 2);
cluster2_2 <- kmeans(data, centers = 2);
normalized_mutinf(cluster2_1, cluster2_2);
cluster3_1 <- kmeans(data, centers = 3);
cluster3_2 <- kmeans(data, centers = 3);
normalized_mutinf(cluster3_1, cluster3_2);
cluster4_1 <- kmeans(data, centers = 4);
cluster4_2 <- kmeans(data, centers = 4);
normalized_mutinf(cluster4_1, cluster4_2);
cluster5_1 <- kmeans(data, centers = 5);
cluster5_2 <- kmeans(data, centers = 5);
normalized_mutinf(cluster5_1, cluster5_2);

#Q3
data2 <- rbind(tibble(r=rnorm(100, 3, 0.8),
                      th=rnorm(100, 3, 0.1)),
               tibble(r=rnorm(100, 0, 1.2),
                      th=rnorm(100, 3*pi/2, 0.1)),
               tibble(r=rnorm(400, 6, 0.5),
                      th=runif(400, 0, 2*pi))) %>%
  transmute(x=r*cos(th),y=r*sin(th)); 
ggplot(data2, aes(x,y)) + geom_point()

#From evaluating the scatterplot, it seems that k-means would fail to cluster the data since there is radial symmetry.
#This makes it difficult to find the true "center" of each of the groups. Fuzzy C-Means, however, would perform better
#since each point doesn't exclusively belong to a certain group like in k-means

#Q4
