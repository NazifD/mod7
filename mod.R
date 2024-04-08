rm(list=ls())

library(tidyverse)

# Part 1

make_df <- function(n=10000){
  x_coords <- runif(n,-1,1)
  y_coords <- runif(n,-1,1)
  points <- data.frame(x=x_coords,y=y_coords)
  points$radius <- with(points, sqrt(x^2 + y^2))
  points$in_circ <- points$radius <= 1
  pi_est <- 4 * sum(points$in_circ) / n
  return(list(pi_est = pi_est, df = points))
}

df_list <- make_df()
df <- df_list$df

circle_plot <- ggplot(df, aes(x=x, y=y, color=in_circ, alpha=radius)) +
  geom_point() +
  labs(title = "Estimating π using Random Points and a Circle",
       x = "X Coordinate", y = "Y Coordinate",
       color = "Inside Circle", alpha = "Radius") +
  theme_classic()

ggsave("circle_plot.png", circle_plot, width = 8, height = 6)

# Part 2

many_samples <- function(reps, group_size) {
  
  shared_birthdays <- sapply(1:reps, function(i) {
    birthdays <- sample(1:365, group_size, replace = TRUE)
    length(unique(birthdays)) != group_size
  })
  return(mean(shared_birthdays))
}

group_sizes <- as.numeric(2:366)
sample_size <- 1000
estimated_probs <- lapply(group_sizes, function(x){
  many_samples(reps = sample_size, group_size = x)
})
df <- data.frame(group_size = group_sizes, estimated_prob = unlist(estimated_probs))

birthday_plot <- ggplot(df %>% filter(group_size <= 75), aes(group_size, estimated_prob)) +
  geom_point() +
  labs(title = "Relationship between Group Size and Probability of Shared Birthday",
       x = "Group Size", y = "Probability of Shared Birthday") +
  theme_classic()

ggsave("birthday_plot.png", birthday_plot, width = 8, height = 6)

