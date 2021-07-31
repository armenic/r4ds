# The goal of a model is not to uncover truth, but to discover a simple
# approximation that is still useful.

library(tidyverse)

library(modelr)
options(na.action = na.warn)

# https://www.jensenmath.ca/1.5%20word%20problems%20lesson%20solutions.pdf
(function() {
  x <- 4200
  y <- 5800
  
  ggplot() + 
    geom_abline(intercept = 10000, slope = -1) + 
    geom_abline(intercept = 413 / 0.035, slope = -0.05 / 0.035) +
    geom_hline(yintercept = y) +
    geom_vline(xintercept = x) +
    geom_label(aes(x + 1500, y + 1000, label = "POI (4200, 5800)")) + 
    xlim(-5000, 7000) + 
    ylim(5000, 20000)
})()


View(sim1)

ggplot(sim1, aes(x, y)) + 
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) + 
  geom_point()



#' @param a model with intercept a[1] and slope a[2]
#' @param data dataframe
#' 
#' @return predicted values as a vector
#'
#' @examples
#' 
#' model1(c(7, 1.5), sim1)
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

# In order to compute overall distance between the predicted and actual values
# we can use root-mean-squared deviation.
#' @param a model with intercept a[1] and slope a[2]
#' @param data dataframe
#'
#' @return root-mean-squared deviation
#' @export
#'
#' @examples
#' mod <- c(7, 1.5)
#' data <- sim1
#' measure_distance(mod, data)
measure_distance <- function(mod, data) {
  predicted_y <- model1(mod, data)
  deviation <- data$y - predicted_y
  squared_deviation <- deviation ^ 2
  sqrt(mean(squared_deviation))
}

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(
    dist = purrr::map2_dbl(a1, a2, sim1_dist),
    rank_dist = rank(dist)
  )

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, color = "red") +
  geom_point(aes(color = -dist))
