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

# https://www.jensenmath.ca/1.5%20worksheet.pdf
# negative reciprocal of the slope makes a perpendicular line
(function() {

  ggplot() + 
    geom_abline(intercept = 3, slope = 5) + 
    geom_abline(intercept = 17, slope = 1/-5) +
    xlim(0, 25) + 
    ylim(0, 25) 
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
    data = dplyr::filter(models, rank_dist <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, color = "red") +
  geom_point(aes(color = -dist))

grid <- expand.grid(
    a1 = seq(-5, 20, length = 25),
    a2 = seq(1, 3, length = 25)
  ) %>%
  mutate(
    dist = purrr::map2_dbl(a1, a2, sim1_dist),
    rank_dist = rank(dist)
  )

unique(grid$a1)

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank_dist <= 10), size = 4, color = "red") +
  geom_point(aes(color = -dist)) 


ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, color = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank_dist <= 10)
  )

best <- optim(c(0, 0), measure_distance, data = sim1)

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

# One downside of the linear model is that it is sensitive to unusual values
# because the distance incorporates a squared term. Fit a linear model to the
# simulated data below, and visualise the results. Rerun a few times to generate
# different simulated datasets. What do you notice about the model?

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

sim1a_mod <- lm(y ~ x, data = sim1a)

ggplot(sim1a, aes(x, y)) + 
  geom_point() + 
  geom_abline(
    intercept = sim1a_mod$coefficients["(Intercept)"],
    slope = sim1a_mod$coefficients["x"]
  )


# One way to make linear models more robust is to use a different distance
# measure. For example, instead of root-mean-squared distance, you could use
# mean-absolute distance:

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}


best <- optim(c(0, 0), measure_distance, data = sim1a)
ggplot(sim1a, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])
