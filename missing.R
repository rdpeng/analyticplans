## Missing data

library(tidyverse)
library(rpart)
library(rpart.plot)
library(broom)

set.seed(2024-06-03)
n <- 200
x0 <- tibble(x = rnorm(n, 0, 1),
             z = rnorm(n, 0, 1),
             miss = rbinom(n, 1, 0.1))
x0

x1 <- tibble(x = rnorm(n, 0, 1),
             z = x + rnorm(n, 0, 1),
             miss = rbinom(n, 1, plogis(z/.5)))
x1

x2 <- tibble(x = rnorm(n, -.7, 1),
             z = rnorm(n, 0, 1),
             miss = rbinom(n, 1, 0.1))
x2

x3 <- tibble(x = rnorm(n, -1.25, 1),
             z = x + rnorm(n, 0, 1),
             miss = rbinom(n, 1, plogis(-z / 1)))
x3

x4 <- tibble(x = rnorm(n, 0, 1),
             z = rnorm(n, 0, 1),
             miss = rbinom(n, 1, plogis(x / 1)))
x4

dat <- bind_rows(x0, x1, x2, x3, x4,
          .id = "dataset")
dat |>
    mutate(miss = factor(miss)) |>
    ggplot(aes(x, dataset)) +
    geom_jitter(aes(color = miss), alpha = 1/2,
                height = 0.1, width = 0) +
    geom_point(aes(x = m, y = dataset), pch = "|",
               size = 10,
               data = dat |>
                   filter(miss == 0L) |>
                   group_by(dataset) |>
                   summarize(m = mean(x)))

tests <- list(
    t1 = function(x, z, miss) {
        between(mean(x, na.rm = TRUE), -0.4, 0.4)
    },
    t2 = function(x, z, miss) {
        ts <- glm(miss ~ z) |>
            tidy() |>
            filter(term == "z") |>
            pull(statistic)
        ts > 3
    },
    t3 = function(x, z, miss) {
        ts <- glm(miss ~ z) |>
            tidy() |>
            filter(term == "z") |>
            pull(statistic)
        abs(ts) < 3
    },
    t4 = function(x, z, miss) {
        ts <- glm(miss ~ z) |>
            tidy() |>
            filter(term == "z") |>
            pull(statistic)
        ts < -3
    },
    t5 = function(x, z, miss) {
        mean(miss) < 0.1
    },
    tn = function(x, z, miss) {
        mean(x, na.rm = TRUE) < -0.4
    }
)

d0 <- dat |>
    mutate(x = replace(x, miss == 1, NA_real_)) |>
    group_by(dataset) |>
    reframe(test = map_dbl(tests, function(f) f(x, z, miss))) |>
    mutate(name = rep(paste0("t", 1:length(tests)), length(unique(dataset)))) |>
    pivot_wider(names_from = "name", values_from = "test")
d0


dat |>
    filter(miss == 0L) |>
    ggplot(aes(x, dataset)) +
    geom_jitter(aes(color = dataset),
                width = 0, height = 0.1) +
    geom_point(aes(x = m, y = dataset), pch = "|",
               size = 10,
               data = dat |>
                   filter(miss == 0L) |>
                   group_by(dataset) |>
                   summarize(m = mean(x)))

dat |>
    filter(miss == 0L) |>
    group_by(dataset) |>
    summarize(mean(x), sd(x))

dat1 <- dat |>
    mutate(x = replace(x, miss == 1L, NA_real_))
dat1

dat1 |>
    group_by(dataset) |>
    summarize(mean(miss))

fit <- rpart(dataset ~ x + z + miss, data = dat1,
      control = rpart.control(cp = 0.01))
fit |>
    rpart.plot()

dat1$pred <- predict(fit, newdata = dat1, type = "class")
dat1

dat1 |>
    group_by(dataset) |>
    summarize(class = which.max(table(pred)),
              prop = max(table(pred)) / sum(table(pred)))

dat1 |>
    group_by(dataset) |>
    reframe(class = table(pred)) |>
    print(n = 25)
