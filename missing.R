## Missing data

library(tidyverse)


n <- 100
x0 <- tibble(x = rnorm(n, 0, 1),
             z = rnorm(n, 0, 1),
             miss = rbinom(n, 1, 0.1))
x0

x1 <- tibble(x = rnorm(n, 0, 1),
             z = x + rnorm(n, 0, 1),
             miss = rbinom(n, 1, plogis(z/.05)))
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
f
