## Expect xbar in [-2, 2]

library(tidyverse)

n <- 5
x <- rnorm(n, 0, 1)
mean(x)

x1 <- rnorm(n, 0, 1)
x1[5] <- 25
mean(x1)

x2 <- rnorm(n, 5, 1)
mean(x2)

x3 <- rnorm(n, 0, 3)
x3[1] <- -15
x3[2] <- 15

x4 <- rnorm(n, -3, 1)
x4[which.max(x4)] <- 35
mean(x4)

x5 <- rnorm(n, -5, 1)
x5[which.max(x5)] <- 20
mean(x5)

x6 <- rnorm(n, 5, 7)
mean(x6)

x7 <- rnorm(n, 5, 1)
x7[which.max(x7)] <- -20
mean(x7)

levels <- c(0, 1, 2, -1, 3, -2, 4, -3)
dat <- tibble(x = c(x, x1, x2, x3, x4, x5, x6, x7),
              id = factor(rep(levels, each = 5)))
dat |>
    ggplot(aes(x, id)) +
    geom_jitter(aes(color = id), size = 2, alpha = 1/2,
                width = 0, height = .05) +
    geom_point(aes(x = mx, y = id, color = id), size = 10,
               alpha = 1, pch = "|",
               data = dat |> group_by(id) |> summarize(mx = mean(x))) +
    geom_vline(xintercept = c(-2, 2), lty = 3)



t1 <- function(x) {
    all(x > 2)
}
t2 <- function(x) {
    any(x > 10)
}


dat |>
    group_by(id) |>
    summarize(
        t1 = all(x > 2),
        t2 = any(x > 10),
        t3 = any(x < -10),
        t4 = between(mean(x > 5), .4, .6)
    ) |>
    mutate(across(starts_with("t"), as.integer))


