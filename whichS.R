## Expect xbar in [-2, 2]

library(tidyverse)
set.seed(2024-06-01)

## Expected range is [-a, a]
a <- 2

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
              id = factor(rep(levels, each = n)))
dat |>
    ggplot(aes(x, id)) +
    geom_jitter(aes(color = id), size = 2, alpha = 1/2,
                width = 0, height = .05) +
    geom_point(aes(x = mx, y = id, color = id), size = 10,
               alpha = 1, pch = "|",
               data = dat |> group_by(id) |> summarize(mx = mean(x))) +
    geom_vline(xintercept = c(-a, a), lty = 3)


tests <- list(
    t1 = function(x) {
        any(between(x, -a, a)) && any(x > 20)
    },
    t2 = function(x) {
        all(x > a) &&
            between(mean(x > mean(x)), 0.4, 0.6)
    },
    t3 = function(x) {
        any(x > 30) &&
            !any(between(x, -a, a))
    },
    t4 = function(x) {
        between(mean(x > mean(x)), 0.4, 0.6) &&
            any(between(x, -a, a)) &&
            any(x > a)
    },
    t5 = function(x) {
        sd(x) > 4
    },
    tn = function(x) {
        mean(x) > a
    }
)


d0 <- dat |>
    group_by(id) |>
    reframe(test = map_dbl(tests, function(f) f(x))) |>
    mutate(name = rep(paste0("t", 1:length(tests)), length(levels))) |>
    pivot_wider(values_from = "test") |>
    filter(id %in% 1:4)
d0


################################################################################

x <- rnorm(5, 5, 7)
map_dbl(tests, ~ .x(x))

plot(x, rep(1, length(x)), xlim = range(x, -a, a))
abline(v = c(-a, a), lty = 3)

d0
