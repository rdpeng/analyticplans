## Invert data

library(MASS)
library(tidyverse)

a <- 0
b <- 0.3
sdev1 <- 1
sdev2 <- 1
rho <- 0.2
S <- cbind(c(sdev1^2, rho * sdev1 * sdev2),
           c(rho * sdev1 * sdev2, sdev2^2))

r <- replicate(2000, {
    x <- mvrnorm(2, c(0, 0), S)
    z <- crossprod(x)
    s <- prod(sqrt(diag(z)))
    r <- z[1,2] / s
    tibble(x1 = x[1, 1], x2 = x[1, 2],
           x3 = x[2, 1], x4 = x[2, 2],
           expect = between(r, a, b))
}, simplify = FALSE) |>
    bind_rows() |>
    mutate(expect = factor(expect,
                           labels = c("Unexpected", "As-Expected")))
r

r |>
    mutate(x3 = cut_number(x3, 3)) |>
    mutate(x4 = cut_number(x4, 3)) |>
    ggplot(aes(x1, x2)) +
    geom_point(aes(color = expect)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    facet_grid(row = vars(x3),
               col = vars(x4))

