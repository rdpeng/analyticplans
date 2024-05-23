## Inverse domain

library(tidyverse)

quad <- function(x1, x2) {
    if(x1 >= 0 && x2 >= 0)
        1
    else if(x1 < 0 && x2 >= 0)
        2
    else if(x1 < 0 && x2 < 0)
        3
    else
        4
}
quad <- Vectorize(quad)

sector <- function(x1, x2, x3) {
    x <- tibble(s1 = sign(x1), s2 = sign(x2), s3 = sign(x3))
    tab <- do.call("expand.grid",
                   map(1:3, function(x) c(-1, 1)))
    names(tab) <- paste0("s", 1:3)
    tab <- tab |>
        mutate(sector = 1:8)
    inner_join(tab, x, by = c("s1", "s2", "s3"))$sector
}
sector <- Vectorize(sector)

## Simulate the distribution of X1 and X2 given (X1+X2)/2 %in% (-1, 1)

a <- -2
b <- 3
sdev <- 10

simdata <- function() {
    x <- rnorm(3, 0, sdev)
    #x <- runif(2, -10, 10)
    #x <- rgamma(2, 1, 1/2)
    # x <- c(rnorm(1, 0, 10),
    #        rgamma(1, 1, 1/2))
    x
}

r <- replicate(2000, {
    x <- simdata()
    m <- median(x)
    tibble(x1 = x[1], x2 = x[2],
           x3 = x[3],
           expect = between(m, a, b))
}, simplify = FALSE) |>
    bind_rows() |>
    mutate(expect = factor(expect,
                           labels = c("Unexpected", "As-Expected")),
           sector = sector(x1, x2, x3))
           #sector = quad(x1, x2))

r |>
    mutate(x3 = cut_number(x3, 2)) |>
    ggplot(aes(x1, x2)) +
    geom_point(aes(color = expect), alpha = 1) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    xlab(expression(x[1])) +
    ylab(expression(x[2])) +
    labs(title = substitute((x[1] + x[2])/2 %in% group("[", list(a, b), "]"),
                            list(a = a, b = b))) +
    facet_wrap(vars(x3))

r |>
    count(expect) |>
    mutate(prop = n / sum(n))
g <- r |>
    group_by(sector) |>
    count(expect) |>
    ungroup()
g

xtabs(n ~ sector + expect, data = g)

# x <- simdata()
# b <- expand.grid(x, x, x) |>
#     rowMeans() |>
#     between(a, b) |>
#     as.integer()
# b |>
#     mean()
#
# f <- function(x) {
#     r <- map_dbl(x, function(xx) {
#         prod(dbinom(b, 1, xx))
#     })
#     r / max(r)
# }
# curve(f, 0, 1, n = 501)
# abline(h = 1/8)
#
# curve(f, -3*sdev, 3*sdev)
# abline(h = 1/8)
