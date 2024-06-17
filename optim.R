## Optimization

library(tidyverse)
library(gganimate)

f <- function(x) {
    m <- matrix(x, nrow = n)
    cc <- cor(m[,1], m[,2])
    (cc - rho)^2
}

f <- function(x) {
    m <- matrix(x, nrow = n)
    cc <- cor(m[,1], m[,2])
    h <- lm(m[,2] ~ m[,1]) |>
        resid() |>
        shapiro.test()
    pval <- h$p.value
    (cc - rho)^2 + (pval)^2
}

n <- 100
rho <- 0

pstart <- matrix(rnorm(n * 2), nrow = n) |>
    scale(center = TRUE, scale = FALSE)
op <- optim(pstart, f, method = "L-BFGS-B",
            lower = -10, upper = 10)
colnames(op$par) <- c("x", "y")
op$par |>
    as_tibble() |>
    ggplot(aes(x, y)) +
    geom_point() +
    geom_smooth() +
    geom_smooth(method = "lm",
                se = FALSE)

plot(op$par)
with(op, abline(lm(par[,2] ~ par[,1])))

r <- replicate(100, {
    pstart <- matrix(rnorm(n * 2), nrow = n) |>
    # pstart <- matrix(rexp(n * 2, 0.2), nrow = n) |>
        scale(center = TRUE, scale = FALSE)
    try({
        op <- optim(pstart, f, method = "L-BFGS-B",
                    lower = -10, upper = 10)
        op$par
    })
}, simplify = FALSE)
str(r)



# r <- vector("list", length = 100)
# pstart <- matrix(rnorm(n * 2), nrow = n)
# for(i in seq_along(r)) {
#     try({
#         op <- optim(pstart, f, method = "L-BFGS-B",
#                     lower = -10, upper = 10)
#         r[[i]] <- op$par
#         pstart <- op$par
#     })
# }
# str(r)

r |>
    map(as_tibble) |>
    bind_rows(.id = "dataset") |>
    ggplot(aes(V1, V2)) +
    geom_point(alpha = 1) +
    facet_wrap(vars(dataset)) +
    theme_bw()


rng <- r |>
    map(as_tibble) |>
    bind_rows() |>
    reframe(across(everything(), range))
rng
for(i in seq_along(r)) {
    d <- r[[i]] |>
        as_tibble()
    g <- d |>
        ggplot(aes(V1, V2)) +
        geom_point() +
        xlim(rng$V1) +
        ylim(rng$V2) +
        coord_fixed()
    print(g)
    readline(sprintf("(%d) Next: ", i))
}


pstart <- matrix(rnorm(5 * 2), nrow = n)
op <- optim(pstart, f, method = "BFGS")
op$par
plot(op$par)
