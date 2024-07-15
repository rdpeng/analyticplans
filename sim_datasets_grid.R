## Simulate datasets on a grid

################################################################################

library(progress)
library(furrr)
library(tidyverse)


ncut <- 40
x0 <- (1:ncut) / ncut
y0 <- (1:ncut) / ncut
gr <- expand_grid(x = x0, y = y0)
ng <- nrow(gr)
ng
p <- 20
# ng^p |>
#     format(big.mark = ",")

ix <- seq_len(ng)

Nsamp <- 200000
pb <- progress_bar$new(total = Nsamp)
r <- replicate(Nsamp, {
    pb$tick()
    j <- sample(ix, p, replace = TRUE)
    dat <- gr[j, ]
    fit <- lm(y ~ x, data = dat)
    tibble(dat = list(dat),
           fit = list(fit))
}, simplify = FALSE)

rr <- r |>
    bind_rows(.id = "id")
rr |>
    mutate(beta = map_dbl(fit, ~ coef(.x)[2])) |>
    filter(beta > 0.4 & beta < 0.5) |>
    mutate(fit2 = map(dat, ~ lm(y ~ ns(x, 2), data = .x))) |>
    mutate(curve = map2_int(fit, fit2, function(f1, f2) {
        anova(f1, f2)$`Pr(>F)`[2] < 0.01
    })) |>
    filter(curve > 0) |>
    mutate(sdr = map_dbl(fit2, ~ summary(.x)$sigma)) |>
    arrange(sdr) |>
    slice(1:8) |>
    unnest(dat) |>
    ggplot(aes(x, y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE,
                formula = "y ~ x") +
    # geom_smooth(method = "lm",
    #             formula = "y ~ ns(x, 2)",
    #             se = FALSE) +
    facet_wrap(vars(id), nrow = 2) +
    coord_fixed() +
    xlim(c(0, 1)) +
    ylim(c(0, 1))





## One at a time
i <- sample(nrow(mu), 1)
gr[mu[i, -k], ] |>
    ggplot(aes(x, y)) +
    geom_smooth(method = "lm", se = FALSE,
                formula = "y ~ x") +
    geom_point(size = 2) +
    # geom_smooth(method = "lm",
    #             formula = "y ~ ns(x, 3)",
    #             se = FALSE) +
    coord_fixed() +
    xlim(c(0, 1)) +
    ylim(c(0, 1))




################################################################################
## 2-D?

library(tidyverse)
library(sets)

ncat <- 5
x0 <- seq(-7, 7, len = ncat)
x0

g <- expand.grid(x = x0, y = x0) |>
    data.matrix()

gs <- as.set(map(1:nrow(g), function(i) g[i, ]))
gs

f <- function(x, y) {
    rbind(x, y) |>
        as.data.frame()
}

nobs <- 5
r <- map(seq_len(nobs), function(x) gs) |>
    reduce(function(x, y) set_outer(x, y, f))
dim(r) <- NULL
length(r)

pb <- progress_bar$new(total = length(r))

y <- map_dbl(r, function(d) {
    pb$tick()
    b <- with(d, try({
        cov(x, y) / var(x)
    }))
    if(inherits(b, "try-error"))
        NA
    else
        b
})

summary(y)

