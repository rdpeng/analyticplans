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
## 1-D case with sample mean

library(tidyverse)

ncat <- 22
x0 <- seq(-7, 7, len = ncat)
x0
nobs <- 6
ncat^nobs |>
    format(big.mark = ",")

y <- map(seq_len(nobs), function(x) x0) |>
    reduce(function(x, y) outer(x, y, "+")) / nobs
dim(y)
range(y)

## y
idx <- which(y > 4 & y < 5, arr.ind = TRUE)
#idx <- which(y > -0.5 & y < 0.5, arr.ind = TRUE)
nrow(idx)

isamp <- idx[sample(seq_len(nrow(idx)), 20), ]
isamp <- isamp[order(apply(isamp, 1, var)), ]
dat <- tibble(x = apply(isamp, 1, function(j) x0[j]) |>
                  as.vector(),
              id = rep(1:nrow(isamp), each = nobs)) |>
    group_by(id) |>
    mutate(positive = ifelse(all(x > 0), "All Positive", "Some Negative"),
           sdx = ifelse(sd(x) < 3, "Small Var.", "Large Var."))
dat |>
    mutate(property = interaction(positive, sdx)) |>
    ggplot(aes(x, id)) +
    geom_hline(aes(yintercept = id), col = "gray") +
    geom_vline(xintercept = 0, lty = 2) +
    geom_jitter(aes(color = sdx),
                size = 4,
                height = 0, width = 0.05,
                alpha = 1/2) +
    geom_point(aes(m, id),
               data = dat |>
                   group_by(id) |>
                   summarize(m = mean(x)),
               color = "red", size = 3, pch = 15) +
    xlim(c(-7, 7)) +
    theme_bw()

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

