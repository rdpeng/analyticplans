## Simulate datasets on a grid

library(tidyverse)

# dat <- airquality |>
#     filter(if_all(everything(), ~ !is.na(.x))) |>
#     as_tibble()
# dat
#
# xrng <- range(dat$Wind)
# yrng <- range(dat$Ozone)
#
# gx <- seq(xrng[1], xrng[2], len = 30)
# gy <- seq(yrng[1], yrng[2], len = 30)
# gr <- expand.grid(Wind = gx, Ozone = gy) |>
#     as_tibble()
# gr
#
# gr |>
#     sample_n(111) |>
#     ggplot(aes(Wind, Ozone)) +
#     geom_point() +
#     geom_point(aes(Wind, Ozone), data = dat,
#                color = "red")



################################################################################

library(progress)
library(furrr)
library(tidyverse)


x0 <- 1:6
y0 <- 1:6
gr <- expand.grid(x = x0, y = y0)
ng <- nrow(gr)
p <- 5
ng^p

#pb <- progress_bar$new(total = ng^p)
plan(multisession, workers = 9L)

r <- future_map(1:ng, function(i1) {
    map(1:ng, function(i2) {
        map(1:ng, function (i3) {
            map(1:ng, function(i4) {
                map(1:ng, function(i5) {
                    # pb$tick()
                    dat <- bind_rows(gr[i1, ],
                                     gr[i2, ],
                                     gr[i3, ],
                                     gr[i4, ],
                                     gr[i5, ])
                    fit <- lm(y ~ x, data = dat)
                    c(i1, i2, i3, i4, i5, coef(fit)[2])
                })
            })
        })
    })
}, .progress = TRUE)



rr <- unlist(r, use.names = FALSE)
m <- matrix(rr, byrow = TRUE, ncol = p + 1)
head(m)
tail(m)

k <- p + 1
hist(m[, k])

u <- !is.na(m[, k]) & m[, k] > 0.4 & m[, k] < 0.5
## u <- !is.na(m[, k]) & m[, k] > 1
sum(u)
mu <- m[u, ]
head(mu)


## i <- 2695599

i <- sample(nrow(mu), 1)
gr |>
    left_join(gr[mu[i, -k], ] |>
                     mutate(include = 1),
              by = c("x", "y")) |>
    ggplot(aes(x, y)) +
    geom_raster(aes(fill = include), alpha = 1) +
    geom_smooth(method = "lm", se = FALSE,
                formula = "y ~ x",
                data = gr[mu[i, -k], ]) +
    geom_point(size = 2,
               data = gr[mu[i, -k], ]) +
    coord_fixed()



################################################################################
## 1-D case with sample mean

library(tidyverse)

ncat <- 20
x0 <- seq(-7, 7, len = ncat)
x0

nobs <- 6
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
    mutate(positive = ifelse(all(x > 0), "pos", "neg"),
           sdx = ifelse(sd(x) < 1.9, "smallv", "largev"))
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

