################################################################################
## 1-D case with sample mean

library(tidyverse)
library(progress)

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

nsamp <- 30L
isamp <- idx[sample(seq_len(nrow(idx)), nsamp), ]
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
## 1-D case with sample mean w/sampling

library(e1071)

ncat <- 50
x0 <- seq(-20, 20, len = ncat)
x0
nobs <- 6
ncat^nobs |>
    format(big.mark = ",")

nsamp <- 30000L
pb <- progress_bar$new(total = nsamp)
y <- replicate(nsamp, {
    pb$tick()
    x <- sample(x0, nobs, replace = TRUE)
    list(dat = list(x),
         mean = mean(x))
}, simplify = FALSE) |>
    bind_rows(.id = "id")

d <- y |>
    filter(mean > 4 & mean < 5) |>
    mutate(var = map_dbl(dat, var)) |>
    mutate(skew = map_dbl(dat, skewness)) |>
    mutate(skewc = cut_number(skew, 3)) |>
    # group_by(skewc) |>
    # slice_sample(n = 5) |>
    arrange(skew) |>
    slice(c(1:5, n():(n() - 5 + 1)))
d |>
    unnest(dat) |>
    mutate(id = fct_reorder(id, dat, skewness)) |>
    ggplot(aes(dat, id)) +
    geom_jitter(size = 4,
                height = 0, width = 0.5,
                alpha = 1/2) +
    geom_point(aes(mean, id),
               data = d |>
                   select(-dat) |>
                   distinct(),
               color = "red", size = 3, pch = 15)



