library(tidyverse)
library(furrr)

k <- 20
n <- 50
N <- 500000

r <- replicate(N, {
    x1 <- sample(k, n, replace = TRUE)
    x2 <- sample(k, n, replace = TRUE)
    list(x1 = x1, x2 = x2,
         cor = cor(x1, x2))
}, simplify = FALSE)

r |>
    map_dbl("cor") |>
    hist()

u <- r |>
    map_dbl("cor") |>
    between(-0.7, -0.5)
sum(u)
r1 <- r[!is.na(u) & u]
r1 |>
    sample(size = min(20, length(r1))) |>
    map(function(z) {
        tibble(x1 = z$x1,
               x2 = z$x2)
    }) |>
    bind_rows(.id = "dataset") |>
    group_by(dataset) |>
    count(x1, x2) |>
    ggplot(aes(x1, x2)) +
    #geom_raster(aes(fill = n)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "loess",
                formula = "y ~ x") +
    facet_wrap(vars(dataset))


################################################################################

dat <- airquality |>
    select(Ozone, Wind) |>
    filter(if_all(everything(), ~ !is.na(.x)))
dat

dat |>
    ggplot(aes(Wind, Ozone)) +
    geom_point()


dat |>
    summarize(cor(Wind, Ozone))

library(furrr)
plan(multisession, workers = 10L)

n <- nrow(dat)
N <- 500000
r <- future_map(seq_len(N), function(i) {
    dat1 <- tibble(Ozone = sample(dat$Ozone, n, replace = TRUE),
                   Wind = sample(dat$Wind, n, replace = TRUE))
    list(dat = dat1, cor = with(dat1, cor(Ozone, Wind)))
}, .options = furrr_options(seed = TRUE))

r |>
    map_dbl("cor") |>
    hist()

u <- r |>
    map_dbl("cor") < -0.3
which(u)

r[[1368]]$dat |>
    ggplot(aes(Wind, Ozone)) +
    geom_point()


rho <- with(dat, cor(Wind, Ozone))
rho
n <- nrow(dat)

f <- function(x) {
    m <- matrix(x, nrow = n)
    cc <- cor(m[,1], m[,2])
    (cc - rho)^2
}

x0 <- dat |>
    as.vector() |>
    unlist(use.names = FALSE)
x0
f(sample(x0, length(x0), replace = TRUE))

pstart <- sample(x0, length(x0), replace = TRUE)
#pstart <- runif(2 * n, 0, 170)
pstart |>
    matrix(nrow = n) |>
    plot(xlim = c(0, 170), ylim = c(0, 170))

op <- optim(pstart, f, method = "L-BFGS-B", lower = 0 , upper = 170)
op$par |>
    matrix(nrow = n) |>
    plot(xlim = c(0, 170), ylim = c(0, 170))

