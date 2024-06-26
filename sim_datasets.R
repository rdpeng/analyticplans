## Datasets

library(tidyverse)

outcome <- function(x) {
    m <- mean(x)
    m >= -1 && m <= 1
}

simdata <- function(n) {
    x <- rnorm(n-1, 0, 1)
    if(rbinom(1, 1, 0.2) == 1L) {
        c(x, 100)
    } else {
        c(x, rnorm(1, 0, 1))
    }
}

d <- tibble(dataset = replicate(10000, simdata(50), simplify = FALSE)) |>
    mutate(result = map_dbl(dataset, outcome))
d |>
    summarize(result = mean(result))
d |>
    sample_n(10)
d

r <- replicate(100, {
    x <- simdata(50)
    dr <- tibble(dataset = replicate(10000, sample(x, length(x), replace = TRUE),
                                     simplify = FALSE)) |>
        mutate(result = map_dbl(dataset, outcome))
    mean(dr$result)
})
## r <- unlist(r)
summary(r)


simdata(100) |>
    matrix(ncol = 1) |>
    dist() |>
    as.matrix() |>
    image(asp = 1)

simdata(100) |>
    matrix(ncol = 1) |>
    dist() |>
    hclust() |>
    plot()

#######################################