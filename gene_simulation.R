library(tidyverse)
library(rpart)
library(rpart.plot)

sim_data <- function() {
    x <- rpois(m, mu)
    names(x) <- paste0("Sample", seq_len(m))
    x
}


# mu <- rexp(m, 0.1)
mu <- c(rep(2, 10))
m <- 10
N <- 1000
a <- 2

r <- replicate(N, sim_data(), simplify = FALSE) |>
    map(~ .x |> as.list() |> as_tibble()) |>
    bind_rows(.id = "dataset") |>
    group_by(dataset) |>
    mutate(mean = across(starts_with("Sample"), ~ .x) |> unlist() |> mean()) |>
    ungroup() |>
    mutate(expect = factor(as.integer(mean >= a)))
r

table(r$expect)

r |>
    ggplot(aes(mean)) +
    geom_histogram(bins = 10)

r |>
    glimpse()

r |>
    select(-dataset) |>
    group_by(expect) |>
    summarize(across(starts_with("Sample"), ~ mean(.x)))


fit <- rpart(expect ~ ., data = r |> select(-c(dataset, mean)),
             control = rpart.control(cp = 0.04))
rpart.plot(fit)

