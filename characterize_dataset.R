## How do you summarize a dataset?

library(MASS)
library(tidyverse)
library(broom)

dat <- airquality |>
    filter(if_all(everything(), ~ !is.na(.x))) |>
    as_tibble()

r <- replicate(2000, {
    d <- dat |>
        sample_frac(size = 1, replace = TRUE)
    fit <- lm(Ozone ~ Temp, data = d)
    b <- coef(fit)[2]
    tibble(dataset = list(d), coef = b)
}, simplify = FALSE)

rr <- r |>
    bind_rows(.id = "id") |>
    mutate(expect = ifelse(coef < 2.2, 1, 0))
rr |>
    count(expect)

rr |>
    unnest(dataset) |>
    count(expect, Temp) |>
    group_by(expect) |>
    mutate(p = n / sum(n)) |>
    arrange(Temp) |>
    mutate(expect = factor(expect)) |>
    ggplot(aes(Temp, p)) +
    geom_point(aes(color = expect))

rr |>
    unnest(dataset) |>
    count(expect, Temp) |>
    group_by(expect) |>
    mutate(p = n / sum(n)) |>
    arrange(Temp) |>
    select(-n) |>
    pivot_wider(names_from = "expect", values_from = "p") |>
    mutate(diff = `1` - `0`) |>
    ggplot(aes(Temp, diff)) +
    geom_point() +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()

rr |>
    unnest(dataset) |>
    count(expect, Ozone) |>
    group_by(expect) |>
    mutate(p = 100 * n / sum(n)) |>
    arrange(Ozone) |>
    select(-n) |>
    pivot_wider(names_from = "expect", values_from = "p") |>
    mutate(diff = `1` - `0`) |>
    ggplot(aes(Ozone, diff)) +
    geom_point() +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()

rr |>
    unnest(dataset) |>
    count(expect, Temp, Ozone) |>
    group_by(expect) |>
    mutate(p = 100 * n / sum(n)) |>
    arrange(Ozone, Temp) |>
    select(-n) |>
    pivot_wider(names_from = "expect", values_from = "p") |>
    mutate(diff = `1` - `0`) |>
    ggplot(aes(Temp, diff)) +
    geom_point(size = 4) +
    geom_hline(yintercept = 0, lty = 2)

rr |>
    unnest(dataset) |>
    count(expect, Temp, Ozone) |>
    group_by(expect) |>
    mutate(p = 100 * n / sum(n)) |>
    arrange(Ozone, Temp) |>
    select(-n) |>
    pivot_wider(names_from = "expect", values_from = "p") |>
    mutate(diff = log2(`1`/ `0`)) |>
    ggplot(aes(Temp, Ozone)) +
    geom_point(aes(color = diff),
               size = 4) +
    scale_color_gradient2()


fit <- lm(Ozone ~ Temp, data = dat)
dat$hat <- hatvalues(fit)
dat |>
    ggplot(aes(Temp, Ozone)) +
    geom_point(aes(color = hat), size = 4) +
    scale_color_continuous(type = "viridis")


###############################################