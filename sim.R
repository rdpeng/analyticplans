## Simulate plan

library(tidyverse)

simdata <- function(n) {
    k <- 0.1
    rnorm(n, 2, 1)
    #c(rnorm(floor(k * n), 0, 1), rnorm(floor((1-k) * n), 0, 5))
    # c(rnorm(floor(n * (1-k)), 0, 1),
    #   runif(ceiling(k * n), 6, 10))
    # #rt(n, 3)
}

plan <- function(x) {
    pos <- mean(x >= 0)
    tests <- c(
        is.numeric(x),
        length(x) >= qnorm(1-.01/2)^2 * 1,
        between(pos, .4, .6)
        ## shapiro.test(x)$p.value > 0.01
    )
    all(tests)
}

replicate(10000, {
    plan(simdata(10))
}) |>
    mean()

outcome <- function(x) {
    mean(x) >= -1 && mean(x) <= 1
}

replicate(10000, {
    outcome(simdata(10))
}) |>
    mean()

evalplan <- function(x, plan) {
    rp <- plan(x)
    ro <- outcome(x)
    c(plan = rp, outcome = ro)
}

m <- replicate(10000, {
    x <- simdata(10)
    evalplan(x, plan)
}) |>
    t() |>
    as_tibble() |>
    mutate(plan = factor(plan, levels = c(FALSE, TRUE)),
           outcome = factor(outcome, levels = c(FALSE, TRUE))) |>
    table()
m

round(
    c(
        ## Specificity
        Spec = m[1,1] / sum(m[,1]),

        ## Specifity
        NPV = m[1,1] / sum(m[1,]),

        ## Sensitivity
        Sens = m[2,2] / sum(m[,2]),

        ## Sensitivity
        PPV = m[2,2] / sum(m[2,])
    ), 3
)

