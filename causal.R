## Mediator
x <- rnorm(100)
z <- x + rnorm(100)
y <- z + rnorm(100)

lm(y ~ x + z) |>
    summary()

lm(y ~ x) |>
    summary()

lm(y ~ x) |>
    confint()

lm(z ~ x) |>
    summary()

## Collider
x <- rnorm(100)
y <- x + rnorm(100)
z <- 0.45*x + 0.77 * y + rnorm(100)

lm(y ~ x + z) |>
    summary()

lm(y ~ x + z) |>
    confint()

lm(y ~ x) |>
    summary()

lm(z ~ x + y) |>
    summary()

## Confounder
z <- rnorm(100)
x <- z + rnorm(100)
y <- 0.5 * x + z + rnorm(100)

lm(y ~ x + z) |>
    summary()

lm(z ~ x + y) |>
    summary()

