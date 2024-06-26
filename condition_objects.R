## Condition objects

library(tidyverse)

condition_add <- function(x, ...) {
    UseMethod("condition_add")
}

condition_add.condition_object <- function(x, f) {
    funs <- attr(x, "conditions")
    funs <- append(funs, list(f))
    attr(x, "conditions") <- funs
    x
}

condition_get <- function(x, ...) {
    UseMethod("condition_get")
}

condition_get.condition_object <- function(x, ...) {
    attr(x, "conditions")
}

check_object <- function(x, ...) {
    UseMethod("check_object")
}


check_object.condition_object <- function(x, ...) {
    funs <- attr(x, "conditions")
    results <- map_lgl(funs, function(f) f(x))
    all(results)
}

check_object.default <- function(x, ...) {
    if(is.function(x))
        return(TRUE)
    FALSE
}

stop_object <- function(x, ...) UseMethod("stop_object")

stop_object.condition_object <- function(x, ...) {
    funs <- attr(x, "conditions")
    results <- map_lgl(funs, function(f) f(x))
    if(!all(results))
        stop("some checks did not pass")
    x
}

stop_object.default <- function(x, ...) {
    x
}

warn_object <- function(x, ...) UseMethod("warn_object")

warn_object.condition_object <- function(x, ...) {
    funs <- attr(x, "conditions")
    results <- map_lgl(funs, function(f) f(x))
    if(anyNA(results))
        warning("some checks produced NA values")
    else if(!all(results))
        warning("some checks did not pass")
    x
}

warn_object.default <- function(x, ...) {
    x
}

condition_numeric <- function(length = 0L) {
    structure(numeric(length),
              class = c("condition_numeric", "condition_object", "numeric"),
              conditions = list())
}

as.condition_numeric <- function(x, ...) {
    UseMethod("as.condition_numeric")
}

as.condition_numeric.numeric <- function(x, conditions = list(), ...) {
    structure(x,
              class = c(class(x), "condition_numeric",
                        "condition_object", "numeric"),
              conditions = conditions)
}

as.condition_dataframe <- function(x, ...) {
    UseMethod("as.condition_dataframe")
}

as.condition_dataframe.data.frame <- function(x, conditions = list(), ...) {
    structure(x,
              class = c(class(x), "condition_dataframe",
                        "condition_object", "data.frame"),
              conditions = conditions)
}

diagnose <- function(x, ...) {
    UseMethod("diagnose")
}

diagnose.condition_object <- function(x, ...) {
    funs <- attr(x, "conditions")
    results <- map_lgl(funs, function(f) identical(TRUE, f(x)))
    funs[!results]
}

callback_name <- "condition_checking_callback"

strict_checking <- local({
    function(val) {
        if(missing(val))
            stop("argument 'val' must be 'TRUE' or 'FALSE'")
        if(val)
            res <- addTaskCallback(function(expr, value, ok, visible) {
                obj <- ls(globalenv()) |>
                    setdiff("callback_name")
                for(name in obj) {
                    x <- get(name, globalenv())
                    if(!check_object(x)) {
                        message(sQuote(name), " does not pass all checks")
                    }
                }
                TRUE
            }, name = callback_name)
        else {
            res <- removeTaskCallback(callback_name)
            if(!res)
                warning("problem removing task callback")
        }
    }
})

all_positive <- function(x) {
    isTRUE(all(x >= 0))
}

no_NA_values <- function(x) {
    isTRUE(all(!is.na(x)))
}

condition_clear <- function(x, ...) {
    UseMethod("condition_clear")
}

condition_clear.condition_object <- function(x, ...) {
    attr(x, "conditions") <- list()
    x
}

################################################################################
## Data Frames

# strict_checking(TRUE)

dat <- airquality |>
    filter(!is.na(Ozone)) |>
    as.condition_dataframe() |>
    filter(!is.na(Solar.R)) |>
    condition_add(function(x) with(x, isTRUE(all(!is.na(Ozone) & Ozone >= 0)))) |>
    condition_add(function(x) with(x, isTRUE(all(Wind >= 0)))) |>
    condition_add(function(x) with(x, isTRUE(all(Temp >= 0)))) |>
    condition_add(function(x) with(x, isTRUE(all(Solar.R >= 0)))) |>
    condition_add(function(x) with(x, isTRUE(all(!is.na(Solar.R))))) |>
    condition_add(function(x) {
        with(x, isTRUE(cor(Wind, Temp) < 0))
    })

dat |>
    stop_object() |>
    filter(!is.na(Ozone)) |>
    stop_object() |>
    filter(!is.na(Wind)) |>
    stop_object() |>
    filter(Wind < quantile(Wind, .9)
           & Wind > quantile(Wind, 0.1)) |>
    stop_object() |>
    filter(between(Temp, quantile(Temp, 0.05),
                   quantile(Temp, 0.95))) |>
    stop_object() |>
    summarize(m = mean(Wind))


################################################################################
## Univariate

diagnose(dat)

x <- as.condition_numeric(rnorm(100))
stop_object(x)
x

y <- as.condition_numeric(10)

x <- x |>
    condition_clear() |>
    condition_add(no_NA_values) |>
    condition_add(local({
        checked <- FALSE
        result <- FALSE
        function(x) {
            if(checked)
                return(result)
            hist(x)
            checked <<- TRUE
            r <- readline("Looks okay? (Y/N) ")
            r <- toupper(r)
            result <<- identical(r, "Y")
            result
        }
    }))

check_object(x)


x[4] <- NA
check_object(x)

x[4] <- .1
check_object(x)

x


################################################################################
## Univariate scenarios with mean


set.seed(2024-06-01)

## Expected range is [-a, a]
a <- 2

n <- 5
x <- rnorm(n, 0, 1)
mean(x)

x1 <- rnorm(n, 0, 1)
x1[5] <- 25
mean(x1)

x2 <- rnorm(n, 5, 1)
mean(x2)

x3 <- rnorm(n, 0, 3)
x3[1] <- -15
x3[2] <- 15

x4 <- rnorm(n, -3, 1)
x4[which.max(x4)] <- 35
mean(x4)

x5 <- rnorm(n, -5, 1)
x5[which.max(x5)] <- 20
mean(x5)

x6 <- rnorm(n, 5, 7)
mean(x6)

x7 <- rnorm(n, 5, 1)
x7[which.max(x7)] <- -20
mean(x7)

levels <- c(0, 1, 2, -1, 3, -2, 4, -3)
dat <- tibble(x = c(x, x1, x2, x3, x4, x5, x6, x7),
              id = factor(rep(levels, each = n)))
dat

ds <- dat |>
    split(dat$id)
ds

d <- ds[[sample(length(ds), 1)]] |>
    as.condition_dataframe() |>
    condition_add(function(d) nrow(d) == 5L) |>
    condition_add(function(d) isTRUE(sd(d$x) < 2)) |>
    condition_add(function(d) with(d, sum(x > mean(x)) != 1L)) |>
    condition_add(function(d) with(d, sum(x < mean(x)) != 1L))

d |>
    warn_object() |>
    summarize(m = mean(x))

diagnose(d)

d


################################################################################
## Scatterplot scenarios

airquality |>
    slice_sample(prop = 0.5) |>
    ggplot(aes(Wind, Ozone)) +
    geom_point()

d <- airquality |>
    filter(!is.na(Ozone)) |>
    as.condition_dataframe() |>
    condition_clear() |>
    condition_add(function(d) with(d, isTRUE(all(between(Wind, 0, 30))))) |>
    condition_add(function(d) with(d, isTRUE(all(between(Ozone, 0, 170))))) |>
    condition_add(function(d) {
        b <- lm(Ozone ~ Wind, data = d) |>
            coef()
        isTRUE(b[2] < 0)
    }) |>
    warn_object()


diagnose(d)

d <- tibble(Wind = 5 + rnorm(100),
       Ozone = 10 + 2 * (Wind - 5) + 1.5*(Wind - 5)^2 + rnorm(100, sd = 2)) |>
    as.condition_dataframe() |>
    condition_add(function(d) {
        with(d, all(Wind >= 0 & Wind < 10))
    }) |>
    condition_add(function(d) {
        r <- lm(Ozone ~ Wind, data = d) |>
            resid()
        isTRUE(shapiro.test(r)$p.value > 0.05)
    }) |>
    warn_object()

diagnose(d)
