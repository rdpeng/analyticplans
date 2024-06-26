library(tidyverse)
library(broom)
library(splines)
library(lubridate)


## Process smoke wave/wildfire data
load("data/smoke_wave_species.rda") ## 'm2'
species <- m2 |>
    rename(smoke = SW111.x, date = date.x, fips = fips.x) |>
    mutate(fips = formatC(fips, flag = "0", width = 5)) |>
    rename_with(~ sub("\\.PM2\\.5\\.LC$|\\.PM2\\.5\\.LC.TOR$", "", .x)) |>
    mutate(include.spec = if_all(c(Aluminum:Zirconium, -Sulfur),
                                 ~ !is.na(.x)),
           include.smoke = !is.na(smoke)) |>
    as_tibble() |>
    as.condition_dataframe() |>
    condition_add(function(d) all(c("smoke", "date", "fips",
                                    "dow", "tmpd", "dptp") %in% names(d))) |>
    condition_add(function(d) with(d, all(smoke >= 0, na.rm = TRUE))) |>
    warn_object()

## Health data
fipsList <- unique(species$fips)
fileList <- sprintf("data/MCAPS/%s.rds", fipsList)
use <- file.exists(fileList)
fileList <- fileList[use]

m <- lapply(fileList, function(infile) {
    infile |>
        readRDS() |>
        rename(cardio = ALL_CVD, resp = ALL_Resp) |>
        dplyr::select(date, denom, cardio, resp) |>
        filter(date >= "2004-01-01" & date <= "2009-12-31") |>
        group_by(date) |>
        ## summarize_at(vars(denom:resp), sum) |>
        summarize(across(denom:resp, sum)) |>
        mutate(dow = factor(weekdays(date))) |>
        ungroup()
})
names(m) <- fipsList
mb <- bind_rows(m, .id = "fips")


mcaps0 <- mb |>
    left_join(select(species, -dow),
              by = c("fips", "date")) |>
    filter(include.smoke) |>
    mutate(season = factor(quarter(date)),
           year.f = factor(year(date))) |>
    select(-Sulfur)

## Base Model on mcaps0

## Expectation is that coef for 'smoke' is > 0 and significant based on Coco's
## previous published work

fit <-
    # resp is a count >= 0
    # denom is a count > 0
    # smoke is 0/1
    # dow is a factor
    # fips is a factor
    # tmpd is numeric >= 0 & <= 120
    # dptp is numeric >= 0 & <= 120
    # date ranges from 2004 to 2009
    # N >= 60000
    glm(resp ~ offset(log(denom)) + smoke
        + dow + fips
        + ns(tmpd, 3) + ns(dptp, 3)
        + ns(date, 6 * 6),
        data = mcaps0, family = poisson)
fit |>
    tidy() |>
    # estimate is >= 0 & <= 0.1
    # p.value < 0.05
    filter(term == "smoke")

## Dataset needed to fit the model
mcaps0 |>
    select(resp, denom, smoke, dow, fips, tmpd, dptp, date)

check <- function(m) {
    with(m, {
        stopifnot(is.integer(resp))
        stopifnot(all(resp >= 0))
        stopifnot(is.integer(denom))
        stopifnot(all(denom > 0))
        stopifnot(all(smoke %in% c(0, 1)))
        stopifnot(is.character(dow) | is.factor(dow))
        stopifnot(is.character(fips) | is.factor(fips))
        stopifnot(is.numeric(tmpd))
        stopifnot(all(tmpd >= 0, na.rm = TRUE))
        stopifnot(all(tmpd <= 120, na.rm = TRUE))
        stopifnot(is.numeric(dptp))
        stopifnot(all(dptp >= -15, na.rm = TRUE))
        stopifnot(all(dptp <= 120, na.rm = TRUE))
        stopifnot(all(lubridate::year(date) >= 2004
                      & lubridate::year(date) <= 2009))
    })
    stopifnot(nrow(m) >= 60000)
    TRUE
}
check(mcaps0)
