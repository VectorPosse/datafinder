## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE-----------------------------------
library(datafinder)
library(mosaic)     #For prop.test later on

## ---- warning = FALSE, message = FALSE-----------------------------------
tail(discover_packages(), 10)

## ------------------------------------------------------------------------
explore_package(MASS)[7:13,]

## ---- fig.width = 5, fig.height = 5--------------------------------------
visualize_dataframe(Melanoma)

## ---- fig.width = 5, fig.height = 5--------------------------------------
cols <- c("status", "sex", "ulcer")
Melanoma[cols] <- lapply(Melanoma[cols], factor)
rm(cols)
levels(Melanoma$status) <- c("died from melanoma", "other", "other")
visualize_dataframe(Melanoma)

## ------------------------------------------------------------------------
status_sex_test <- prop.test(status ~ sex, data = Melanoma)
status_sex_test$statistic
status_sex_test$p.value

