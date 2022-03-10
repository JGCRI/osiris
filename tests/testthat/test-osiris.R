context("osiris Tests")
library(osiris); library(testthat); library(dplyr)

test_that("placeholder test", {
  tVal1 <- 2
  testthat::expect_gt(tVal1,0)
})
