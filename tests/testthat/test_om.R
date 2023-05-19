## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(dataSPA)
library(testthat)
context("verify calculations")
data(om)

#test_that("omBar + omPie",
#          {
#              df <- plotSPA(om, which="omBar", dataframe = TRUE, id=1234)
#              expect_equal(sum(df["2021-2022"]), 640000) # Total amount of first year money
#
#          }
#)

