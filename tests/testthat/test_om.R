## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
# These values are testing against manual calculations
#library(dataSPA)
library(testthat)
library(dataSPA)
context("verify calculations")
data(om)

test_that("omBar + omPie",
          {
            df <- plotSPA(om=om, id=1234, which="omBar", dataframe = TRUE)
            expect_equal(sum(df["2014-2015"]), 605000)

          }
)

test_that("omAllocation",
          {
            df2 <- plotSPA(om=om, id=1234, which="omAllocation", dataframe = TRUE)
            expect_equal(df2[[1]], 15000)
            expect_equal(df2[[2]], 1000)
          }
)

