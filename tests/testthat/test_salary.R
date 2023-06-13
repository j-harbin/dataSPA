## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
# These values are testing against manual calculations
#library(dataSPA)
library(testthat)
context("verify calculations")
data(salary)

test_that("salaryBar",
          {
            df <- plotSPA(salary=salary, id=1234, which="salaryBar", dataframe = TRUE)
            expect_equal(round(sum(df["2014-2015"]),0), 176840)

          }
)

test_that("salaryAllocation",
          {
            df2 <- plotSPA(salary=salary, id=1234, which="salaryAllocation", dataframe = TRUE)
            expect_equal(round(df2[[1]]$`BI-03`,0), 18660)
            expect_equal(round(df2[[1]]$`EG-04`,0), 88772)
            expect_equal(round(df2[[1]]$`EG-05`,0), 66418)
            expect_equal(round(df2[[1]]$`SE-RES-02`,0), 184)
            expect_equal(round(df2[[1]]$`PC-02`,0), 1576)
            expect_equal(round(df2[[1]]$`EG-03`,0), 1229)
          }
)

test_that("weekAllocation",
          {
            df3 <- plotSPA(salary=salary, id=1234, which="weekAllocation", dataframe = TRUE)
            expect_equal(df3[[1]]$`BI-03`, 10)
            expect_equal(df3[[1]]$`EG-04`, 45)
            expect_equal(df3[[1]]$`EG-05`, 30)
            expect_equal(df3[[1]]$`SE-RES-02`, 0.1)
            expect_equal(df3[[1]]$`PC-02`, 1)
            expect_equal(df3[[1]]$`EG-03`, 1)


          }
)

test_that("indeterminate",
          {
            df4 <- plotSPA(salary=salary, id=1234, which="indeterminate", dataframe = TRUE)
            expect_equal(df4[[1]]$indeterminate, 100)
            expect_equal(df4[[1]]$`Non-Indeterminate`, 0)

          }
)
