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
            expect_equal(round(df2[[1]],0), 18660)
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
            expect_equal(df4[[1]]$Indeterminate, 100)
            expect_equal(df4[[1]]$`Non-indeterminate`, 0)

          }
)
