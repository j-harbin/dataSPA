## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
# These values are testing against manual calculations
#library(dataSPA)
library(testthat)
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
            expect_equal(df2[[1]]$`Field Travel`, 15000)
            expect_equal(df2[[1]]$`Training, domestic conferences`, 1000)
            expect_equal(df2[[1]]$`Field Equipment`, 4000)
            expect_equal(df2[[1]]$`IM/IT - computers, hardware, software`, 1000)
            expect_equal(df2[[1]]$Field, 18000)
            expect_equal(df2[[1]]$Contracts, 65000)
            expect_equal(df2[[1]]$Translation, 1000)
            expect_equal(df2[[1]]$`Vessels, Boats`, 500000)
          }
)

test_that("omAllocationGeneral",
          {
            df3 <- plotSPA(om=om, id=1234, which="omAllocationGeneral", dataframe = TRUE)
            expect_equal(df3[[1]]$Travel, 16000)
            expect_equal(df3[[1]]$`Equipment Purchase`, 5000)
            expect_equal(df3[[1]]$`Material and Supplies`, 18000)
            expect_equal(df3[[1]]$`Contracts, Leases, Services`, 566000)

          }
)

