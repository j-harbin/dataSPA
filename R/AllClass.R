# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' A Package for Obtaining Data for DFO Strategic Planning
#'
#' The `dataSPA` package is intended to be used by Department
#' Of Fisheries and Oceans Canada (DFO) employees to obtain data
#' from A) the Project Planning Tool (PPT) API and B) Salary
#' Spread sheets to plot information related to allocation of
#' funding.
#'
#' The sketch given below illustrates the typical workflow with the package
#'
#' \if{html}{\figure{workflow.png}{options: width="300" alt="Figure: workflow.png"}}
#' @importFrom methods new
#' @name dataSPA-package
#' @docType package
NULL

#' A pay scale spread sheet
#'
#' This includes salary for different levels of different
#' positions in different years. This information is used
#' to calculate median salaries spent on projects to ensure
#' confidentiality.
#'
#' @examples
#' library(dataSPA)
#' data(salaries)
#' head(salaries,5)
#' @name salaries
NULL

#' An example project including o&m information
#'
#' This includes a sample of O&M data obtained from an
#' imaginary project. The purpose of this is for the user
#' to get familiar with the structure of the data,
#' and to allow the developers to validate their calculations
#' in a suite of tests.
#'
#' @examples
#' \dontrun{
#' library(dataSPA)
#' data(om)
#' plotSPA(om=om, which="omBar", id=1234)
#' }
#' @name om
NULL

#' An example project including salary information
#'
#' This includes a sample of salary data obtained from an
#' imaginary project. The purpose of this is for the user
#' to get familiar with the structure of the data,
#' and to allow the developers to validate their calculations
#' in a suite of tests.
#'
#' @examples
#' \dontrun{
#' library(dataSPA)
#' data(salary)
#' plotSPA(salary=salary, which="salaryAllocation", id=1234)
#' }
#' @name salary
NULL

