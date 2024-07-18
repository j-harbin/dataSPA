library(dataSPA)
library(ssh)

path <- "//dcnsbiona01a/BIODataSVC/IN/MSP/PowerBI-Projects/dataSPA/"

om <- getData(type="om", cookie=cookie, age=0,keep = TRUE,path=path)
sal <- getData(type="salary", cookie=cookie, age=0,keep = TRUE,path=path)
