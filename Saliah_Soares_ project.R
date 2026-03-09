setwd("/Users/saliahsoares/Desktop")
library(readr)
ukcpr <-read.csv("ukcpr_national_welfare_data.csv")
ukcpr2 <- ukcpr[, c("state_name",
                       "state_fips",
                       "year",
                       "FederalMinimumWage",
                       "StateMinimumWage")]
ukcpr_2000_2019 <- subset(ukcpr2, 
                          year >= 2000 & year <= 2019 &
                            !(state_name %in% c("Alaska", "Hawaii", "District of Columbia"))
)
write.csv(ukcpr_2000_2019,
          "national_welfare_data_2000_2019.csv",
          row.names = FALSE)
ukcpr_2000_2019$higher_state_mw <- ifelse(
  ukcpr_2000_2019$StateMinimumWage > ukcpr_2000_2019$FederalMinimumWage,
  1,
  0
)
data_2017 <- subset(ukcpr_2000_2019, year == 2017)
sum(data_2017$higher_state_mw)
x <- c(7,11,3) 
if ((x[1] >= x[2] & x[1] <= x[3]) | (x[1] <= x[2] & x[1] >= x[3])) {
  median_3 <- x[1]
} else if ((x[2] >= x[1] & x[2] <= x[3]) | (x[2] <= x[1] & x[2] >= x[3])) {
  median_3 <- x[2]
} else {
  median_3 <- x[3]
}

median_3
x <-rnorm(20, mean = 0, sd =1)
count <- 0
for (i in 1:length(x)) {
  if (x[i] > 0) {
    count <- count + 1
  }
}
x <- matrix(data=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), nrow=4, ncol=4)
x
diag_sum <- 0
for (i in 1:nrow(x)) {
  diag_sum <- diag_sum + x[i, i]
}
setwd("/Users/saliahsoares/Desktop")
library(readr)
GDP<-read.csv("GDP_per_capita.csv")
install.packages("tidyr") 
library(tidyr)
library(tidyverse)
north.america <- subset(gdp, Country.Names %in% c("United States", "Canada", "Mexico"))
north.america.1 <- north.america[, c(
  "Country.Name", "Country.Code",
  "X2000","X2001","X2002","X2003","X2004",
  "X2005","X2006","X2007","X2008","X2009","X2010"
)]
north.america.2 <- north.america[, c(
  "Country.Name", "Country.Code",
  "X2011","X2012","X2013","X2014","X2015","X2016",
  "X2017","X2018","X2019","X2020","X2021","X2022"
)]
north.america.merged <- merge(
  north.america.1,
  north.america.2,
  by = c("Country.Name", "Country.Code")
)
