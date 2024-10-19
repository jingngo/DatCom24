
## Day 4
## Data I/O and advanced issues in data wrangling
require(tidyverse)
require(tidyr)
require(data.table)
table1
table2
table3
table4a
table4b
table4a_copy <- table4a
setDT(table4a_copy)

melt(table4a_copy, id.vars = "country",
     )



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## install required packages if not installed
required.packages <- c("rio", "readr", "tidyverse", "data.table",
                       "nycflights13", "readxl", "xlsx")

install.required <- !(required.packages %in% installed.packages()[,"Package"])
if(length(required.packages[install.required]) > 0)
  install.packages(required.packages[install.required], dependencies = T)

library(data.table)
library(readr)
library(tidyverse)
library(rio)
library(haven)
library(readxl)
library(xlsx)

## read-in data
data <- read_sav("TwitterAf.sav")

## using rio
twitter <- import("TwitterAf.sav")
covid <- import("covid.csv")

## using readr
covid <- read_csv("covid.csv")
dim(covid)
str(covid)


## working with tidy data
options(scipen = 999)

table1 %>%
  mutate(rate = cases / population * 10000)

table1 %>%
  count(year, wt = cases)


## working with non-tidy data, table2
require(data.table)
table2copy <- table2
setDT(table2copy)

cases <- table2copy[type == "cases", count]
pop <- table2copy[type == "population", count]

table2copy[, .(country, year)] %>%
  unique %>% ## unique returns unique cases defined by rows
  cbind(., rate = cases / pop *10000)

table2 %>%
  group_by(country, year) %>%
  mutate(rate = count/lead(count)*10000) %>%
  na.omit %>% select(country, year, rate)


## working with non-tidy data, table4a + table4b
rate_1999 <- table4a[, 2] / table4b[, 2] * 10000
rate_2000 <- table4a[, 3] / table4b[, 3] * 10000

country <- c("Afghanistan", "Brazil", "China",
             "Afghanistan", "Brazil", "China")
year <- c(1999, 1999, 1999, 2000, 2000, 2000)

# data.frame(country = rep(table4a$country, 2),
#            year = rep(c(1999, 2000), 3),
#            rate = unlist(table4a[, -1] / table4b[, -1] * 10000))


rate_data <-
  data.frame(country,
             year = year,
             rate = c(rate_1999$`1999`, rate_2000$`2000`))


## pivoting tables
table4a %>%
  pivot_longer(c(`1999`, `2000`),
               names_to = "year",
               values_to = "cases")

library(data.table)

table4a_copy <- table4a
setDT(table4a_copy)

table4a_copy <-
  melt(table4a_copy,
       id.vars = "country",
       variable.name = "year",
       value.name = "cases")
columns(table4a)
table4b %>%
  pivot_longer(c(`1999`, `2000`),
               names_to = "year",
               values_to = "populations")

table4b_copy <- table4b
setDT(table4b_copy)

table4b_copy <-
  melt(table4b_copy,
       id.vars = "country",
       variable.name = "year",
       value.name = "populations")


## joining tables
library(nycflights13)
data(flights)
data(airlines)

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

View(flights2)
View(airlines)
head(airlines)

#larger dataset called first, smaller dataset called later 
flights2 %>%
  inner_join(airlines, by = "carrier")

#changing the position of these two tables
joined_table <- airlines %>% 
  inner_join(flights2, by = "carrier")
joined_table
## example: table4a and table4b
table4a_copy <-
  table4a %>%
  pivot_longer(c(`1999`, `2000`),
               names_to = "year",
               values_to = "cases")

table4b_copy <-
  table4b %>%
  pivot_longer(c(`1999`, `2000`),
               names_to = "year",
               values_to = "populations")


table4a_copy %>%
  inner_join(table4b_copy,
             by = c("country", "year")) %>%
  mutate(rate = cases / populations * 1000)


## example: matching 2 columns that have the same values but different variable names on the fly
View(flights2)
data(airports)
View(airports)

left_joined_table <- left_join(flights, airports, by = c("origin" = "faa"))


flights2 %>%
  left_join(airports, c("dest" = "faa")) %>%

flights2 %>%
  left_join(airports, c("origin" = "faa"))

