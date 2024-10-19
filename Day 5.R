
## -----------------
## Control structure
## -----------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.of.packages <- c("plotly", "data.table", "haven", "tidyverse")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## use built-in dataset
data(mtcars)

require(tidyverse)
require(data.table)

## if / ifelse / elseif

values <- runif(100, 0, 1)

mean_v <-  mean(values)
mean_v
values[1]
if(values[1] < mean_v)
  print(paste0(values[1], " is less than average")) # combine the values[1] and the target
ifelse(values[1] < mean_v, print("YES"), print("NO"))
ifelse(values[1] < mean_v, TRUE, FALSE)

# if(sample(values, 1) < mean(values)) print(paste(values, "is less than average"))

ifelse(values[1] > mean(values[1]), print(paste(values[1], "is more than average")),
                              print(paste(values[1], " is less than average")))


for (i in 1:100) {
  ifelse(values[i] > mean(values), 
         print(paste(values[i], " is more than average")),
         print(paste(values[i], " is less than average")))
}


for (i in 1:100) {
  val <- (i / sqrt(i))^i / 10
  print(val)
}


set.seed(123)
sample(100:200, 10, FALSE)


## create binary variable x4
## based on the variable x2
## if it is greater than 150, assign 1,
## then assign 0 for otherwise

mydata <- data.frame(x1 = seq(1, 20, by = 2),
                     x2 = sample(100:200, 10, FALSE),
                     x3 = LETTERS[1:10]) %>% setDT
class(mydata)

mydata[, x4 := ifelse(x2 > 150, 1, 0)]
mydata
mydata$x4 <- ifelse(mydata$x2 > 150, 1, 0)

mydata <- mydata %>%
  mutate(x4 = ifelse(x2 > 150, 1, 0))

mydata[, x4 := ifelse(x2 > 150, 1, 0)]


example.data <- mtcars
setDT(example.data)
example.data[, mpg_cmprsn :=
                 ifelse(mpg > mean(mpg), 
                        "above_average", "below_average") %>% as.factor]


## based on mydata, create y:
## multiplying x1 by 2 when x3 contains A or B,
## if values are C or D, multiply by 3
## else multiply by 4

mydata[, y := ifelse(x3 %in% c("A", "B"), 
                     x1*2, 
                     ifelse(x3 %in% c("C", "D"), 
                            x1*3, 
                            x1*4))]

mydata %>%
  mutate(
    y = ifelse(x3 %in% c("A", "B"), x1 * 2,
          ifelse(x3 %in% c("C", "D"), x1 * 3, x1 * 4))
  )

## tidyverse has `case_when` for this:
mydata %>%
  mutate(y = case_when(x3 %in% c("A", "B") ~ x1 * 2,
                       x3 %in% c("C", "D") ~ x1 * 3,
                       TRUE ~ x1 * 4))

## data.table way: fcase (much like SQL case when type)
mydata[, y := fcase(x3 %in% c("A", "B"), x1 * 2,
                    x3 %in% c("C", "D"), x1 * 3,
                    !(x3 %in% c("A", "B", "C", "D")), x1 * 4)]



## for loop:

for (i in 1:10) {
  val <- (i / sqrt(i))^i / 100
  print(val)
}


values <- seq(1, 19, by = 2)
for (value in values) print(value)

for (value in seq_along(values)) print(value)


my_list <- list(c(5, 8, 2, 9),
                c('cat', 'dog', 'koala', 'panda', 'rabbit'),
                c(T, F, T),
                3.14)

for (item in my_list) item %>% length %>% print


## example:
example.data[, mpg_cmprsn := NULL][,]

mpg_cmprsn <- numeric(nrow(example.data))
for (i in seq_len(nrow(example.data))) {
  mpg_cmprsn[i] <-
    ifelse(example.data$mpg[i] > mean(example.data$mpg), "above_average", "below_average")
}

example.data[, mpg_cmprsn := mpg_cmprsn][,]

## another example:
mat <- example.data[, table(gear, carb)]

mateval <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))

for (i in 1:nrow(mat)) {
  for (j in 1:ncol(mat)) {
    mateval[i,j] <- mat[i,j] > 0
  }
}


## while & repeat (w/break):

while(i <= 10) {
  val <- (i / sqrt(i))^i / 100
  print(val)
  i <- i + 1 ## when omit, infinite operation!
}


i <- 1
repeat {
  val <- (i / sqrt(i))^i / 100
  print(val)
  i <- i + 1
  if (i > 10) { ## when omit, infinite operation!
   break
  }
}



## -----------------
## Custom functions
## -----------------

## create a standardized value
## for mpg, disp, & hp variables

## cf. standardized b/w 0 & 1 =
## (values - min) / (max - min)

require(tidyverse)
example.data <- mtcars
range(example.data$mpg)
(example.data$mpg - min(example.data$mpg)) / (max(example.data$mpg) - min(example.data$mpg))
  
mtcars <- mtcars %>% 
  mutate(mpg.std = mpg - min(mpg) / (max(mpg)-min(mpg)))

# Using for loop to repeat this process for other columns
# remove.na is just a variable, like you can also assign remove.na = 3, 5, etc.
var.std <- function(var, remove.na = TRUE) {
  max_var <-  max(var, na.rm = remove.na)
  min_var <-  min(var, na.rm = remove.na)
  std.var <-  (var - min_var) / (max_var - min_var)
  return(std.var)
}

var.std(c(0,1,0.5,0.5,2))

mtcars %>% 
  mutate(mpg.std = var.std(mpg),
         cyl.std = var.std(cyl),
         disp.std = var.std(disp))


example.data %>% 
mutate(
  std.mpg = std.0to1(mpg),
  std.cyl = std.0to1(cyl),
  std.disp = std.0to1(disp)
  ) %>% View


example.data <- example.data %>%
  mutate(std.mpg = (mpg - min(mpg, na.rm = T)) / (max(mpg, na.rm = T) - min(mpg, na.rm = T)),
         std.cyl = (cyl - min(cyl, na.rm = T)) / (max(cyl, na.rm = T) - min(cyl, na.rm = T)),
         std.disp = (disp - min(disp, na.rm = T)) / (max(disp, na.rm = T) - min(disp, na.rm = T)))


#the flight dataset
example.data2 %>% 
  mutate(air_time.std = var.std(air_time)) %>% 
  View

# another function, be careful of the variables that have missing values
variance <- function(var, remove.na = TRUE) {
  var <- var[!(is.na(var))]
  mean_var <- mean(var, na.rm = remove.na)
  n <- length(var)
  variance <-  sum((var - mean_var)^2) / (n-1)
}

variance(test)
# mini-test 
test <- c(1,2,NA)
valid_value <- test[!(is.na(test))]
valid_value


firstnames <- c('Joris', 'Carolien', 'Koen')
lastname  <- 'Meys'
paste(firstnames, lastname, sep = " ")



## custom function to covert values
## between 0 and 1 range

std.0to1 <- function(vector, na.rm) {
  std.vector <- (vector - min(vector, na.rm = na.rm)) / (max(vector, na.rm = na.rm) - min(vector, na.rm = na.rm))
  return(std.vector)
} 


std.0to1 <- function(var, na.remove = T) {
  ## min(var)
  min.var <- min(var, na.rm = na.remove)

  ## max(var)
  max.var <- max(var, na.rm = na.remove)

  ## return(result)
  std.var <- (var - min.var) / (max.var - min.var)
  return(std.var)
}

## use the function
example.data <-
  example.data %>%
  mutate(std.mpg = std.0to1(mpg, T),
         std.disp = std.0to1(disp, T),
         std.hp = std.0to1(hp, T))

#change the data porperty to data frame
example.data <- as.data.frame(example.data)
# important: looping through the whole dataset


var_list <- colnames(example.data)
# i <- "mpg"


var_function <- function(var, data) {
  vec <- data[, var]
  out <- var(vec)
  print(out)
}


#lapply function
var_function("mpg", example.data)

#apply a function to a list
lapply(var_list, var_function, data = example.data)


var_list %>% 
  lapply(var_function, data = example.data)

## another example:
## let's find the variance of a vector

variance <- function(vector, na.rm) {
  non_na_vec <- vector[!(is.na(vector))]
  n <- length(non_na_vec)
  temp <- non_na_vec - mean(vector, na.rm = na.rm)
  temp_sq <- temp^2
  sum_sq <- sum(temp_sq)
  variance <- sum_sq / (n-1)
  return(variance)
}

std.var <- function(vars, data = glbwarm) {
  vars <- vars[!(na.rm(vars))]
}




find.mean <- function(vars, na.remove) {
  total <- sum(vars, na.rm = na.remove)
  non.na.vars <- vars[!(is.na(vars))]
  n <- length(non.na.vars)
  average <- total/n
  return(average)
}

## compare w/ built-in function
example.data %>%
    summarize(own.fun.var = variance(mpg),
              builtin.fun.var = var(mpg))


example.data %>%
  summarize(mean(mpg), 
            mean(cyl),
            mean(disp), 
            mean(hp), 
            mean(drat),
            mean(wt),
            mean(qsec),
            mean(vs),
            mean(am),
            mean(gear),
            mean(carb))
















## linear regression model example:
glbwarm <- read_spss("glbwarm.sav")

  ## visual representation of OLS regression
  ggplot(glbwarm, aes(x = negemot, y = govact)) +
    geom_point(color = "black", alpha = 0.1) +
    geom_smooth(method = "lm", se = T, color = "red") +
    xlab("Negative emotion") + ylab("Support for goverment action") +
    ggtitle("Scatterplot")

  ## OLS regression
  model <- lm(govact ~ negemot, data = glbwarm)
  summary(model)

  ## options(scipen = 999);  summary(model)

  ## minimize SS_residual
  check.lm.fit <- function(intercept, b_coeff) {
    Y <- glbwarm$govact
    X <- glbwarm$negemot
    Y_hat <- intercept + b_coeff*X
    residuals <- Y - Y_hat
    SS_sq <- sum(residuals^2)
    return(SS_sq)
  }

cond <- expand.grid(
  intercept = c(seq(-3, 2.75, by = 0.1), 2.75732, seq(3, 5, by = 0.01)),
  b_coeff = c(seq(-2, 0.5, by = 0.1), 0.51424, seq(0.6, 1.5, by = 0.01)))

SSresidual <- mapply(check.lm.fit,
                     intercept = cond$intercept,
                     b_coeff = cond$b_coeff)


## using plot_ly
require(plotly)
plot_ly(type = "mesh3d",
        opacity = 0.5,
        x = cond$intercept,
        y = cond$b_coeff,
        z = SSresidual
) %>%
  layout(title = 'SS residual',
         scene = list(
           xaxis = list(title = 'intercept'),
           yaxis = list(title = 'b_coeff'),
           zaxis = list(title = "SS residual")
         ))



## function scoping rule

f <- function(a, b) {
  (a * b) / tidyr::table2[1, 4]
}

z <- 2
f(a = 10, b = 5)


## apply a function over a list of column

example.data %>%
  summarize(var(mpg),
            var(cyl),
            var(disp),
            var(hp),
            var(drat),
            var(wt),
            var(qsec),
            var(vs),
            var(am),
            var(gear),
            var(carb),
            var(std.mpg),
            var(std.disp),
            var(std.hp))

## instead of copy-paste,
## let's loop through variable list


# dont use this 
var_list <- colnames(example.data)


lapply(var_list, var_function, data = example.data)
sapply(var_list, var_function, data = example.data)


lapply(var_list, function(var, data = example.data) {
  vec <- data[, get(var)]
  out <- var(vec)
  print(out)
})



x <- list(a = 1:5, b = rnorm(10), c = rbinom(1:54, 100, 0.5))
lapply(x, mean)


x <- 1:5
lapply(x, runif)


## function w/ additional arguments
x <- list(a = 1:5,
          b = sample(c(rnorm(10), NA), 20, replace = T),
          c = rbinom(54, 100, 0.5))
lapply(x, mean, na.rm = T)


## more example:
lapply(example.data, var)
sapply(example.data, var)

apply(example.data,
      MARGIN = 2,
      FUN = mean)

apply(example.data,
      MARGIN = 1,
      FUN = mean)


## col/row sums and means: rowMeans and rowSums
example.data[, mpg_cmprsn := NULL][,]

colMeans(example.data)
colSums(example.data)

news <- readRDS(
  url("https://github.com/MrKevinNa/MrKevinNa.github.io/raw/master/data/naver_baseball_news_2018.RDS")
  ) %>% tibble

rowSums(is.na(news)) ## no. of missing variables per case: same as `apply(is.na(news), 1, sum)`


## ---------------------------
## example analysis:
## television rating analysis
## ---------------------------

tvratings <-
  readRDS(
    url("https://github.com/revelunt/example_tvratings_data/raw/main/tvratings.RDS")
  ) %>% tibble

tvratings_description <-
  read_csv(
    url("https://github.com/revelunt/example_tvratings_data/raw/main/variable%20description.csv")
  )

require(data.table)
setDT(tvratings)

tvratings[, table(CHNNEL_NM)] %>% as_tibble
tvratings[, table(PROGRM_GENRE_LCLAS_NM)] %>% as_tibble
tvratings[, table(PROGRM_GENRE_MLSFC_NM)] %>% as_tibble
tvratings[, table(PROGRM_GENRE_SCLAS_NM)] %>% as_tibble

## average ratings:
require(tidyverse)
tvratings <- tvratings %>%
  mutate(avrg_ratings = rowMeans(across(MALE_4_9YO_WTCHNG_RT:FEMALE_N60S_ABOVE_WTCHNG_RT)),
         sum_ratings = rowSums(across(MALE_4_9YO_WTCHNG_RT:FEMALE_N60S_ABOVE_WTCHNG_RT)))

## average ratings per genre
tvratings %>%
  group_by(PROGRM_GENRE_SCLAS_NM) %>%
  summarize(mean_rts = mean(sum_ratings),
            mean_MALE49 = mean(MALE_4_9YO_WTCHNG_RT),
            mean_MALE_N10s = mean(MALE_N10S_WTCHNG_RT))

## which is the most popular?
tvratings %>%
  group_by(PROGRM_NM) %>%
  summarize(mean_rts = mean(sum_ratings)) %>%
  arrange(desc(mean_rts))

## popular by genre?
tvratings %>%
  group_by(PROGRM_GENRE_SCLAS_NM, PROGRM_NM) %>%
  summarize(mean_rts = mean(sum_ratings))

## male/female average
tvratings %>%
  mutate(male_average =
           rowMeans(
             across(MALE_4_9YO_WTCHNG_RT:MALE_N60S_ABOVE_WTCHNG_RT)),
         female_average =
           rowMeans(
             across(FEMALE_4_9YO_WTCHNG_RT:FEMALE_N60S_ABOVE_WTCHNG_RT)
           ))

##
tvratings[, .(PROGRM_NM, BRDCST_TME_NM, sum_rating)] %>%
  group_by(PROGRM_NM) %>%
  filter(sum_rating == max(sum_rating))

##
tvratings %>%
  select(PROGRM_NM, BRDCST_TME_NM, sum_rating) %>%
  group_by(PROGRM_NM) %>%
  filter(sum_rating == max(sum_rating))


