sessionInfo()
##=========
##array
vec1 <- c(5,9,3)
vec2 <- 10:15
array.example <- array(c(vec1, vec2), dim = c(3,3,2))

##=========
##data frame

rm(list = ls())
university.students <-  c('freshman', 'sophomore', 'junior', 'senior')
university.students <- c(university.students, 'graduated')
one.to.five <- 1:5
names.student <- c('jin', 'jennifer', ' mizki', 'hoki', 'suny')
graduated <- c(FALSE, FALSE, FALSE, FALSE, TRUE)
first.data.frame <- data.frame(one.to.five, university.students, names.student, graduated)
first.data.frame
first.data.frame2 <- data.frame(year = one.to.five, 
                                names = university.students, 
                                names.student = names.student, 
                                graduated = graduated)
first.data.frame2
class(first.data.frame$one.to.five)

##==========
## list: different data types can be hierarchically organized
mylist <- list(one.to.five, graduated, first.data.frame2)
mylist
mylist[[3]]

##==========
## index
university.students[5]
university.students[1:4]
first.data.frame2[1,2] #first row, second col
first.data.frame2[1,]
first.data.frame2[,2]
first.data.frame2[c(1,3), c(2,4)]

array.example[1,,2]

#control L to erase everything


##===========
save(mylist, file = "mylist.Rdata")
save(mylist, one.to.five, first.data.frame, file = "my2list.Rdata")

load(file = "mylist.Rdata")
load(file = "my2list.Rdata")

