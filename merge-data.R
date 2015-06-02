# From Deducer:

# Let's create 2 random variables with the same variable "subject.id":
demographics<-data.frame(subject.id = 1:10,
                         age = round(rnorm(10,40,10)),
                         gender = rep(c("male","female"),5))

visit.data<-data.frame(subject.id = sort(c(1:10,1:10,1:10)),
                       week = rep(c(1:3),10),
                       outcome1 = rnorm(30),
                       outcome2 = rnorm(30))

# Now run the following commands
demographics.temp <- demographics[setdiff(colnames(demographics), c())]

visit.data.temp <- visit.data[setdiff(colnames(visit.data), c())]

result <- merge(demographics.temp, visit.data.temp, by.x = c("subject.id"),
                by.y = c("subject.id"), incomparables = NA,
                all.x = TRUE, all.y = TRUE)

rm(list=c("demographics.temp", "visit.data.temp"))

# If you want to, say, drop unmatched cases of y and keep all cases of x,
# just change to
all.x = TRUE, all.y = FALSE)
