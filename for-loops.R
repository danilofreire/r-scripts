# "For loops" have always been difficult to master in R. 
# Nevertheless, there are some tricks that one can do to improve their efficiency
# This example was extracted from: http://paleocave.sciencesortof.com/2013/03/writing-a-for-loop-in-r/

# Let's get started

# Basically, a loop's syntax can be written as
# for (counter in vector) {commands}

# In a simple example: I’m going to set up a loop to square every element of my dataset,
# foo, which contains the odd integers from 1 to 100
# (keep in mind that vectorizing would be faster for my trivial example – see below).

foo = seq(1, 100, by=2)
foo.squared = NULL
for (i in 1:50 ) {
  foo.squared[i] = foo[i]^2
}

# If the creation of a new vector is the goal, 
# first you have to set up a vector to store things in prior to running the loop.
# This is the foo.squared = NULL part. 

# However, this loop could be easily substituted by:

foo.squared = foo^2

# But in case a loop is indeed necessary, here are some tips:
# 1 - if you want to, create a loop of the same lenght as the vector

for (i in 1:length(foo)) {
  #stuff to do the number of times that foo is long
}

# 2 - Of course, sometimes when we write loops we don’t know how many things 
# are going to come out the other end. Usually we can guess on an upper bound though.
# It’s going to be faster to partially fill a very long vector using a loop then get
# rid of the meaningless stuff at the end than to grow a vector one loop at a time. 
# We can make a very large vector full of NAs and dump them at the end. 

bar.squared = rep(NA, 200000)

for (i in 1:length(bar) ) {
  bar.squared[i] = bar[i]^2
}

#get rid of excess NAs

bar.squared = bar.squared[!is.na(bar.squared)]
summary(bar.squared)

# Compare the speed:

bar = seq(1, 200000, by=2)
bar.squared = NULL

for (i in 1:length(bar) ) {
  bar.squared[i] = bar[i]^2
}
summary(bar.squared)

# Another simple example:

x <- rnorm(100)
for(i in length(df$x)){
  x2 <- x*2
}
df <- data.frame(x,x2)
View(df)

# Loop for squaring values, comparing with ifelse:

values <- rnorm(1000, 10, 5)
squared <- rep(NA, 1000)
squared1 <- rep(NA, 1000)

for(i in seq(along=squared1)){
  if (values[i] <= 5) {
    squared1[i] <- -1
  }  else {
    squared1[i] <- values[i]^2
  }
}
squared <- ifelse(values <= 5, -1, values^2)

hist(squared)
df <- data.frame(values, squared, squared1)
View(df)
