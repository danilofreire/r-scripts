# Dplyr is another amazing package of the `Hadleyverse'
# It is an improvement over plyr and is aimed at dataframes.
# It does 5 basic functions:
# group_by, summarise, mutate, filter and arrange

# Let's play around with the `turnout' dataset in Zelig.

library(dplyr)
library(Zelig)
data(turnout)
View(turnout)

# Let's create 2 groups, race == white and others.
# Then let's check their mean incomes.

df1 <- group_by(turnout, race)
df2 <- summarise(df1, mean(income))
df2

# Now let's check the education level between those
# who voted and those who did not.

df3 <- group_by(turnout, vote)
df4 <- summarise(df3, mean(educate))
df4

# Dplyr also creates new variables with mutate().
# Let's add log.income to our table

df5 <- mutate(turnout, log.income = log(income))
head(df5)

# We can select cases very easily too. 
# We use the filter() command.
# Selecting those who are white, voted and
# have income higher than 3 and lower than 6.

df6 <- filter(turnout, race == "white", vote == 1, income > 3, income < 6)
head(df6)
summary(df6)

# Let's now arrange them by income, descending order
# and also by education, ascending order.
# We'll use the arrange() command.

df6 <- arrange(df6, arrange = desc(income), arrange = (educate))
head(df6)
tail(df6)

# dplyr provides another innovation over plyr: 
# the ability to chain operations together from left to right 
# with the %.% operator. This makes dplyr behave a little like
# a grammar of data manipulation:

turnout %.%
group_by(vote) %.%
summarise(mean(educate))

# Read more about it in the help, ?"%.%".

# ... and that's it. Very easy to use! 
# More at: http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# 
