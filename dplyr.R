### dplyr tutorial

# This file replicates the functions included in
# https://www.listendata.com/2016/08/dplyr-tutorial.html.
# Thanks to Deepanshu Bhalla for the code! All credits to him!
# I've only updated a few examples to include more modern functions,
# such as across(), and exclude deprecated ones such as do() or
# summarise_*()

## Important dplyr functions

# dplyr has 7 important functions:
# select, filter, group_by, arrange, summarise, join, and mutate.
# Let's take a closer look at them.

## Load required packages
packages <- c("tidyverse", "vroom", "dtplyr")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

## Load dataset
df <- vroom("https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv")

## dplyr Practical Examples

## slice_sample() Function

# Example 1: Selecting random n rows with `slice_sample`
slice_sample(df, n = 10)

# Example 2: Selecting random fraction of rows with replacement
slice_sample(df, prop = 0.1, replace = TRUE)

## distinct() Function

# Example 3: remove duplicate rows based on all the variables with
# `distinct`
df2 <- df %>% slice(rep(1:n(), each = 3))
df2 %>% head()

distinct(df2) %>% head()

# Example 4: remove duplicate rows based on a variable
distinct(df, Index, .keep_all = TRUE)

# Example 5: remove duplicates rows based on multiple variables
distinct(df, Index, Y2010, .keep_all = TRUE)

## select() function

# Example 6: selecting variables (or columns)
select(df, Index, State:Y2008)

# Example 7: dropping variables
select(df, -Index, -State)

# Example 8: selecting or dropping variables that start with "Y"
select(df, starts_with("Y"))
select(df, -starts_with("Y"))

# Example 9: selecting variables that contain "St" in their names
select(df, contains("St"))

# Example 10: reorder variables
select(df, State, Index, everything())

## rename() Function

# Example 11: rename variables
rename(df, index1 = Index, STATE = State)

## filter() Function

# Example 12: filter rows
filter(df, State == "Alaska")

# Example 13: multiple selection criteria
filter(df, Index %in% c("A", "R"))

# Example 14: "AND" condition in selection criteria
filter(df, Index %in% c("A", "R") & Y2002 >= 1300000)

# Example 15: "OR" condition in selection criteria
filter(df, Index %in% c("A", "R") | Y2002 >= 1300000)

# Example 16: "NOT" condition
filter(df, !Index %in% c("A", "R"))

# Example 17: "CONTAINS" condition
# Note: `contains()` cannot be used here, it only works
# within a selecting function. Use `grepl()` instead.
filter(df, grepl("Ar", State))

# summarise() Function

# Example 18: summarise selected variables
summarise(df,
          Y2015_mean = mean(Y2015),
          Y2015_med = median(Y2015))

# Example 19: summarise multiple variables
df %>%
  select(Y2005:Y2006) %>%
  summarise(across(everything(),
                   list(n      = ~n(),
                        mean   = ~mean(.),
                        median = ~median(.))))

# Example 20: summarise with custom functions
df %>%
  select(Y2011, Y2012) %>%
  summarise(across(everything(),
                   list(n       = ~n(),
                        log     = ~{1 + log(.)}, # custom
                        missing = ~sum(is.na(.)),
                        mean    = ~mean(., na.rm = TRUE),
                        median  = ~median(., na.rm = TRUE))))

# Example 21: summarise all numeric variables
df %>%
  summarise(across(where(is.numeric),
            list(n    = ~n(),
                 mean = ~mean(., na.rm = TRUE))))

# Example 22: summarise factor variable
df %>%
  select(Index, State) %>%
  mutate(across(everything(), ~as.factor(.))) %>%
  summarise(across(where(is.factor),
                   list(levels = ~nlevels(.))))

## arrange() Function

# Example 23: sort data by multiple variables
df %>%
  arrange(Index, State)

## group_by() Function

# Example 24: summarise data by categorical variable
df %>%
  group_by(Index) %>%
  select(Y2004:Y2007) %>%
  summarise(across(where(is.numeric),
                   list(mean   = ~mean(.),
                        median = ~median(.))))

# Example 25: filter data within a categorical variable
df %>%
  select(Index, State, Y2004:Y2006) %>%
  filter(Index %in% c("A", "C", "I")) %>%
  group_by(Index) %>%
  summarise(head(across(), 2))

# Example 26: selecting 3rd maximum value by categorical variable
df %>%
  select(Index, Y2004:Y2006) %>%
  filter(Index %in% c("A", "C")) %>%
  group_by(Index) %>%
  arrange(desc(.)) %>%
  slice(3)

# Example 27: summarise, group and sort together
df %>%
  group_by(Index) %>%
  select(Index, Y2004:Y2006) %>%
  summarise(across(where(is.numeric),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd   = ~sd(., na.rm = TRUE)))) %>%
  drop_na() %>%
  arrange(desc(Y2004_mean))

## mutate() Function

# Example 28: create a new variable
df %>%
  select(State, Y2004) %>%
  mutate(Y2004_from_mean = (Y2004 - mean(Y2004)),
         mean            = mean(Y2004)) %>%
  arrange(Y2004_from_mean)

# Example 29: divide all the numeric variables by 1000
# and delete all unused variables
df %>%
  transmute(across(where(is.numeric),
                   list(ths = ~{. / 1000})))

# Example 30: calculate rank for variables
df %>%
  select(State, Y2004) %>%
  mutate(ranking = min_rank(desc(Y2004))) %>%
  arrange(ranking)

# Example 31: select state that generated highest
# and lowest income across index
df %>%
  group_by(Index) %>%
  select(Index, State, Y2004) %>%
  mutate(ranking = min_rank(desc(Y2004))) %>%
  slice_min(ranking, 1)

df %>%
  group_by(Index) %>%
  select(Index, State, Y2004) %>%
  mutate(ranking = min_rank(desc(Y2004))) %>%
  slice_max(ranking, 1)

# Example 32: cumulative income of 'Index' variable
df %>%
  group_by(Index) %>%
  mutate(total = cumsum(Y2015)) %>%
  select(Index, Y2015, total)

## join() Function

# Example 33: common rows in both the tables
df1 <- tibble(ID1 = c(1, 2, 3, 4, 5),
              w   = c("a", "b", "c", "d", "e"),
              x   = c(1, 1, 0, 0, 1),
              y   = rnorm(5),
              z   = letters[1:5])

df2 <- tibble(ID = c(1, 2, 3, 4, 5),
              a  = c("f", "g", "h", "i", "j"),
              b  = c(2, 3, 4, 5, 6),
              c  = rnorm(5),
              e  = letters[10:14])

inner_join(df1, df2, by = c("ID1" = "ID"))

# Example 34: applying LEFT JOIN
left_join(df1, df2, by = c("ID1" = "ID"))

# Example 35: applying INTERSECT
data(mtcars)
mtcars %>%
  rownames(mtcars) -> mtcars$model
mtcars %>%
  slice(1:20) -> first
mtcars %>%
  slice(10:32) -> second
intersect(first, second)

# Example 36: applying UNION
x <- tibble(ID = 1:6, ID1 = 1:6)
y <- tibble(ID = 1:6, ID1 = 1:6)
union(x, y)
union_all(x, y)

# Example 37: rows appear in one table but not in other table
setdiff(first, second)

# Example 38: SQL-style CASE WHEN statement
df5 <- data.frame(x = c(1:5, NA))

df5 %>%
  mutate(case_when(x == 1 ~ "One",
                   x == 2 ~ "Two",
                   x == 3 ~ "Three",
                   x == 4 ~ "Four",
                   x == 5 ~ "Five",
                   TRUE ~ "Missing data"))

# Example 39: apply ROW WISE operation
df %>%
  select(Y2004:Y2006) %>%
  rowwise() %>%
  mutate(max = max(across(everything())))

# Example 40: combine data frames
df1 <- tibble(ID = 1:6,  x = letters[1:6])
df2 <- tibble(ID = 7:12, x = letters[7:12])

bind_rows(df1, df2)
bind_cols(df1, df2)

# Example 41: calculate percentile values
df %>%
  group_by(Index) %>%
  summarise(percentile_25 = quantile(Y2015, probs = 0.25),
            percentile_50 = quantile(Y2015, probs = 0.5),
            percentile_75 = quantile(Y2015, probs = 0.75),
            percentile_99 = quantile(Y2015, probs = 0.99))

# There are other examples in the text, but I've
# already covered the functions above.
