# Three solutions for removing missing data:

x <- na.omit(your.data.frame)

# or

x <-  dataframe[complete.cases(dataframes),]

# or even

x <-subset(df, is.na(VariableWithNA))

# To remove NAs only from one variable:

df <- df[!is.na(df$var),]
