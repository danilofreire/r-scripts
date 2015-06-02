# Renaming a variable
names(dataframe)[names(dataframe)=="oldvariablename"] <- "newvariablename"

# An easier way of doing it
names(dataframe)[1] <- "newvariablename" # replace [1] by the column number you want to change
