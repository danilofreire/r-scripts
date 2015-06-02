# Lag / lead variables in cross-sectional data
# http://christophergandrud.blogspot.kr/2013/05/slide-one-function-for-laglead.html

# I've found the various R methods for doing this hard to remember and usually 
# need to look at old blog posts. 
# Any time we find ourselves using the same series of codes over and over,
# it's probably time to put them into a function.

# So, I (Christian) added a new command–slide–to the DataCombine R package (v0.1.5).

# Not Cross-sectional data

# Create time variable
Year <- 1980:1999

# Dummy covariates
A <- B <- 1:20

Data1 <- data.frame(Year, A, B)

head(Data1)

# Lag A to A-1
library(DataCombine)

DataSlid1 <- slide(Data1, Var = "A", slideBy = -1)

head(DataSlid1)

#----------------

# Time-series Cross-sectional data

# Create time and unit ID variables
Year <- rep(1980:1983, 5)
ID <- sort(rep(seq(1:5), 4))

# Dummy covariates
A <- B <- 1:20

Data2 <- data.frame(Year, ID, A, B)

head(Data2)

# Now let's create a two time unit lead variable based on B for each unit 
# identified by ID:

DataSlid2 <- slide(Data2, Var = "B", GroupVar = "ID",
                    slideBy = 2)

head(DataSlid2)

# Sometimes the method above is slow. A speedier method will be convert the 
# dataset as data.table, then set it back to data.frame.

library(data.table)

# Writing the lag function
lg <- function(x)c(NA,x[1:(length(x)-1)])

# Using a true example: 

df1 <- data.table(df, keep.rownames=TRUE)
df1 <- df1[,transform(.SD,wdi_gdpc.thdollars.1=lg(wdi_gdpc.thdollars)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,wdi_aid.milldollars.1=lg(wdi_aid.milldollars)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,coups.1=lg(coups)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,coup.success.1=lg(coup.success)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,coup.unsuccess.1=lg(coup.unsuccess)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,fe_etfra.1=lg(fe_etfra)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,unna_pop.1=lg(unna_pop)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,wdi_urban.1=lg(wdi_urban)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,right.1=lg(right)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,left.1=lg(left)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,british.colony.1=lg(british.colony)),by=c("ccodecow")]
df1 <- df1[,transform(.SD,french.colony.1=lg(french.colony)),by=c("ccodecow")]
df <- as.data.frame(df1)
