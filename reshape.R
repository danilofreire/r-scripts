# Loading required package
library(foreign)

# Loading the dataset and removing unused variables
awsd <- read.dta("/home/sussa/Downloads/AWSD.dta")
variables.keep <- c("year", "country", "un", "ingo", "lngonrcs", "icrc",
                    "ifrc", "other", "nationalskilled", "nationalswounded",
                    "nationalskidnapped", "totalnationals", "internationalskilled",
                    "internationalswounded", "internationalskidnapped", "totalinternationals",
                    "totalkilled","totalwounded", "totalkidnapped", "totalaffected")
awsd <- awsd[variables.keep]

# Reshaping the dataset
awsd <- subset(awsd, year <= 2013 & year >= 1997)
awsd$un <- as.integer(awsd$un)
awsd$ingo <- as.integer(awsd$ingo)
awsd$lngonrcs <- as.integer(awsd$lngonrcs)
awsd$icrc <- as.integer(awsd$icrc)
awsd$ifrc <- as.integer(awsd$ifrc)
awsd$other <- as.integer(awsd$other)
awsd$nationalskilled <- as.integer(awsd$nationalskilled)
awsd$nationalswounded <- as.integer(awsd$nationalswounded)
awsd$nationalskidnapped <- as.integer(awsd$nationalskidnapped)
awsd$totalnationals <- as.integer(awsd$totalnationals)
awsd$internationalskilled <- as.integer(awsd$internationalskilled)
awsd$internationalswounded <- as.integer(awsd$internationalswounded)
awsd$internationalskidnapped <- as.integer(awsd$internationalskidnapped)
awsd$totalinternationals <- as.integer(awsd$totalinternationals)
awsd$totalkilled <- as.integer(awsd$totalkilled)
awsd$totalwounded <- as.integer(awsd$totalwounded)
awsd$totalkidnapped <- as.integer(awsd$totalkidnapped)
awsd$totalaffected <- as.integer(awsd$totalaffected)
awsd2 <- aggregate(data = awsd,cbind(un, ingo, lngonrcs, icrc, ifrc, other,nationalskilled,
                                     nationalswounded, nationalskidnapped, totalnationals,
                                     internationalskilled, internationalswounded, internationalskidnapped,
                                     totalinternationals, totalwounded, totalkidnapped, totalaffected)~year+country, sum)

# Including 0s for years without a record
awsd3 <- expand.grid(unique(awsd$year), unique(awsd$country))
names(awsd3)[names(awsd3)=="Var1"]<-"year"
names(awsd3)[names(awsd3)=="Var2"]<-"country"
awsd3 <- merge(awsd3,awsd2, all.x = TRUE, by = 1:2)
awsd3[is.na(awsd3)] <- 0

# Sorting by country name
awsd3 <- awsd3[order(awsd3$country),]
View(awsd3)

# Saving the results as a csv file.
write.csv(awsd3, "/home/sussa/Desktop/awsd3.csv")


#######################################################

# Same thing with data tables (can be faster on large datasets).

library(data.table)
dt   <- data.table(df,key="year,country")
smry <- dt[,list(totalnationals      =sum(totalnationals), 
                 internationalskilled=sum(internationalskilled)),
           by="year,country"]
countries   <- unique(dt$country)
template    <- data.table(year=rep(1997:2013,each=length(countries)),
                          country=countries, 
                          key="year,country")
time.series <- smry[template]
time.series[is.na(time.series)]=0

########################################################
#Converting data from wide to long and vice-versa.

#1) Fro wide to long:

#From this: 
  
#  Code Country        1950    1951    1952    1953    1954
#AFG  Afghanistan    20,249  21,352  22,532  23,557  24,555
#ALB  Albania        8,097   8,986   10,058  11,123  12,246

#To this:
  
#  Code Country        Year    Value
#AFG  Afghanistan    1950    20,249
#AFG  Afghanistan    1951    21,352
#AFG  Afghanistan    1952    22,532
#AFG  Afghanistan    1953    23,557
#AFG  Afghanistan    1954    24,555
#ALB  Albania        1950    8,097
#ALB  Albania        1951    8,986
#ALB  Albania        1952    10,058
#ALB  Albania        1953    11,123
#ALB  Albania        1954    12,246

#Two ways:
  library(reshape)
reshape(d, direction="long", varying=list(names(d)[3:7]), v.names="Value", 
        idvar=c("Code","Country"), timevar="Year", times=1950:1954)

#Or:
  
x2 <- melt(x,id=c("Code","Country"),variable_name="Year")
x2[,"Year"] <- as.numeric(gsub("X","",x2[,"Year"]))

#2) From long to wide:
  
#  1      2      3      4
#firstName   value  value  value  value
#secondName  value  value  value  value

reshape(dat1, idvar = "name", timevar = "numbers", direction = "wide")

# There is now the  "tidyr" package which does the same thing with less commands,
# but I have yet to test it :)
