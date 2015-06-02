# Create a data set from vectors

employee <- c('John Doe','Peter Gynn','Jolie Hope')
salary <- c(21000, 23400, 26800)
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))
employ.data <- data.frame(employee, salary, startdate)

# R transforms string values to factors automatically.
# To prevent that, add the following argument: stringsAsFactors=FALSE
employ.data <- data.frame(employee, salary, startdate, stringsAsFactors=FALSE)

# Add a new variable

employ.data$salarary2 <- salary^2

# Finally
View(employ.data)
write.dta(employ.data, "/home/sussa/Desktop/teste.dta")
