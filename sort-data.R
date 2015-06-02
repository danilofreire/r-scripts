# Install eeptools
install.packages("eeptools")

# Load eeptools
library(eeptools)

# Open data
data(stulevel)

# Sort data by one column, lowest to highest
stulevel <- stulevel[order(stulevel$ability) , ]

# Sort data by one column highest to lowest
stulevel <- stulevel[order(-stulevel$ability) , ]

# Sort data by two columns highest to lowest
stulevel <- stulevel[order(stulevel$grade, stulevel$ability) , ]

# Sort data by two columns highest to lowest
stulevel <- stulevel[order(-stulevel$grade, stulevel$ability) , ]

# Sort Text Data in R by One Column Lowest to Highest
# First create some example character data
stulevel$proflvl_character <- as.character(stulevel$proflvl)
stulevel$race_character <- as.character(stulevel$race)

# Sort character data by one column lowest to highest
stulevel <- stulevel[with(stulevel, order(stulevel$proflvl_character)),]

# Sort character data by one column highest to lowest
stulevel <- stulevel[with(stulevel, order(stulevel$proflvl_character, decreasing=TRUE)),]

# Sort character data by two columns lowest to highest
stulevel <- stulevel[with(stulevel, order(stulevel$proflvl_character, stulevel$race_character)),]

# Sort character data by one column highest to lowest
stulevel <- stulevel[with(stulevel, order(stulevel$proflvl_character, stulevel$race_character, decreasing=TRUE)),]
