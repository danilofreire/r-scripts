#######################
### Characters in R ###
#######################

# Here I'll show how to perform simple character operations in R.
# Gaston Sanchez has a great page on that: 
# http://gastonsanchez.com/blog/resources/how-to/2013/09/22/Handling-and-Processing-Strings-in-R.html

# This script uses the same material from 
# http://quantifyingmemory.blogspot.com/2015/03/digital-data-collection-course.html
# It's a very good course! :)

# Let's get started.

# Top string manipulation functions: 
# tolower (also toupper, capitalize)
# grep
# gsub
# str_split (library: stringr) -substring
# paste and paste0
# nchar
# str_trim (library: stringr)

# With those functions you can do a lot of tasks. Let's start with 
# tolower and toupper. 
tolower("TEST")
toupper("test")

# Using USArrests data
data(USArrests)
str(USArrests)

states <- rownames(USArrests)
head(states)
tolower(states[0:6]) # lower case -- 6 observations
toupper(states[0:6]) # upper case -- 6 observations

# We can also count the number of characters per observation.
nchar(states)

states[nchar(states)==5] # states with 5 characters

# Abbreviating names
substr(x = states, start = 1, stop = 4)

# Plotting the number of characters
hist(nchar(states), main = "Histogram",
	 xlab = "number of characters in US State names")

# To manipulate URLs, use the function str_split() from stringr.
library(stringr)

link <- "http://stats.grok.se/json/en/201401/web_scraping"
str_split(link,'/')

# One can also clean the string with str_trim().
annoyingString <- "\n    something HERE  \t\t\t"
nchar(annoyingString)
str_trim(annoyingString)
tolower(str_trim(annoyingString))
nchar(str_trim(annoyingString))

# Count total number of a's
str_count(states, "a")
str_count(tolower(states), "a") # include all letters

# Count all vowels
vowels <- c("a", "e", "i", "o", "u")               # vowels
num_vowels <- vector(mode = "integer", length = 5) # vector for storing results

for (j in seq_along(vowels)) {                     # calculate number of vowels
num_aux <- str_count(tolower(states), vowels[j])
num_vowels[j] <- sum(num_aux)
}

names(num_vowels) <- vowels                        # add vowels' names

barplot(num_vowels, main = "Number of vowels in USA States names",
border = NA, ylim = c(0, 80))                      # barplot

#  Concatenating with str_c()
str_c("May", "The", "Force", NULL, "Be", "With", "You", character(0))
str_c("May", "The", "Force", "Be", "With", "You", sep = " ") # add a separator

# Number of characters with str_length()
some_text <- c("one", "two", "three", NA, "five")
str_length(some_text) # it handles NA nicely

# Substring with str_sub()
had <- "Hadley Wickham"
str_sub(had, 1, 3)
str_sub(had, seq_len(nchar(had))) # extracting sequentially
str_sub(had, 1, 6) <- "Havard"    # substitute
print(had)
str_sub(had, - 7) <- "William"    # replacing with negative positions
print(had)

# Duplication with str_dup()
str_dup("hola", 3)
str_dup("adios", 1:3)
words <- c("lorem", "ipsum", "dolor", "sit", "amet")
str_dup(words, 2)
str_dup(words, 1:5)

# Padding with str_pad()
str_pad("hashtag", width = 8, pad = "#")
str_pad("hashtag", width = 9, side = "both", pad = "-")

# Trimming with str_trim()
bad_text = c("This", " example ", "has several ", " whitespaces ")
str_trim(bad_text, side = "both")

# Word extraction with word()
change <- c("Be the change", "you want to be")
word(change, 1)
word(change, -1)

# Detecting patterns with str_detect()
some_objs <- c("pen", "pencil", "marker", "spray")
str_detect(some_objs, "pen")
some_objs[str_detect(some_objs, "pen")]

# Extract first match with str_extract()
paris_tweets <- c(
"#Paris is chock-full of cultural and culinary attractions",
"Some time in #Paris along Canal St.-Martin famous by #Amelie",
"While you're in #Paris, stop at cafe: http://goo.gl/yaCbW",
"Paris, the city of light")
hash = "#[a-zA-Z]{1,}" # hashtags
str_extract(paris_tweets, hash)
str_extract_all(paris_tweets, "#[a-zA-Z]{1,}")

#  Locate first match with str_locate()
str_locate(paris_tweets, "#[a-zA-Z]{1,}")

# Replace first match with str_replace()
cities <- c("San Francisco", "Barcelona", "Naples", "Paris")
str_replace(cities, "[aeiou]", ";") # replace first matched vowel
str_replace_all(cities, pattern = "[aeiou]", ";") # replace all vowels
str_replace_all(cities, pattern = "[^aeiou]", ";") # replace all consonants

# String splitting with str_split()
sentence <- c("R is a collaborative project with many contributors")
str_split(sentence, " ")

# Let's do another example:
library(RCurl)

download.file('https://raw.githubusercontent.com/fredheir/WebScraping/gh-pages/Lecture1_2015/text.txt',destfile='tmp.txt',method='curl')
text <- readLines('tmp.txt')

length(text)                   # 98 lines
unlist(str_split(text[7],' ')) # print the 7th line and split words
table(unlist(str_split(text[7],' ')))  # word frequency
sort(table(length(unlist(str_split(text,' '))))) # sort

# grep() allows regular expressions in R.
grep("k", states)         # states with k
states[grep("k", states)] # selection

grep("London", text)

# Regex matches the beginning or end of a word.
stalinwords <- c("stalin","stalingrad","Stalinism","destalinisation")

grep("stalin", stalinwords, value = TRUE)
grep("[Ss]talin", stalinwords, value = TRUE) # capital letters allowed
grep("s*grad", stalinwords, value = TRUE)    # wildcards
grep('\\<d', stalinwords, value = TRUE)      # beginning
grep('d\\>', stalinwords, value = TRUE)      # end

# gsub() 
author <- "By Rolf Fredheim"
gsub("By ","", author)

# paste()
var <- 201401
paste("http://stats.grok.se/json/en/",var,"/web_scraping", sep = "")
