#########################
### Web scraping in R ###
#########################

# R can be used to scrap information from the web. There are
# many ways to do so, and here I'll show the simplest ones.

# Hadley Wickham has a great R package for web scraping.
# It's called "rvest" and it's available on CRAN. 
# On Ubuntu 14.04LTS, I had to install the XML package with the
# terminal: sudo apt-get install r-cran-xml . It's also very useful
# to download the SelectorGadget extension for Chrome.
# A tutorial: https://vimeo.com/52055686
library(rvest)

# Let's also load other packages 
library(dplyr)
library(reshape2)
library(googleVis)

# Load page
lego_movie <- html("http://www.imdb.com/title/tt1490017/")

lego_movie %>% 
  html_node("strong span") %>% # check vignette("selectorgadget")
  html_text() %>%
  as.numeric()                 # rating

lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

# Another example
html("http://www.stat.iastate.edu/people/faculty/") %>%
	html_nodes("#content a") %>%
	html_attr(name="href") -> hrefs
head(hrefs)

# Using Wikipedia
html("http://en.wikipedia.org/wiki/List_of_Presidents_of_Brazil")%>%   
  html_nodes(".wikitable b a") %>% 
  html_text()

# Table with dates:
html("http://en.wikipedia.org/wiki/List_of_Presidents_of_Brazil")%>%   
  html_nodes("td:nth-child(5) , tr:nth-child(2) td:nth-child(6) , td:nth-child(4) a , .wikitable b a") %>%
  html_text()

# Another example with Wikipedia
html("http://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States")%>%   
  html_nodes("span+ a") %>% 
  html_text()

# Fetching data using jsonlite
library(jsonlite)

var <- 201403
url <- paste("http://stats.grok.se/json/en/",var,"/web_scraping",sep="")
rd <- fromJSON(readLines(url, warn="F"))
rd.views <- rd$daily_views 
df <- as.data.frame(unlist(rd.views))
head(df)

# Plot
library(ggplot2)
library(lubridate)

df$date <-  as.Date(rownames(df))
colnames(df) <- c("views","date")
ggplot(df,aes(date,views))+
  geom_line()+
  geom_smooth()+
  theme_bw(base_size=20)

# Number of Facebook shares of a BBC website using Facebook's API.
url <- 'http://graph.facebook.com/?id=http://www.bbc.co.uk/sport/0/football/31583092'
raw.data <- readLines(url, warn="F") 
rd  <- fromJSON(raw.data)
df <- data.frame(rd)
head(df)

# With Twitter
url <- 'http://www.dailymail.co.uk/news/article-2643770/Why-Americans-suckers-conspiracy-theories-The-country-founded-says-British-academic.html'
target <- paste('http://urls.api.twitter.com/1/urls/count.json?url=',url,sep="")
raw.data <- readLines(target, warn="F") 
rd  <- fromJSON(raw.data)
tw1 <- data.frame(rd)
head(tw1)
