# Script by Paulo Barberá.

# First, load the packages
library(Rfacebook)
library(Rook)

# Read: https://github.com/pablobarbera/Rfacebook

# OAuth - Get token at: https://developers.facebook.com/tools/explorer
token <- "insert token here"
me <- getUsers("danilofreire", token, private_info = TRUE)
me$name
me$hometown

# Friends data
my_friends <- getFriends(token)
head(my_friends$id, n = 1)
table(my_friends$gender)
table(substr(my_friends$locale, 1, 2)) 
table(substr(my_friends$locale, 4, 5))
table(my_friends$relationship_status)

# Creating friend's matrix
mat <- getNetwork(token, format = "adj.matrix")
dim(mat)

# Graph
library(ggplot2)
library(scales)
library(grid)
library(igraph)

network <- graph.adjacency(mat, mode="undirected") ## igraph object
fc <- fastgreedy.community(network) ## communities / clusters
set.seed(123)
l <- layout.fruchterman.reingold(network, niter=1000, coolexp=0.5) ## layout

# preparing data for plot
d <- data.frame(l); names(d) <- c("x", "y")
d$cluster <- factor(fc$membership)

# plot with only nodes, colored by cluster
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_point()
pq

## too many clusters! let's pick just those with 10 friends or more
large.clusters <- which(table(fc$membership)>=10)
fc$membership[fc$membership %in% large.clusters == FALSE] <- "Others"
d$cluster <- factor(fc$membership)

# plot with only nodes, colored by cluster
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_point()
pq

## let's simplify even further by keeping only nodes in giant component
cl <- clusters(network)
gc <- which(cl$membership == 1)
mat <- mat[gc, gc]

network <- graph.adjacency(mat, mode="undirected") ## igraph object
fc <- fastgreedy.community(network) ## communities / clusters
fc ## to see which are the communities
set.seed(123)
l <- layout.fruchterman.reingold(network, niter=1000, coolexp=0.5) ## layout
d <- data.frame(l); names(d) <- c("x", "y")
d$cluster <- factor(fc$membership)

p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_point()
pq

## now let's add the edges
edgelist <- get.edgelist(network, names=FALSE)
edges <- data.frame(d[edgelist[,1],c("x", "y")], d[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2")

pq <- pq + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2), 
  data=edges, size=0.25, color="grey", alpha=1/3)
pq

## (note that the order matters!)

p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2), 
  data=edges, size=0.25, color="grey", alpha=1/3) +
  geom_point()
pq

## change a few of the theme options to make it look better
pq <- pq + theme(
  # dark background
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill="black"),
  # removing axis lines and ticks
  axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
  axis.title = element_blank(), panel.border = element_blank(), 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank()
)
pq

## now let's customize the legend
pq <- pq + theme(
  # dark background to legend
  legend.background = element_rect(colour = "white", fill = "black"),
  legend.key = element_rect(fill = "black", colour = F),
  # text and border in white
  legend.title = element_text(color="white"),
  legend.text = element_text(color="white")
)
pq

## let's also improve the labels by identifying most central node within
## each community (using degree as measure of centrality) and adding labels
## based on what we learn from that
d$degree <- degree(network)
which.max(degree(network)) ## who do I have more friends in common with?
central.nodes <- lapply(communities(fc), function(x) x[which.max(d$degree[x])])
central.names <- fc$names[unlist(central.nodes)] ## names of central nodes
## within each cluster

## labels I give to each cluster
labels <- c("USP", "IPSA/IAPSS", "Naumann", "Foyer", "ESPM", "IHEID", "Rosario", "Others")
pq <- pq + scale_color_discrete(labels=labels)
pq

## we can also add the labels to the plot
d$label <- NA
d$label[unlist(central.nodes)] <- labels

pq <- pq + geom_text(aes(x=x, y=y, label=label), data=d, color="white", size=3)
pq ## let's forget about it for now...


## let's put it all together, with some final touches on how points look
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2), 
  data=edges, size=0.25, color="white", alpha=1/3) +
  ## note that here I add a border to the points
  geom_point(color="grey20", aes(fill=cluster), shape=21, size=2) +
  scale_fill_discrete(labels=labels) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill="black"),
    axis.line = element_blank(), axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.background = element_rect(colour = F, fill = "black"),
    legend.key = element_rect(fill = "black", colour = F),
    legend.title = element_text(color="white"),
    legend.text = element_text(color="white")
  ) +
  ## changing size of points in legend
  guides(fill = guide_legend(override.aes = list(size=5)))

pq

# Searching pages
# N can be any number big enough to download all the comments
page <- getPage("TheSecretSits", token, n = 100000)

# Post with the highest number of likes
page[which.max(page$likes_count), ] # or
library(dplyr)
band1 <- arrange(page, desc(likes_count))
View(band1)

# See what kind of post gets more likes, if videos, photos, etc
# Arrange by likes
band <- group_by(page, type)
band1 <- summarise(band, mean(likes_count))
band1
band1 <- arrange(page, desc(likes_count))
View(band1)

# Comments
band2 <- summarise(band, mean(comments_count))
band2

## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

## aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

# create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)

# visualize evolution in metric
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(labels = date_format("%m")) + ylab("Average count per post") + 
  ggtitle("The Secret Sits") + theme_classic() + xlab("Month")

