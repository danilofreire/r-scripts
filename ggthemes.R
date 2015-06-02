# ggthemes is a very cool package
# it adds a lot of very nice themes to ggplot.
# here: https://github.com/jrnold/ggthemes

install.packages('ggthemes', dependencies = TRUE)

# using the good ol' diamonds dataset

library("ggplot2")
library("ggthemes")
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]

# Tufte theme: Minimal theme and geoms

(ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rangeframe() + theme_tufte())

(ggplot(mtcars, aes(factor(cyl), mpg)) + theme_tufte(ticks = FALSE) + geom_tufteboxplot())

# The Economist

(qplot(carat, price, data = dsamp, colour = cut) + theme_economist() + scale_colour_economist() + 
    ggtitle("Diamonds Are Forever"))

# Stata too! You can add the following commands:
# scale_color_stata, scale_shapes_stata, scale_linetype_stata
# which are: color, shape, and linetype palettes from Stata graph schemes.

(qplot(carat, price, data = dsamp, colour = cut) + theme_stata() + scale_colour_stata() + 
    ggtitle("Plot Title"))

# Tableau

(qplot(carat, price, data = dsamp, colour = cut) + theme_igray() + scale_colour_tableau())

# Tableau with colour palette for colourblind people

(qplot(carat, price, data = dsamp, colour = cut) + theme_igray() + scale_colour_tableau("colorblind10"))

# A last and nice one: Few.
# Color palette and theme based on Stephen Few's "Practical Rules for Using Color in Charts".

(qplot(carat, price, data = dsamp, colour = cut) + theme_few() + scale_colour_few())



 


