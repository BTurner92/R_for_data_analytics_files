rm(list = ls())
library(tidyverse)

# Basic Plotting Functions -----------------------------------------

df<- iris
plot(df$Sepal.Length)
# Plot against index number which is a bit pointless...
# requires more input

plot(df$Sepal.Length, type = "l", col = "salmon")
# Again, doesn't make any sense as it is graphed against index.
# Only necessary if data is assembled by time/date

plot(x = df$Sepal.Length, y = df$Sepal.Width)
# Only plots against the two values where they overlap. Plot actually means
# very little
# We're getting there
plot(x = df$Sepal.Length, y = df$Sepal.Width, col = df$Species,
     xlab = "Sepal Length, cm", ylab = "Sepal Width, cm",
     main = "Relathionship between Sepal Length and Width by Species")
# Much better plot, allows for relationships to be seen easily
# Legend here will help to explain further!
legend(x = "topright", legend = unique(df$Species),
       col = 1:length(df$Species), pch = 1)
# Another extra function is point, allows to easily add more info.
points(x = df$Petal.Length, y = df$Petal.Width,
       col = "blue", pch = 16, cex = 3)
# Points here allow for additional comparison between the sepal and petal 
# measurements.
# Function points cannot influence the scale of the graph it is being added to.

# Now adding more lines, make it more interesting...
lines(x = c(5,7), y = c(2,4), col = "blue", lty = 3, lwd = 2)
abline(h = 3, col = "red", lwd = 6, lty = 4)
# Abline creates only straight lines!

# Points of note regarding boxplots and categorical data in graphs.
boxplot(Sepal.Length ~ Species, data = df)
abline(h = mean(df$Sepal.Length), col = "red", lty = 3)
abline(v = c(1.5, 2.5), col = "green")
#Creation of a line for the mean of the entire data set and two vertical
# lines to neatly separate the 3 boxplots.
# A Piped and separated line
lines(x = c(1,2,3), 
      y = df %>% group_by(Species) %>%
        summarise(m = mean(Sepal.Length)) %>%
        pull(),
      col = "pink", lwd = 4, lty = 3)
# Creates a moving average line through the boxplots.



### GGPLOT2 The best data visualisation package! -----------------------------
# 2 versions, qplot and ggplot, q is simplified verison of ggplot
rm(list = ls())
graphics.off() # Cleans out all data visualisations, like brush in graph window.

### QPLOT ------------------------
# Tries to emulate the syntax used in basic plotting functions with the 
# functionality of tidyverse.
?qplot 
# Example below is same data as above
qplot(x = df$Sepal.Length, y = df$Sepal.Width, col = df$Species,
     xlab = "Sepal Length, cm", ylab = "Sepal Width, cm",
     main = "Relationship between Sepal Length and Width by Species")
# Significantly easier to read and understand

# for easier tidyverse style syntax, can do the following
qplot(x = Sepal.Length, y = Sepal.Width, data = df, col = Species,
      xlab = "Sepal Length, cm", ylab = "Sepal Width, cm",
      main = "Relationship between Sepal Length and Width by Species")

# MULTIPLE PLOTS-=-=-=-
# Using facets arguments will split from the "data" into multiple plots
qplot(x = Sepal.Length, y = Sepal.Width, data = df, col = Species,
      facets = Species ~ ., #switching arguments will flip graph
      xlab = "Sepal Length, cm", ylab = "Sepal Width, cm",
      main = "Relationship between Sepal Length and Width by Species")

# GEOM -=-=-
# More descriptive lines and graph manipulations
?qplot
# More advanced combinations of graph types
qplot(x = Sepal.Length, data = df, geom = "boxplot")

qplot(x = Sepal.Length, y = Species, data = df, geom = "boxplot")

qplot(y = Sepal.Length, x = Species, data = df, geom = c("boxplot", "point"))
# combines two types of graphs together, allows us to visualise where the
# points lie within variables.
qplot(y = Sepal.Length, x = Species, data = df, geom = c("boxplot", "jitter"))
# jitter spreads the points around in general area of boxplot.

# combining data with lines (linear regression, trendline etc.)
qplot(x = Sepal.Length, y = Sepal.Width, data = df, col = Species,
      facets = Species ~ ., 
      geom = c("point", "smooth"), method = "lm", 
      xlab = "Sepal Length, cm", ylab = "Sepal Width, cm",
      main = "Relationship between Sepal Length and Width by Species")

#### GGPLOT #### ----------------------------------
?ggplot
# Only two arguments, data and mapping.
# Mapping should use aes "aesthetics" function, contains all of the extra 
# parameters. ggplot simply adds the stage, prepares for the addition of
# extra layers.

ggplot(data = df, mapping = aes(x=Sepal.Length,
                                y=Sepal.Width,
                                colour = Species)) + geom_point()
# use plus sign + to add additional elements (via geom functions for example)
# ggplot acts by the addition of graph elements + via functionsas opposed to 
# adding function arguments
ggplot(data = df, mapping = aes(x=Sepal.Length,
                                y=Sepal.Width,
                                colour = Species)) + 
  geom_point(size = 3, alpha = .5)
# adding alpha layering means that points are semi transparent therefore will
# appear bold if points overlap
ggplot(data = df, mapping = aes(x=Sepal.Length,
                                y=Sepal.Width,
                                colour = Species)) + 
  xlab("Sepal Length, cm") + ylab("Sepal Width, cm") +
  ggtitle("Relationship between Sepal Length and Width") +
  geom_hline(yintercept = 3, linewidth = 10) +
  geom_point(size = 3, alpha = .5)
# Axis and title are now functions not arguments, use geom to produce lines,
# can check individual functions for their specific arguments.
# NOTE - Lines/elements will appear in the order they are programmed, layer on
# top of each other.
ggplot(data = df, mapping = aes(x=Sepal.Length,
                                y=Sepal.Width,
                                colour = Species)) + 
  xlab("Sepal Length, cm") + ylab("Sepal Width, cm") +
  ggtitle("Relationship between Sepal Length and Width") +
  geom_hline(yintercept = 3, linewidth = 10) +
  geom_point(size = 3, alpha = .5) +
  geom_point(mapping = aes(x = Petal.Length, y = Petal.Width))
# Able to add additional data set / points as an additional graph layer
# Far simpler than default functions as ggplot resizes the axes to suit all data
# Fed into it.

# HISTOGRAMS
# A bit more complicated than default, requires you to define every parameter
# if you wish to have the best looking histogram.

?geom_histogram
df %>% ggplot(mapping = aes(x = Sepal.Length)) +
  geom_histogram(bins = 20)
# Can also define bins by breaks in a sequence, bin number or by binwidth
# breaks = seq(4, 8, 0.2)

# Three separate histograms via facet_grid
df %>% ggplot(mapping = aes(x = Sepal.Length)) +
  geom_histogram() + facet_grid(rows = vars(Species))

# Creation of boxplots with scatter data points on them
df %>% ggplot(aes(x = Sepal.Length, y = Species)) +
  geom_boxplot() +
  geom_jitter(height = 0.1, width = 0.1)
# can also adjust the spread of these extra points using height and width.  
  
# Vid 3
# Good practice to store graphs as a variable, makes for easy comparison
# Creates an object of class gg ggplot
# need to print the object to see again, can continually add to the plot with +

gr1 <- df %>% ggplot(aes(x = Sepal.Length, y = Species)) +
  geom_boxplot() + geom_jitter()
print(gr1)  

gr1 + geom_point(colour = "red")  # not really necessary, proof of concept
gr1 + ggtitle("Insert title here")

gr2 <- ggplot(df, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point(size = 2) + geom_smooth()
print(gr2)  

library(ggpubr) #themes from ggpubr
library(tidyverse)
# allows you to arrange graphs together for better look when publishing analysis
ggarrange(gr1, gr2, ncol = 1) 

# THEMES -----------------------------
gr1 + theme_cleveland()
gr2 + theme_dark()

# more available from ggthemes
library(ggthemes)
# contains themes for certain publications
gr2 + theme_economist()
gr2 + theme_wsj()

# GGPUBR
# Designed to be a simpler version of ggplot, without the complex syntax, 
# aimed at less experienced coders eg. researchers
?ggpubr

gghistogram(df, x = "Sepal.Length", fill = "Species", add = "mean")
# note, needs quotation marks as it is not part of dplyr, doesnt work with 
# dplyr syntax, also need quotes on function name
ggboxplot(df, x = "Species", y = "Sepal.Length", width = 0.8, add = "jitter")

