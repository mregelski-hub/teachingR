# Anna Dornhaus & Lab
# R script for data analysis and figures for 
# just learning to do it in R

# Paper reference: NONE

### INTRO ----------------------

# Note how you can collapse parts of the code by clicking on the small triangles
# next to the line numbers. 
# Do this now for all of them except the Intro if you like. 
# This code starts with libraries and graphics setup, which are sections you 
# should have in every script, in addition to the header above. 
# You may initially have the libraries and graphics setup sections empty, and 
# populate them as your code gets more complicated. Your header should never be
# empty!! Also, make sure immediately that your code is synced to github, and 
# don't have several undistinguished versions lying around. 

### Libraries ------------------
#library(lme4) #needed for GLMMs
#library(lmerTest) #needed for obtaining p-values in lmm
#library(emmeans) #post-hoc comparisons

# General data handling
library(tidyverse) # for bind.rows
# File access
library(readxl)
library(googlesheets4) # for working with Google Sheets
library(googledrive) # for drive_download
# Colors
library(scales)  # for number_format & color scales
library(viridis)
# Making output tables for statistical models
library(sjPlot) # for linear models output tables


# Graphics setup -----------------------------
no_categories <- 15
## Colorpalette
twogroupcolors <- c("#5a9a8f", "#7b6ea8")
threegroupcolors <- viridis(3)
severalcategories_colors <- magma(no_categories)

## Similar to colors, it often makes sense to define some other things
## universally for all your plots, e.g. margins, where the sample sizes
## are plotted, y-axis range. This depends on your figures though.
y_max <- 6
y_min <- 0
N_y_offset <- 0.95 # This puts sample size numbers 5% below max, for example



# IMPORT ----------------------------------
# This section has several methods for importing. Don't feel like you need to master
# all of them at once - just pick whichever one is most important for you right 
# now. 

# A common problem when reading any files is that you must make sure
# that R uses the folder you want as 'working directory'. You can
# pick the working directory from the 'Session' menu above; or 
# you can write the path here in the code, e.g.:
setwd("C:/Users/dornh/Dropbox/Github/teachingR")
# But note that this is a little problematic as this detailed path
# would never work on someone else's computer. Instead,
# you can also use
setwd("../teachingR")
# or similar ... the ".." means 'go up one directory level' and then
# it will go into the folder to the right of the "/". 
MyDataLocalFolder <- read.table("./example_data/bb col data.csv"
                     , header=T
                     , row.names=1
                     , sep = ","
                     , dec = "."
                     )

# Or directly from a Google Sheet:
MyDatafromGoogleSheets <- read.csv("https://docs.google.com/spreadsheets/d/1K2D2rH770iDfZzlwakHK89bwvoPqWDLaHbVNsanJFX8/gviz/tq?tqx=out:csv")
# Or an existing .csv file on Google Drive:
MyDatafromGoogleSheets <- read.csv("https://drive.google.com/uc?export=download&id=1woJYnpsfMBPCC3kPSErcydgDCduEKjzy")
# Note that in both cases you are not just pasting the entire link from your browser, you are modifying it to include 
# the file id and some other stuff. Pattern match here!

# An .xlsx file from Google Drive is more complicated:
# You have to first download the file into a local file, then
# import the local file into R. 
googlepath <- "https://docs.google.com/spreadsheets/d/1sF5WnJs0uxeLKApn7_JzGJMozu6wgSPJ/edit"
tempxlsfile <- tempfile(fileext = ".xlsx")
drive_download(as_id(googlepath), path = tempxlsfile, overwrite = TRUE)
MyDatafromGoogleDrive  <- read_excel(tempxlsfile)
unlink(tempxlsfile)

# Or directly from github:
MyDatafromGithub <- read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Maze%20Data%20Raw")

# Import a whole list of files
# Note that this will only work if all the files have the same columns (and otherwise
# it is anyway doubtful that this would make sense)
files <- (Sys.glob("./example_data/similardatasheets/*.csv"))
# Initiate a blank data frame
EnormousDataLocal <- data.frame()
# Read content of all files into a list
listOfDataframes <- lapply(files, 
                             function(x) {
                               read.table(x, 
                                          header = T,
                                          sep = ",",
                                          skip = 6
                               )
                             }
)
# Add all the rows from all the files together
EnormousDataLocal <- do.call("bind_rows", listOfDataframes)

# Import a whole list of xls files from Google Drive
# Headache! Saving as csv is better...
googledrivefolderfiles <- drive_ls(as_id("https://drive.google.com/drive/folders/1aZWvhJjgTH9QTrD9hV1LiPiOKFRxmH7x"))
files2 <- googledrivefolderfiles$id
names2 <- googledrivefolderfiles$name
files_local <- vector(mode = "character", length = length(files2))
dir.create(file.path("./", "temp"), showWarnings = FALSE)
for (i in 1:length(files2)) {
  # Put name in files_local
  files_local[i] <- paste("./temp/", names2[i])
  # Download file
  drive_download(files2[i], path = files_local[i], overwrite = TRUE)
}
EnormousData <- data.frame()
# Read content of all files into a list
listOfDataframes <- lapply(files_local, 
                           function(x) {
                             read_excel(x)
                           }
)
# Add all the rows from all the files together
EnormousData <- do.call("bind_rows", listOfDataframes)


# GRAPHING THINGS ------------------------------

### BOXPLOTS ### ---------------
# Generally you use a boxplot when plotting a continuous y
# against a categorical x axis (e.g. an outcome against 2 treatments).

# Bare boxplot
boxplot(avgsize ~ treatment
        , data = MyData
        , xlab = "X Concept [unit measured]"
        , ylab = "Y Concept [unit measured]"
        , col = threegroupcolors
        , range = 0 # to get whiskers to extend to range (no outlier points)
)

# Fancy boxplot - but this should be your default
# What makes it fancy:
# Adjust margins
# Add sample sizes
# Add data points

# Margins:
par(oma = c(2,2,2,2), mar = c(4,4,1,1), mgp=c(3, 1, 0), las=1) 
# bottom, left, top, right
par(mfrow=c(1,1))

# How to make a great boxplot -

# Saving the plot into a variable allows us to access plot parameters afterwards.
Nice_Plot <- boxplot(avgsize ~ treatment
                     , data = MyData
                     , xlab = "X Concept [unit measured]"
                     , ylab = "Y Concept [unit measured]"
                     , range = 0
# Always make axis descriptions as clear and comprehensive as possible
                     , names = c("Large bees", "Middling bees", "Small bees")
                     , col = alpha(threegroupcolors, 0.5) # use same colors as elsewhere, 
                     # but slightly transparent so we can see data points
                     , ylim = c(y_min, y_max) # always think about the scale - starting from zero is typically better
)

# Putting sample sizes above bars
nbGroup <- nlevels(as.factor(Nice_Plot$names)) # this is just a way to extract
# category names from the plot - you could get this directly from data
text(x=c(1:nbGroup) 
  , y=N_y_offset*y_max
  , cex = 1
  , col = threegroupcolors
  , paste("N=", Nice_Plot$n, sep="")  # again, the sample size 'n' is directly extracted from the plot
)

mtext("additional margin label", side=2, line=2, las=0)
mtext("additional margin label on outside", side=1, line=4, las=0, xpd = TRUE)

# Represent the raw data as well, especially for mid- to low sample sizes.
stripchart(avgsize ~ treatment
           , data = MyData
           , add = TRUE # this plots this graph on top of the existing one
           , pch = 19
           , col = threegroupcolors
           , method = "jitter"
           , jitter = 0.2
           , vertical = TRUE
)



### SCATTERPLOTS ### ---------------
# You may or may not need all the features included here. But at minimum, you want
# fairly large points, in almost all cases make them slightly transparent, and
# really clear axis labels. 
# These are just the standard margins I use - you could have something 
# else - but I want to make sure they are reset here in case you were experimenting
# with it before. 
par(oma = c(0,0,0,0), mar = c(4,4,1,1), mgp=c(3, 1, 0), las=1) 
# bottom, left, top, right
par(mfrow=c(1,1))
graph_data <- MyDataLocalFolder
plot(avgsize ~ stdevsize 
     , data = graph_data
     , pch = 19 # set point shape
     , col = threegroupcolors[graph_data$treatmentcode]
    #, col = threegroupcolors[sapply(graph_data$treatment, function(x) switch(x, "S"=1, "M"=2, "L"=3))]
     # So you can use the first line if you have a numerical column you want to use
     # to determine the color, or you use the second version if you have a set of label names. 
     , cex = 1.5 # point size - 1 is default, but I like them bigger
     , xlab = "X-Axis Label [units]"
     , ylab = "Y-Axis Label [units]"
)
# In some cases, it makes sense to label points individually. 
label_xoffset <- -max(graph_data$stdevsize) * 0.002 # you have to play around with 
# this to see what looks good. I do it relative to the x-axis for comparability
# between plots. 
label_yoffset <- 0
text(graph_data$stdevsize + label_xoffset # x coordinates of labels
     , graph_data$avgsize + label_yoffset # y coordinates of labels
     , labels = graph_data$queensproduced # text in labels
     , cex = 0.5 # size of text
     , pos = 4 # make the text left aligned (to the right of given coordinates)
)


# Let's try something like this with a larger dataset:
graph_data <- EnormousDataLocal
# I'm going to redefine colors here to make sure it fits with this dataset -
# in your own script, you would presumably do this in the graphics settings
# at the top.
num_colors <- 15
# I also want the points to be semi-transparent. This is called 'alpha', which is 
# a number between 0 (transparent) and 1 (totally opaque). There are different 
# functions to do this, but it's also built-in in the viridis package color scales.
colorgradient <- magma(num_colors, alpha = 0.6)
# For that, I 'cut', i.e. categorize, a continuous variable into the number of 
# colors I want to use. 
graph_data$colors <- cut(graph_data$total.agent.timesteps.spent.searching, breaks = num_colors)

# Ok now the actual graph code:
par(oma = c(0,0,0,0), mar = c(4,4,1,1), mgp=c(3, 1, 0), las=1) 
# bottom, left, top, right
par(mfrow=c(1,1))
plot(collected.resource.units ~ average.exploited.resource.distance
     , data = graph_data
     , pch = 19 # set point shape
     , col = colorgradient[graph_data$colors]
     , cex = sapply(as.character(graph_data$number.of.clusters), function(x) switch(x, "5"=0.5, "10"=1, "50"=1.5, "100"=2))
     , xlab = "X-Axis Label [units]"
     , ylab = "Y-Axis Label [units]"
)
# Here I used 'number.of.clusters' to define the size of the points as well. 

# We can even upgrade this to a multi-panel plot, if we want a boxplot on the sides
# to show the distribution of values. This makes some sense here also since the points
# overlap so much it is hard to tell how many are where.
graph_data <- EnormousDataLocal

# For this we can use another par() setting (e.g. mfrow=c(2,2)), or for more 
# control use 'layout()'.
layout(matrix(c(1,2,0,3), 2, 2, byrow = T), widths=c(1,5), 
       heights=c(5,1)) 
# This gives us a four-panel plot; the plots will be inserted into the panels in 
# order by row.
# How: matrix() gives the table, it has 4 entries, so 4-panel plot. 
# byrow = T means we are labeling the 4 panels by row, i.e. first the first row, 
# then the second, etc.
# Inside matrix(), the four numbers are the order in which plots below will be 
# inserted into the four panels: first the first one (top left), then the second 
# (top right) then the bottom right panel is empty, then the bottom left is number 
# 3. So we expect a total of three plots below. 
# heights() specifies the heights of the two rows. The first row is much taller than
# the second row. widths() specifies the width of the columns: the second column is
# much wider. So plot number 2 will be both wide and high, plot number one will be tall 
# but narrow, and plot number 3 is wide and short. The cell in bottom left doesn't
# have a plot but is both short and narrow. 

# See above for the color strategy here. 
num_colors <- 15
colorgradient <- magma(num_colors, alpha = 0.6)
graph_data$colors <- cut(graph_data$total.agent.timesteps.spent.searching, breaks = num_colors)

# Various other format adjustments
par(oma = c(0,0,0,0), mgp=c(3, 1, 0), las=1)

# Panel 1: Distribution of y-values
par(mar = c(4,0,1,0)) # bottom, left, top, right
boxplot(graph_data$collected.resource.units
        , xaxt = 'n'
        , yaxt = 'n'
        , frame = FALSE
        , range = 0
        , col = twogroupcolors[1]
)

# Panel 2: The main graph, a scatterplot
par(mar = c(4,4,1,2)) # bottom, left, top, right
plot(collected.resource.units ~ average.exploited.resource.distance
     , data = graph_data
     , pch = 19 # set point shape
     , col = colorgradient[graph_data$colors]
     , cex = sapply(as.character(graph_data$number.of.clusters), function(x) switch(x, "5"=0.5, "10"=1, "50"=1.5, "100"=2))
     , xlab = "X-Axis Label [units]"
     , ylab = "Y-Axis Label [units]"
)

legend("topright"
       , title = "Time searching"
       , c(paste("t=", min(graph_data$total.agent.timesteps.spent.searching))
           , paste("t=", median(graph_data$total.agent.timesteps.spent.searching))
           , paste("t=", max(graph_data$total.agent.timesteps.spent.searching)))
       , col = c(colorgradient[1], colorgradient[round(num_colors/2)], colorgradient[num_colors])
       , pch = 19 # you'll normally match the shape of the scatterplot points
)

# Panel 3: empty
# We put a 0 in the layout matrix there, so R should know we don't want this to be used
# One could put a legend or text here if needed.

# Panel 4: Boxplot of x-axis values
par(mar = c(0,4,0,2)) # bottom, left, top, right
boxplot(graph_data$average.exploited.resource.distance
        , xaxt = 'n' # This deletes this axis
        #, yaxt = 'n'
        , frame = FALSE
        , range = 0
        , col = twogroupcolors[2]
        , horizontal=TRUE
)

# Note that if you are doing a graph of this type, I would
# - make sure the boxplot colors are not the same as the scatterplot points but somehow
# make sense in the context. Possibly better to leave the boxplots white.
# - make sure the color gradient doesn't have a very light yellow like this one that
# is nearly invisible when transparent (see third point in legend); or make sure the point
# in the legend does not have transparency. 
# - one could also squish the boxplots on the sides more by changing the heights and widths
# argument in layout, to get rid of more of the white space.



