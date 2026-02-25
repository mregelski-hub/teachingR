## CODE TO GO WITH BOOKCLUB
## ON Richard McElreath's 'Statistical Rethinking'
# Anna Dornhaus, 2025

########### PART III: QUAP, linear regressions with two variables ##########

## LIBRARIES  -----------------------------
library(rethinking)
library(scales)
library(ellipse)
## Loading the data we'll use -----------------
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

## WHAT THIS SCRIPT CONTAINS ---------------------
## This script is about the second part of chp 4 and video 3
## 2nd edition of the book, videos from 2023
## Using the Howell human growth data
## Important techniques here: using quap; building a generative model first;
## fitting two response variables (weight and height) and their relationship

## Chapter 4 2nd part & Video 3 2nd part ---------------
## QUAP - starting with quadratic approximations, not grid approximations ----------------

### ONE PARAMETER - HEIGHT----------------------
## We'll redo the one-response-variable fit from script II, but this time
## with the quap() function. 
## Remember although there is only one response variable, there are two parameters
## we are trying to fit: mu and sigma. 
list_of_assumptions <- alist(
  height ~ dnorm( mu , sigma ), #likelihood
  mu ~ dnorm( 178 , 20 ), #prior
  sigma ~ dunif( 0 , 50 ) #prior
)
resulting_fit <- quap(list_of_assumptions, data=d2)
## This function gives us a summary of the results, i.e. the posterior for the
## two parameters.
precis(resulting_fit)

# Optionally, you can tell quap to use overall distribution summaries
# as starting values for the hill-climbing of finding the peak of the posterior.
# So here we are repeating the same analysis, but in addition to the assumptions
# and data we also input a set of starting values to quap(). 
start <- list(
  mu=mean(d2$height),
  sigma=sd(d2$height)
)
resulting_fit <- quap(list_of_assumptions, data=d2, start=start)
precis(resulting_fit)

#### Covariances --------------------------
## Note that the distributions given by precis() reflect the probabilities for
## each parameter given the probability distribution for the other parameter.
## The two parameters are not independent. 

# We can also show the variance-covariance matrix. 
vcov(resulting_fit)
# Explanation: The diagonal shows the variance in each parameter, while the other
# cells in the matrix show correlations among each parameter pair. 
variances <- diag(vcov(resulting_fit)) # (marginal) variances in individual parameters
sqrt(variances) # Square root of variances gives standard deviations from precis

cov2cor(vcov(resulting_fit))
# The latter tells us the correlation coefficients between any two parameters 
# without the variances of the parameters themselves (the original diagonal).

#### Sampling from posterior ------------------------

# The rethinking package can generate samples directly from a model fit
samples_of_posterior <- extract.samples(resulting_fit, n=10000)
# Or:
#library(MASS)
#samples_of_posterior <- mvrnorm(n=1e4, mu=coef(resulting_fit), Sigma=vcov(resulting_fit))
head(samples_of_posterior)
precis(samples_of_posterior)
plot(samples_of_posterior) # Note this plots the parameters 'mu' (mean height) on the x
# and sigma (standard deviation in height across individuals) on the y axis

# Ploting the data as well as the samples from the posterior
boxplot(d2$height
        , range = 0 # makes the whiskers extend to the extremes, i.e. no outlier points
        )
stripchart(d2$height   # Actual datapoints
           , method = "jitter" # Random noise horizontally, to spread points
           , pch = 19          # pch sets the symbol: here a filled circle
           , vertical = TRUE   # vertical stripchart, i.e. point distribution
           , add = TRUE        # add to previous plot instead of making a new plot
           , col = alpha("black", 0.5) # col is color, and alpha() defines degree of transparency
           )

stripchart(samples_of_posterior$mu # posterior distribution of average, i.e. estimate of average w uncertainty
           , method = "jitter" # Random noise horizontally, to spread points
           , pch = 19          # pch sets the symbol: here a filled circle
           , col = alpha("blue", 0.1) # We have 10000 data points here...
           , vertical = TRUE
           , add = TRUE)
# We can see that the posterior overlaps the data mean, and is much tighter around
# it than the actual spread of the data (as expected).

# The above code thus uses a Bayesian estimation method to get the mean 
# of a random variable and its standard deviation.
# One could equally interpret this in an NHST way by checking whether some 
# comparison mean is within 95% of this distribution of points. 


### TWO PARAMETERS - HEIGHT AND WEIGHT --------------------
## Back to video 3

# Define limits - this is initially for the sake of the graph, but later is
# actually used to define the ranges of simulated data.
min_height <- 130
max_height <- 180
min_weight <- 25
max_weight <- 80

##### Data --------------------
# This is a plot of the actual data.
plot(weight~height
     , data = d2
     , lwd = 3
     , xlim = c(min_height, max_height)
     , ylim = c(min_weight, max_weight)
     , col = alpha("slategray", 0.5)
)

##### Simulation (generative model) -------------
# To better understand what we're looking at, we first build a generative 
# model, i.e. a simulation. 

# Define stuff for the simulation.
n_samples <- 200
# Essentially, this is formalizing a particular hypothesis about the true 
# pattern (relationship between variables/quantitative values of parameters).
# I.e. we are assuming these values (slope, intercept, and error term) and 
# also a distribution for H for a hypothetical dataset, and then calculating
# simulated samples from that dataset. 
slope_b <- 0.7
error_term <- 5
# NOTE: Intercept is missing in book because he assumes it is 
# zero (it is obviously not necessarily zero for a
# real dataset of people where we just make the simplifying
# assumption that their weight is linearly related to height. 
# Let's make it more general by defining an intercept:
intercept_w_h <- -60

# We assume that weight W is proportional to height H, but with a normally 
# distributed error U. The simulation function takes as input a vector of heights,
# the slope, intercept, and standard deviation of the error. 
sim_weight <- function(H, b, i, sd) {
  U <- rnorm(length(H), 0, sd)
  W <- b * H + i + U
  return(W)
}

# To simulate data by first picking n_samples heights. 
H <- runif(n_samples, min = min_height, max = max_height)
# Or perhaps we want height also normally distributed
H <- rnorm(n_samples, mean = ((min_height+max_height)/2), sd = 15)
# Whichever of the two lines you ran (last) defines a set of heights (this is
# part of your hypothesis about the true pattern).
# Now we simulate corresponding weights:
W <- sim_weight(H, slope_b, intercept_w_h, error_term) # imagining units of kg and cm

points(W~H
       , col = alpha("slateblue2", 0.5)
       , pch = 19
)
# We can see that the simulated, fake data are similarly (though not identically)
# distributed as the real data. But the key point is that they reflect a particular
# hypothesis about a generative pattern. 

# This generative model, and the resulting simulated data, serve multiple purposes.
# First, by defining a generative model, we got some clarity about
# what our assumptions are about how these data are distributed and how the parameters
# relate to one another. The generative model could include other known processes
# like errors in measurement etc. Either way, the generative model is a tool
# to produce a bunch of fake data for which we actually *know* the parameters
# (e.g. we know the actual mean and standard deviation of the error with which
# they were produced, and the slope and intercept with which height and weight
# relate to one another). 
# Second, the generative model reflects a particular hypothesis. It shows what our
# results might look like if that hypothesis were true. 
# With these fake data in hand, we can run our planned analysis workflow and see
# how well our analysis recovers those true parameter values. Once we are convinced
# that our analysis can recover the true parameter values of data generated this way,
# we can run it on real empirical data to estimate the parameter values for that,
# and thus test our hypothesis about the parameter values. 

##### Looking at different possible priors ----------------
# Chapter 4 treats this in slightly different order from video 3. 
# It uses Hi - H bar instead of H in the model, so that the intercept
# is at the mean of H rather than at 0.
# It also first regresses height on weight rather than the other way around.

# And we first simulate entire priors, e.g.:
# set.seed(2971)
# A prior is a probability distribution for each parameter.
# Here the parameters of interest are the slope and the intercept. 
# We simulate N pairs of slope and intercept, which are samples from the distributions
# that are our prior for these parameters. Here, the implication is that our
# prior for the intercept is a normally distributed 178 +/-20, and our prior for
# the slope is a 0 +/- 10. 
N <- 100
# Now we generate a bunch of intercepts and slopes based on the prior. 
# Possible intercepts
intercepts <- rnorm(N, 178, 20)
# Possible slopes
slopes <- rnorm(N, 0, 10)
a <- intercepts
b <- slopes
# We plot a coordinate system with nothing in it
plot(NULL 
     , xlim=range(d2$weight) 
     , ylim=c(-100,400) 
     , xlab="weight" 
     , ylab="height" 
     )
# We plot the height of an embryo
abline( h=0 , lty=2 )
# And the height of the tallest man
abline( h=272 , lty=1 , lwd=0.5 )
# Explanation about the priors we're depicting here
mtext( "slopes ~ dnorm(0,10)" )
# Calculate the average of actual weights
xbar <- mean(d2$weight)
# Now, into this coordinate system, we plot our 100 samples from our prior.
# These are 
for (i in 1:N) 
  curve(a[i] + b[i]*(x - xbar)
        , from=min(d2$weight)
        , to=max(d2$weight)
        , add=TRUE
        , col=col.alpha("black",0.2) 
        )
# The plot now shows a bunch of possible linear relationships between weight and
# height based on the prior.

# (We're still in video 3)
# As stated in (my) Part II, he likes priors to be somewhat more informative than this.

### Whole Bayesian analysis for the weight ~ height relationship -----------------
# Better prior, and for the reverse relationship (weight on height),
# as well as defining the intercept at mean (not zero), and doing it with any
# number of points. 

###### Data subset ------------------------
## First we subset the data for the case that we want to run our analysis with 
# only a limited number of data points.
# The original has 352 rows
N_data <- 50 # Number of data points we'll use
# His code to subsample data:
# dN <- d2[1:N_data, ] # Subset of data with N_data rows
# I want to use a random sample rather than the N_data rows from the top:
dN <- d2[sample(1:nrow(d2), N_data, replace=FALSE),]
# The following is the mean of all the data, not just the ones we are using.
xbar <- mean(d2$height)

###### Sampling from prior to illustrate prior ------------------------
## Then we plot some samples from our prior, i.e. some of the possible linear
# relationships before information from data is used. 
n_plot <- 30 # Number of lines we'll plot
# Priors
intercept_prior_mean <- 50
intercept_prior_SD <- 50
slope_prior_min <- 0
slope_prior_max <- 1
sigma_prior_max <- 20

# Simulate some points from priors for plot
intercepts_at_mean <- rnorm(n_plot, intercept_prior_mean, intercept_prior_SD)
slopes <- runif(n_plot, slope_prior_min, slope_prior_max)

# Another possible prior that enforces a positive slope is
# b <- rlnorm(N, 0, 1)
# Defining b as Log-Normal(0,1) means to claim that the logarithm of b has a 
# Normal(0,1) distribution. If the logarithm of b is normal, then b itself is 
# strictly positive. The reason is that exp(x) is greater than zero for any real 
# number x.

###### Plot the frame ----------------------
plot(NULL 
     , xlim= c(min_height, max_height)   # Or: range(d2$height)
     , ylim= c(min_weight, max_weight)   # Or: range(d2$weight)
     , xlab="Height [cm]" 
     , ylab="Weight [kg]")
# Realistic limits on height; if xlim and ylim are defined, as above, as the 
# actual data range, then these vertical lines indicating height = 0 or height
# = 272 won't show up. 
abline(v=0, lty=2)
abline(v=272, lty=1)

# Describing the prior we used
mtext(paste("Priors: intercept at mean =", intercept_prior_mean
            , ", slope max =", slope_prior_max
            , ", min =", slope_prior_min, sep = "")
      , cex = 0.75)

###### Plot the prior ----------------------
# Now we plot our samples from the prior, i.e. the possible lines from the probability
# distribution reflecting our pre-data assumptions about possible relationships
# between weight and height. 
for ( i in 1:n_plot ) 
  curve(intercepts_at_mean[i] + slopes[i]*(x-xbar)
        , from=min_height, to=max_height
        , add=TRUE 
        , col=col.alpha("black",0.3) 
        )
###### Plotting real data ------------------------

# Plotting the real data points;
# first we plot all of them 
points(d2$weight ~ d2$height
       , col = alpha("slateblue2", 0.3)
       , lwd = 1)
# and then highlight the ones we actually decided to use (the subset defined above)
points(dN$weight ~ dN$height
       , col = alpha("slateblue2", 0.7)
       , lwd = 3)
legend("bottomright"
       , legend = c("Prior", "Data", "All data")
       , col = c("black", alpha("slateblue2", 0.7), alpha("slateblue2", 0.3))
       , lwd = c(1, 3, 1)
       , lty = c(1, NA, NA)
       , pch = c(NA, 1, 1)
       #, bty="n"
)

###### RUnning the Bayesian analysis ------------------
# What we want next is our statistical model and Bayesian estimation of posterior.
# But McElreath emphasizes that before we feed it real data, we should feed it a variety of simulated
# data to see that it 'works'.
# Primarily two things:
# When feeding large sample sizes, should converge on actual answer (e.g. slope).
# When simulating different slopes, should track those.

# I'm going to define starting values for the approximation algorithm
# only so that it is more likely to succeed at all with few data points
start <- list(
  a=mean(d2$weight),
  b=0.5,
  sigma= sd(d2$weight)
)

# His actual model code:
# (note I changed the priors to match the ones above)
dat <- list(W=dN$weight, H=dN$height)
model1.0 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*(H - xbar),
    # Again centering intercept at the average height
    # Alternative is:
    # mu <- a + b*H,
    a ~ dnorm(intercept_prior_mean, intercept_prior_SD),
    b ~ dunif(slope_prior_min, slope_prior_max),
    sigma ~ dunif(0, sigma_prior_max)
  )
  , data = dat
  , start = start
)
precis(model1.0)
# Note that actually three parameters are being fitted, namely slope, intercept,
# and sigma of the error.
# These parameter estimates are not in fact independent of one another. 

###### Plotting the posterior ----------------------

# We can better understand the posterior by sampling from it
samples_of_posterior <- extract.samples(model1.0, n=n_plot)

a <- samples_of_posterior$a
b <- samples_of_posterior$b
for ( i in 1:100 ) 
  curve(a[i] + b[i]*(x-xbar)
        , from=min_height, to=max_height 
        , add=TRUE 
        , col=col.alpha("darkred", 0.1) 
        , lwd=3
        )
# Above is same code as for priors, but in the video he also uses the much shorter
# for (j in 1:100) abline(a=a[j], b=b[j], col=col.alpha("darkred", 0.1))

# Note that this is not plotting sigma, but it is plotting only valid combinations
# of slope and intercept, rather than slopes and intercepts drawn independently from 
# the resulting distribution.

###### Plotting the prediction envelope ----------------------
# But we can also draw the prediction envelope around the line(s).

# We do this by first creating a sequence of possible x-values
height_seq <- seq(min_height, max_height, len=n_plot)
# The n_plot just determines the resolution here - doesn't need to be the same
# value as what we used for number of lines above.

# Now we use a built-in function to simulate data based on the posterior, for
# the given x-values (heights)
sim.weight <- sim(model1.0, data = list(H=height_seq))

# Remember 'PI' stands for percentile interval. This gives us the 95% interval
# of the points in sim.weight.
weight.PI <- apply(sim.weight, 2, PI, prob=0.95)
# Now we add a shaded interval for each x-axis value that corresponds to the 
# 95% interval of the corresponding weights.
shade(weight.PI, height_seq
      , col = col.alpha("lightgrey", 0.4)
      )

# We could add another one:
weight.PI <- apply(sim.weight, 2, PI, prob=0.50)
shade(weight.PI, height_seq
      , col = col.alpha("lightgrey", 0.6)
)
# In hindsight it might be better for an actual plot to plot these intervals 
# first, then the posterior lines and then the data points on top. 
# So that's what we're doing below, also with slightly different colors:

###### Redoing the entire graph ---------------------
plot(NULL 
     , xlim=range(d2$height)
     , ylim=range(d2$weight)
     , xlab="Height [cm]" 
     , ylab="Weight [kg]")
for ( i in 1:n_plot ) 
  curve(intercepts_at_mean[i] + slopes[i]*(x-xbar)
        , from=min(d2$height) , to=max(d2$height) 
        , add=TRUE 
        , col=col.alpha("black",0.2) 
  )
for ( i in 1:50 ) 
  curve(a[i] + b[i]*(x-xbar)
        , from=min(d2$height) , to=max(d2$height) 
        , add=TRUE 
        , col=col.alpha("violetred4", 0.3) 
        , lwd=3
  )
weight.PI <- apply(sim.weight, 2, PI, prob=0.95)
shade(weight.PI, height_seq
      , col = col.alpha("violetred1", 0.08)
)
lines(height_seq, weight.PI[1,], lty=2, lwd=2
      , col=col.alpha("violetred3", 0.3))
lines(height_seq, weight.PI[2,], lty=2, lwd=2
      , col=col.alpha("violetred3", 0.3))
# Adding the 50% interval for emphasis
weight.PI <- apply(sim.weight, 2, PI, prob=0.50)
shade(weight.PI, height_seq
      , col = col.alpha("violetred1", 0.09)
)
lines(height_seq, weight.PI[1,], lty=2, lwd=2
      , col=col.alpha("violetred3", 0.3))
lines(height_seq, weight.PI[2,], lty=2, lwd=2
      , col=col.alpha("violetred3", 0.3))
# Data points we are not using
points(d2$weight ~ d2$height
       , col = alpha("slateblue2", 0.5)
       , lwd = 1)
# Data points (actual, not simulated)
points(dN$weight ~ dN$height
       , col = alpha("slateblue2", 0.7)
       , pch = 19
       , cex = 1.2)

# You may want to run the entire code above again with different values for
# N_data, i.e. allowing the Bayesian model to use different numbers of data points.
# How much does the spread of the lines & prediction interval increase if the model
# is just based on 5, 20, vs 100 or 352 data points?









