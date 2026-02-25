## CODE TO GO WITH BOOKCLUB
## ON Richard McElreath's 'Statistical Rethinking'
# Anna Dornhaus, 2025

########### PART IV: Workflow and Curves ##########
## I recommend 'closing' all sections for better readability, and only opening the section
## you are working on: click on the small triangles by the line numbers to close/open
##
## LIBRARIES  -----------------------------
library(rethinking)
library(scales)
library(splines)
library(viridisLite)
## Loading the data we'll use -----------------
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

## WHAT THIS SCRIPT CONTAINS ---------------------
## This script has two parts:
## First, actually going through the whole workflow for the weight(height)
## fitting.
## Second, about the third part of chp 4 and video 4
## 2nd edition of the book, videos from 2023
## Using the Howell human growth data, this time with kids:
## fitting a curve rather than linear relationship

### Proper analysis workflow -------------------
## First we practice what we preach by testing model with simulated data, 
# Define generative model
# Standardize data first
# Define Bayesian model & priors
# Run model on several simulated data and make sure it recovers parameters
# Run model on real data
# Illustrate & interpret results, e.g. parameters, prediction intervals

# Important points McElreath makes:
# * Standardize the data first, by subtracting mean and dividing by standard deviation. 
# This gives a 'z-score' and makes interpreting the intercepts and slopes easier.
# * Testing the workflow forces you to examine what you think the generative model
# is and making sure there are neither coding nor interpretive errors.

#### Define generative model -------------------------
# We assume that weight W is proportional to height H, but with a normally 
# distributed error U. This means we have parameters slope (b), intercept (i),
# and standard deviation of U (sd). 
# We're going to assume the heights H are not yet standardized.
sim_weight <- function(H, b, i, sd) {
  U <- rnorm(length(H), 0, sd)
  W <- b * (H - mean(H)) + i + U
  return(W)
}

#### Define hypothesis simulated ------------------

# I'm going to assume that to properly test our analysis workflow, we're going
# to simulate data with different sample sizes. Lets say
N_1 <- 3
N_2 <- 10
N_3 <- 100

# Parameters for the generative model. We assume units of kg and cm.
slope_b <- 0.7 # b
intercept_w_h <- 50 # i
error_term <- 5 # sigma

# When we are generating (fake) data, we also need to define the set of heights
# we want to use. We'll pick normally distributed heights.
mean_height <- 170
sd_height <- 10

# Now, we're on the way to generating three sets of fake data for our model:
heights1 <- rnorm(N_1, mean = mean_height, sd = sd_height)
heights2 <- rnorm(N_2, mean = mean_height, sd = sd_height)
heights3 <- rnorm(N_3, mean = mean_height, sd = sd_height)

#### Generating the actual simulated data ---------------------
# After this, we can simulate corresponding weights
weights1 <- sim_weight(heights1, slope_b, intercept_w_h, error_term) 
simdata1 <- data.frame(heights1, weights1)
colnames(simdata1) <- c("Height", "Weight")
weights2 <- sim_weight(heights2, slope_b, intercept_w_h, error_term) 
simdata2 <- data.frame(heights2, weights2)
colnames(simdata2) <- c("Height", "Weight")
weights3 <- sim_weight(heights3, slope_b, intercept_w_h, error_term) 
simdata3 <- data.frame(heights3, weights3)
colnames(simdata3) <- c("Height", "Weight")

### Prep real data ---------------------------------------
# The real data we will use is d2.
realdata <- d2
colnames(realdata) <- c("Height", "Weight")

### Standardize data (both real and simulated) ----------------------------
# Standardization done prior to model is better he says.
zscore <- function(dat) {
  z <- (dat - mean(dat)) / sd(dat)
  return(z)
}

simdata1_z <- data.frame(zscore(simdata1$Height), zscore(simdata1$Weight))
simdata2_z <- data.frame(zscore(simdata2$Height), zscore(simdata2$Weight))
simdata3_z <- data.frame(zscore(simdata3$Height), zscore(simdata3$Weight))
realdata_z <- data.frame(zscore(realdata$Height), zscore(realdata$Weight))
colnames(simdata1_z) <- c("Height", "Weight")
colnames(simdata2_z) <- c("Height", "Weight")
colnames(simdata3_z) <- c("Height", "Weight")
colnames(realdata_z) <- c("Height", "Weight")

# Let's make sure this worked correctly. We'll plot the untransformed next to the 
# transformed data.
# Defining a nice color set
sim_sets_colors <- viridis(3, alpha = 0.7, begin = 0.1, end = 0.9)
# Determining extent of axes
h_max <- max(simdata1$Height, simdata2$Height, simdata3$Height, realdata$Height)
w_max <- max(simdata1$Weight, simdata2$Weight, simdata3$Weight, realdata$Weight)
h_min <- min(simdata1$Height, simdata2$Height, simdata3$Height, realdata$Height)
w_min <- min(simdata1$Weight, simdata2$Weight, simdata3$Weight, realdata$Weight)
# Two graphs next to each other
par(mfrow=c(1,2))
# Plotting the real data first
plot(realdata$Weight ~ realdata$Height
     , col = alpha("slateblue", 0.3)
     , pch = 19
     , xlim = c(h_min, h_max)
     , ylim = c(w_min, w_max)
     , xlab = "Height"
     , ylab = "Weight"
     #, main = "Data in real units"
     )
points(Weight ~ Height
       , data = simdata3
       , bg = sim_sets_colors[3]
       , pch = 21
       , col = "black"
      )
points(Weight ~ Height
       , data = simdata2
       , bg = sim_sets_colors[2]
       , pch = 21
       , col = "black"
       )
points(Weight ~ Height
       , data = simdata1
       , bg = sim_sets_colors[1]
       , pch = 21
       , col = "black"
       )
legend("bottomright"
       , legend = c("Real data", "Sim data 1", "Sim data 2", "Sim data 3")
       , col = c("slateblue", sim_sets_colors[1], sim_sets_colors[2], sim_sets_colors[3])
       , pch = 19
       #, bty="n"
       #, cex = 0.75
)
mtext("Data in real units", 3, 2, cex = 1.25)
mtext(paste("3 simulated sets with N1=", N_1, ", N2=", N_2, ", N3=", N_3, sep = ""), 3, 1)
# Now we'll plot the same 4 datasets but in their standardized form.
plot(realdata_z$Weight ~ realdata_z$Height
     , col = alpha("slateblue", 0.3)
     , pch = 19
     , xlim = c(-3, 3)
     , ylim = c(-3, 3)
     , xlab = "Z-Score Height"
     , ylab = "Z-Score Weight"
)
points(Weight ~ Height
       , data = simdata3_z
       , bg = sim_sets_colors[3]
       , pch = 21
       , col = "black"
)
points(Weight ~ Height
       , data = simdata2_z
       , bg = sim_sets_colors[2]
       , pch = 21
       , col = "black"
)
points(Weight ~ Height
       , data = simdata1_z
       , bg = sim_sets_colors[1]
       , pch = 21
       , col = "black"
)
mtext("Standardized x", 3, 2, cex = 1.25)
# What's different between these two graphs?
# The overall orientation of points to each other, within each dataset (!),
# should be the same. However, the point cloud should have moved to be 
# centered on 0 in both dimensions (axes), and the axis that had less variation
# will be drawn out, so that the amount of variation looks the same in both 
# axes. 

### Define priors -------------------------------------
# Remember priors are really also probability distributions (just like
# likelihood and posterior are). 
# So we are not defining a single slope as the prior, but over all slopes,
# we are defining how likely we find each of them (prior to knowing about
# the current dataset).

# Here we are going to assume that our prior for the intercept is normally
# distributed, our prior for the slope is uniformly distributed, and our
# prior for sigma is also uniformly distributed and its minimum is 0.
# Given these assumptions about the shape of the prior, we just have to define
# these parameters:
intercept_prior_mean <- 50
intercept_prior_SD <- 25
slope_prior_min <- 0
slope_prior_max <- 10
sigma_prior_max <- 20

# Here is the formal list of the assumptions:
list_of_assumptions <- alist(
  W ~ dnorm(mu, sigma),
  mu <- a + b*H,
  a ~ dnorm(intercept_prior_mean, intercept_prior_SD),
  b ~ dunif(slope_prior_min, slope_prior_max),
  sigma ~ dunif(0, sigma_prior_max)
)

# Let's plot some samples from this prior to make sure it's reasonable.
n_plot <- 100
intercepts_at_mean <- rnorm(n_plot, intercept_prior_mean, intercept_prior_SD)
slopes <- runif(n_plot, slope_prior_min, slope_prior_max)
for ( i in 1:n_plot ) 
  curve(intercepts_at_mean[i] + slopes[i]*x
        , from=-3, to=3
        , add=TRUE 
        , col=col.alpha("black",0.3) 
  )
mtext("Lines are samples from prior", 3, 1)

#### Define Bayesian model procedure -------------------
# This is where the model fitting happens, i.e. here we use quap().

# It is helpful to define starting values, as the quap() optimization
# procedure can fail, especially with too few data points. 
# Hopefully what we do here has no influence on the results, just on whether
# the approximation procedure works or fails. 
start <- function(dat) {
  return(
    list(
      a = mean(dat$Weight)
      , b = slope_prior_min + (slope_prior_max-slope_prior_min)/2
      , sigma = sd(dat$Weight)
    )
  )
}

# We define the Bayesian procedure as a function that can be run on any dataset
# that is given as argument. 
W_H_model <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(W=dat$Weight, H=dat$Height)
    , start = start(dat)
    )
}

#### Run model on simulated data ------------------------
# Ok we defined our model as a function above, now we run it on some data.

# First, we want to check it is working on the simulated data.
post_sim1 <- W_H_model(simdata1_z)
precis(post_sim1)
post_sim2 <- W_H_model(simdata2_z)
precis(post_sim2)
post_sim3 <- W_H_model(simdata3_z)
precis(post_sim3)

# We actually know what the 'correct' values for a, b, and sigma are - they are our 
# assumptions in the generative model of b (the slope), a (the intercept), and the
# error term (sigma).
slope_b
intercept_w_h
error_term
# Now, our original slope is in terms of the real units of x (i.e. height), so
# it won't be the same as what we found in the posterior. 
# But the other two parameters should be close, and in fact should be getting 
# closer to the real value with higher sample size (from simdata1 to simdata3).

##### Plot the results for simulated data to test working of the model ------------
####### Plot frame & simdata points ------------
par(mfrow=c(1,1))
plot(realdata$Weight ~ realdata$Height
     , col = alpha("slateblue", 0.1)
     , pch = 1
     , xlim = c(h_min, h_max)
     , ylim = c(w_min, w_max)
     , xlab = "Height [cm]"
     , ylab = "Weight [kg]"
)
points(Weight ~ Height
       , data = simdata3
       , bg = sim_sets_colors[3]
       , pch = 21
       , col = "black"
)
points(Weight ~ Height
       , data = simdata2
       , bg = sim_sets_colors[2]
       , pch = 21
       , col = "black"
)
points(Weight ~ Height
       , data = simdata1
       , bg = sim_sets_colors[1]
       , pch = 21
       , col = "black"
)
mtext("Data in real units", 3, 2, cex = 1.25)
mtext(paste("3 simulated sets with N1=", N_1, ", N2=", N_2, ", N3=", N_3, sep = ""), 3, 1)

####### Convert back to real data from z scores ------------

# In order to plot our estimated relationships (with slopes and intercepts) on this
# graph, we need to convert the parameters based on the z-scores back to ones
# based on original units.
slope_from_stand_x_slope <- function(z_slope, x_values) {
  new_slope <- z_slope/sd(x_values)
  return(new_slope)
}
intercept_from_stand_x_slope <- function(z_intercept, x_values, real_slope) {
  new_intercept <- z_intercept - mean(x_values) * real_slope
  return(new_intercept)
}

####### Plot the prior (for slope & intercept) as a distribution of lines ------------
x_distr <- rnorm(100, mean = mean_height, sd = sd_height)
slopes_real <- slope_from_stand_x_slope(slopes, x_distr)
intercepts_real <- intercept_from_stand_x_slope(intercepts_at_mean, x_distr, slopes_real)
for ( i in 1:n_plot ) 
  abline(intercepts_real[i], slopes_real[i]
         , col = alpha("black", 0.1) 
         )
mtext("Thin lines are samples from prior", 3, 0, cex = 0.75)

####### Plot the posterior for each of the three simulated datasets ------------
# Reading out the posterior mean slope and intercept from the precis table,
# and converting back to real units:
post_slope_sim1 <- slope_from_stand_x_slope(precis(post_sim1)[2,1], simdata1$Height)
post_interc_sim1 <- intercept_from_stand_x_slope(precis(post_sim1)[1,1],
                                                 simdata1$Height,
                                                 post_slope_sim1)
post_slope_sim2 <- slope_from_stand_x_slope(precis(post_sim2)[2,1], simdata2$Height)
post_interc_sim2 <- intercept_from_stand_x_slope(precis(post_sim2)[1,1],
                                                 simdata2$Height,
                                                 post_slope_sim2)
post_slope_sim3 <- slope_from_stand_x_slope(precis(post_sim3)[2,1], simdata3$Height)
post_interc_sim3 <- intercept_from_stand_x_slope(precis(post_sim3)[1,1],
                                                 simdata3$Height,
                                                 post_slope_sim3)
# Now actually plot these lines
abline(post_interc_sim1, post_slope_sim1
       , lwd = 3
       , col = alpha(sim_sets_colors[1], 0.5)
       )
abline(post_interc_sim2, post_slope_sim2
       , lwd = 3
       , col = alpha(sim_sets_colors[2], 0.5)
       )
abline(post_interc_sim3, post_slope_sim3
       , lwd = 3
       , col = alpha(sim_sets_colors[3], 0.5)
       )

####### Plot the original hypothesis the simulated data are based on ----------
intercept_at_zero <- intercept_w_h - mean_height * slope_b
abline(intercept_at_zero, slope_b
       , lwd = 3
       , col = alpha("black", 0.8)
       , lty = 2
)

legend("bottomright"
       , legend = c("Assumed pars", "Sim data 1", "Sim data 2", "Sim data 3", "Prior")
       , col = c("black", sim_sets_colors[1], sim_sets_colors[2], sim_sets_colors[3], alpha("black", 0.1))
       , pch = c(NA, 19, 19, 19, NA)
       #, bty="n"
       , cex = 0.75
       , lty = c(2, 1, 1, 1, 1)
       , lwd = c(3, 3, 3, 3, 1)
)
###### WHAT DO WE SEE HERE -------------
# First, the colored lines seem to go through the middle of the correspondingly
# colored point clouds: that means the Bayesian estimation of slope and intercept,
# and our code, and conversion back to real units, all seem to work reasonably
# well to fit a line to points.
# Second, the prior covers a large range of slopes and particularly intercepts,
# and this does not seem to have harmed the regression fits. If you want, you could try
# running the entire code again but with changed values for the prior, to see whether 
# and how much this impacts the outcome.
# Third, the posterior lines are not too far from the 'true' value, even for the 
# case with a quite small sample size; and at a sample size of n=100, the fit is
# basically indistinguishable from the 'true' slope & intercept. 

# This is what we wanted: to be convinced that our workflow could recover the 
# actual relationship, which for the simulated data we made up. You could also 
# rerun this entire code using different values for the 'hypothesis simulated'
# to see what other true relationships can be recovered.


###### Plot shaded confidence areas ----------
# There are two types of 'confidence intervals' or as he says, two types
# of uncertainty.
# I'll plot this here just for the Sim data set 2. I'm going to add elements
# in the 'reverse' order, which will make them easier to see.
par(mfrow=c(1,1))
plot(NULL
     , xlim = c(h_min, h_max)
     , ylim = c(w_min, w_max)
     , xlab = "Height [cm]"
     , ylab = "Weight [kg]"
)

n_plot <- 30
# The first is the uncertainty about where the actual linear relationship is.
samples_of_post <- extract.samples(post_sim2, n=n_plot)
a <- samples_of_post$a
b <- samples_of_post$b
post_slopes <- slope_from_stand_x_slope(b, simdata2$Height)
post_intercepts <- intercept_from_stand_x_slope(a,
                                                 simdata2$Height,
                                                 post_slopes)
# These are now a bunch of lines, illustrating the range of possible slopes and
# intercepts. I'll plot them in a minute.

# The second type of uncertainty is the prediction uncertainty, i.e. the
# additional spread that real data points will have around even the 'true'
# relationship. 
# To illustrate the full range of how data may spread around the linear 
# relationship, we simulate the posterior across the entire range of x (height).
# This is actually what we did above too: after all, we just plotted the line 
# across the entire range of x, regardless of whether we expect points there.
height_seq <- seq(h_min*0.9, h_max*1.1, len = n_plot)
height_seq_as_z <- (height_seq - mean(simdata2$Height)) / sd(simdata2$Height)
post_sim <- sim(post_sim2, data = list(H = height_seq_as_z))
# Remember 'PI' stands for percentile interval; here we find the 90 percent
# interval (i.e. 5th to 95th percentile; arbitrary as there is no convention, 
# nonetheless perhaps the interval that makes most intuitive sense).
weight.PI <- apply(post_sim, 2, PI, prob = 0.90)
shade(weight.PI, height_seq
      , col = alpha(sim_sets_colors[2], 0.3)
)

# Now add to the plot the first type of uncertainty, in slope & intercept:
for (i in 1:n_plot)
abline(post_intercepts[i], post_slopes[i]
       , lwd = 3
       , col = alpha(sim_sets_colors[2], 0.4)
)
# Now we'll add the (simulated) data points again as well:
points(Weight ~ Height
       , data = simdata2
       , bg = sim_sets_colors[2]
       , pch = 21
       , col = "black"
)
# And this is the 'correct' answer
abline(intercept_at_zero, slope_b
       , lwd = 3
       , col = alpha("black", 0.8)
       , lty = 2
)


#### Run model on real data -------------------------
# The whole point is that we use the exact same analysis function as on the simulated
# data:
post_real <- W_H_model(realdata_z)
precis(post_real)
# This is the result!!

#### Illustrate and interpret results from analysis of empirical data -------------------
## We'll use essentially the same code as we did to illustrate the analysis 
## of sim data 2 above.
par(mfrow=c(1,1))
plot(NULL
     , xlim = c(h_min, h_max)
     , ylim = c(w_min, w_max)
     , xlab = "Height [cm]"
     , ylab = "Weight [kg]"
)
n_plot <- 50
samples_of_post <- extract.samples(post_real, n=n_plot)
a <- samples_of_post$a
b <- samples_of_post$b
post_slopes <- slope_from_stand_x_slope(b, realdata$Height)
post_intercepts <- intercept_from_stand_x_slope(a,
                                                realdata$Height,
                                                post_slopes)
height_seq <- seq(h_min*0.9, h_max*1.1, len = n_plot)
height_seq_as_z <- (height_seq - mean(realdata$Height)) / sd(realdata$Height)
post_sim <- sim(post_real, data = list(H = height_seq_as_z))
weight.PI <- apply(post_sim, 2, PI, prob = 0.90)
shade(weight.PI, height_seq
      , col = alpha("slateblue", 0.2)
    )
for (i in 1:n_plot)
  abline(post_intercepts[i], post_slopes[i]
         , lwd = 3
         , col = alpha("slateblue", 0.3)
  )
points(Weight ~ Height
       , data = realdata
       , bg = "slateblue4"
       , pch = 21
       , col = "black"
)
## INTERPRET -----------------
# This graph contains the actual data points, a set of lines illustrating a
# sample of the posterior for the linear relationship between weight and height,
# and the shaded region illustrates a 90% range of predicted data from the
# posterior (or 'simulated data from posterior').
# Note that the range depicted by the graph is affected by what we defined as 
# the hypothesis above, to allow the simulated data to fit into the same frame
# - if you want you could adjust the axes ranges here according to the real data.
##############################################################################

##############################################################################

### FITTING CURVES -----------------------------------------------
#### Chapter 4 -----------------------------------
#First, going along with the book & videos about how curve-fitting works

# Actual dataset - plotting just to check
plot(d$weight~d$height)
xbar <- mean(d$height)
ybar <- mean(d$weight)
# Standardize data
H_stand <- zscore(d$height)
Hsq_stand <- H_stand*H_stand
# Priors
intercept_prior_mean <- 50
intercept_prior_SD <- 50
linearslope_prior_mean <- 0
linearslope_prior_sd <- 1
sigma_prior_max <- 20
# Other elements of the prior are the parameters for the quadratic slope
# and of course the functional form/distribution of these, e.g. lognormal 
# or uniform; these are specified in the model

# Simulate some samples from priors for plot (i.e. parameter value sets)
n_plot <- 50
intercepts_at_mean <- rnorm(n_plot, intercept_prior_mean, intercept_prior_SD)
slopes <- rlnorm(n_plot, linearslope_prior_mean, linearslope_prior_sd)

##### Quadratic model -----------------
# Which is a terrible fit. 
# illustrate by showing several panels with different sample size?

# Optionally, define starting points for the estimation algorithm
start <- list(
  a = ybar,
  b1 = 0.5,
  b2 = 0,
  sigma = sd(d$weight)
)

# Actual model
# using log-normal distribution for linear factor to force positive slope
model2.0 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b1*H_stand + b2*Hsq_stand,
    a ~ dnorm(intercept_prior_mean, intercept_prior_SD),
    b1 ~ dlnorm(linearslope_prior_mean, linearslope_prior_sd),
    b2 ~ dnorm(linearslope_prior_mean, linearslope_prior_sd),
    sigma ~ dunif(0, sigma_prior_max)
  )
  , data=d
  , start = start
)

precis(model2.0)

# Now we'll graph and interpret the result by sampling from the posterior
height_seq <- seq(min(H_stand)*1.1, max(H_stand)*1.1, len=n_plot)
# The n_plot just determines the resolution here - doesn't need to be the same
# value as what we used for number of lines above. I am multiplying the max and
# min only to give the plot a bit more space on either side
# We defined the square term as a separate value, so need to generate this
sim_input_xvalues <- list(H_stand=height_seq, Hsq_stand=height_seq^2)
sim_weight <- sim(model2.0, data = sim_input_xvalues, n=100)
# Note the above generates predictions from the posterior using sim()
# Remember 'PI' stands for percentile interval
weight.PI <- apply(sim_weight, 2, PI, prob=0.95)
# Instead of predictions, we can extract posteriors for mu:
mu <- link(model2.0, data=sim_input_xvalues, n=1000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.95)

# McElreath's graph, fig. 4.11 in book:
plot(weight ~ H_stand, data=d, col=col.alpha(rangi2,1) 
     , xaxt = "n" # We'll plot original units on later
     , xlab = "Height [cm]"
     , ylab = "Weight [kg]"
     , xlim = c(min(H_stand), max(H_stand))
     )
lines(height_seq, mu.mean, lwd = 3)
shade(mu.PI , height_seq ) # Describing posterior for mu
shade(weight.PI , height_seq ) # Describing posterior for overall data

# Plotting the correct x-axis units
at <- c(-3,-2,-1,0,1)
labels <- round(at*sd(d$height) + mean(d$height))
axis(side=1, at=at, labels=round(labels,1))

# Note my graph above had also used this more explicit way of sampling
# from the posterior:
samples_of_posterior <- extract.samples(model2.0, n=n_plot)

#### VIDEO 4 --------------------

## McElreath says DO NOT USE polynomials. Why? Because they are 
## 'global' smoothers, assuming various symmetries across the whole 
## x-range, and any datapoint anywhere can change the curve arbitrarily 
## far away from itself.
## After all, we are talking about not-mechanistically motivated
## polynomials purely for the purpose of fitting a non-linear pattern.
## It is always better to use a functional form derived from mechanistic
## understanding (e.g. if you expect something to be logarithmic or something else).

## If you don't have that, use splines. Splines simply create localized
## fits and then smoothe the transitions. 

#### Still chapter 4 ---------------------------------
##### SPLINES --------------------------------
# This is essentially the code straight from the book
data(cherry_blossoms)
OurData <- cherry_blossoms
precis(OurData)
plot(temp~year
     , data = cherry_blossoms
     , col = col.alpha(rangi2,0.5)
     )

# Divide x into num_knots regularly spaced quantiles to 
# place 'knots' for spline
d2 <- OurData[ complete.cases(OurData$temp) , ] # complete cases on temp
num_knots <- 5
degree <- 2
knot_list <- quantile(d2$year , probs=seq(0,1,length.out=num_knots))

# We need this 
library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)],
        degree=degree, intercept=TRUE)
plot(NULL 
     , xlim=range(d2$year) 
     , ylim=c(0,1) 
     , xlab="year" 
     , ylab="basis value"
     )
for (i in 1:ncol(B)) 
  lines(d2$year, B[,i])
model_splines <- quap(
  alist(
    T ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    # This is matrix multiplication, which is the same as
    # mu <- a + sapply(1:1124, function(i) sum(B[i,]*w)),
    a ~ dnorm(6,10),
    w ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data=list(T=d2$temp, B=B),
  start=list(w=rep(0, ncol(B))) 
  )
precis(model_splines, depth=2)

post <- extract.samples(model_splines)
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-2,2) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

mu <- link(model_splines)
mu_PI <- apply(mu,2,PI,0.97)
plot(d2$year , d2$temp , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )

# End of chapter 4 (not video 4) ------------------------

#### SPLINES WORKED EXAMPLE ------------
# OK now let's do proper workflow with Howell data.

## MOVED THIS TO A SEPARATE SCRIPT '4b'
