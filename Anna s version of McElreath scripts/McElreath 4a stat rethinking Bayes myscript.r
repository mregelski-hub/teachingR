## CODE TO GO WITH BOOKCLUB
## ON Richard McElreath's 'Statistical Rethinking'
# Anna Dornhaus, 2026

########### PART IV: Workflow ##########
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

## EXAMPLE 1: fitting a model of weight ~ height ---------------------------------------------------
#### Step 1: Define generative model -------------------------
# We assume that weight W is proportional to height H, but with a normally 
# distributed error U. This means in general we have parameters slope (b), intercept (i),
# and standard deviation of U (sd).
# Here we subtract the mean of the heights vector, which is not yet standardized, in 
# order for the 'intercept' to be the intercept at the mean (of height), not the 
# normal intercept at height=0. This is because we already know our model will not do 
# well near height=0, and also whatever the value is there is not that informative. 
# If we use the intercept at mean, we know that it should reflect the weight of a person of 
# medium height. 
sim_weight <- function(vector_of_heights, slope, intercept, error_sd) {
  samplesize <- length(vector_of_heights)
  error_around_line <- rnorm(samplesize, 0, error_sd)
  responsevariable <- slope * (vector_of_heights - mean(vector_of_heights)) + intercept + error_around_line
  return(responsevariable)
}

##### Define hypothesis simulated ------------------
# The parameters for the generative model are essentially the hypothesis; they are
# what we want to get out.
# Note that in this version, we have not yet standardized anything (weights or heights),
# so these parameters really reflect the actual slopes etc. in original units (here
# that is cm for height and kg for weight).
slope_b <- 0.7 # b
intercept_at_mean <- 50 # i
error_term <- 5 # sigma

#### Step 2: Generating the actual simulated data ---------------------
samplesize <- 100 
# Note that for this to be a proper test of how well our analysis is going to work,
# this should be close to the sample size we actually have or anticipate. 

# When we are generating (fake) data, we first need to define the set of x-values
# we want to use. Here, that's heights, and we'll pick them from a normal distribution.
mean_height <- 170
sd_height <- 10
heights <- rnorm(samplesize, mean = mean_height, sd = sd_height)

# If we wanted a regularly spaced, uniform set of x values, this is what we 
# could use:
#tempmin <- 0
#tempmax <- 35
#temps <- seq(tempmin, tempmax, (tempmax-tempmin)/(samplesize-1))

# Now, we're on the way to generating a full fake data table, by simulating 
# weights that correspond to our set of simulated heights:
weights <- sim_weight(heights, slope_b, intercept_at_mean, error_term) 
simdata <- data.frame(heights, weights)
colnames(simdata) <- c("Height", "Weight")

##### Prep real data ---------------------------------------
# The real data we will use is d2.
realdata <- d2
# We want it to have the exact same format as the simulated data (or vice versa):
colnames(realdata) <- c("Height", "Weight")

#### Step 3: Standardize data (both real and simulated) ----------------------------
# Standardization done prior to model is better he says. This is the general formula
# for the 'z-score': everything is centered on its mean, and relative to the 
# standard deviation in the data. Thus, a z-score of 1 means one standard deviation 
# higher than the mean. 
zscore <- function(dat) {
  z <- (dat - mean(dat)) / sd(dat)
  return(z)
}

# In actual fact, we only standardize the x-axis - and if we have a multifactorial
# model, we want to standardize all the input factors. The response does not have
# to be standardized, since its distribution just emerges from what we assume the
# generative model is. 
simdata_z <- data.frame(zscore(simdata$Height), simdata$Weight)
realdata_z <- data.frame(zscore(realdata$Height), realdata$Weight)
colnames(simdata_z) <- c("Height", "Weight")
colnames(realdata_z) <- c("Height", "Weight")

# Let's make sure this worked correctly. We'll plot the untransformed next to the 
# transformed data.
# Defining a nice color
sim_data_color <- "seagreen"
real_data_color <- "slateblue"
# Determining extent of axes; this is just making sure we capture all the points
# from either dataset. In a lot of cases, you can probably specify this manually.
h_max <- max(simdata$Height, realdata$Height)
w_max <- max(simdata$Weight, realdata$Weight)
h_min <- min(simdata$Height, realdata$Height)
w_min <- min(simdata$Weight, realdata$Weight)

# Two graphs next to each other
par(mfrow=c(1,2))
# Plotting the real data first
plot(realdata$Weight ~ realdata$Height
     , col = alpha(real_data_color, 0.3) #I'm assuming here there are a lot of these,
     # and so making them fairly transparent. You don't have to do this. 
     , pch = 19
     , xlim = c(h_min, h_max)
     , ylim = c(w_min, w_max)
     , xlab = "Height [cm]"
     , ylab = "Weight [kg]"
     #, main = "Data in real units"
)
# And now the simulated data next to it. 
# Remember there is no particular reason to think that the simulated data will 
# 'fit' the real data; they are after all just whatever parameters for the model
# we put in.
points(Weight ~ Height
       , data = simdata
       , bg = sim_data_color # Again somewhat arbitrary but I wanted the points
       # to have a black outline, and that is what this does. 
       , pch = 21
       , col = "black"
)
legend("bottomright"
       , legend = c("Real data", "Sim data")
       , col = c(real_data_color, sim_data_color)
       , pch = 19
       #, bty="n"
       #, cex = 0.75
)
mtext("Data in real units", 3, 2, cex = 1.25)
mtext(paste("Simulated data N=", samplesize), 3, 1)

# Now we'll plot the same 2 datasets but with a standardized x-axis.
plot(realdata_z$Weight ~ realdata_z$Height
     , col = alpha(real_data_color, 0.3)
     , pch = 19
     , xlim = c(-3, 3)
     , ylim = c(w_min, w_max)
     , xlab = "Z-Score Height [sd]"
     , ylab = "Weight [kg]"
)
points(Weight ~ Height
       , data = simdata_z
       , bg = sim_data_color
       , pch = 21
       , col = "black"
)
mtext("Standardized x", 3, 2, cex = 1.25)
# What's different between these two graphs?
# The overall orientation of points to each other, within each dataset (!),
# should be the same. However, the point cloud should have moved to be 
# centered on 0 in the x-dimensions (axis). 

#### Step 4: Define priors -------------------------------------
# Remember priors are really also probability distributions (just like
# likelihood and posterior are). 
# So we are not defining a single slope as the prior, but over all slopes,
# we are defining how likely we find each of them (prior to knowing about
# the current dataset).

# Here we are going to assume that our priors for the intercept and slope are normally
# distributed, and our prior for sigma is uniformly distributed and its minimum is 0.
# Given these assumptions about the shape of the prior, we just have to define
# these parameters:

# Intercept of model - remember we want to think about the intercept as the y value
# at the mean x value - and with our standardized x, this is automatic, since
# the mean x is in fact 0. Also, the intercept is in units of the y-axis, so if 
# the y-axis is not transformed, this is here units of weight (specifically kg).
intercept_prior_mean <- 50
intercept_prior_SD <- 10

# Slope of model - the slope is how much more weight per height. Now if the height
# is standardized, this is really how much more y (so weight) do we get for an 
# increase in x (so height) of 1 standard deviation. In the original data, 
sd(realdata$Height)
# So that means our prior says this is how much weight we think may be added for this 
# increase in height. The units of the slope are thus [kg/sd of height].
slope_prior_mean <- 10
slope_prior_sd <- 5
# Lastly, we define our prior for the error term. This 'max' here is really what is 
# the largest error term, in units of y (so kg) that we will allow for people of
# the same height. 
sigma_prior_max <- 20

# Here is the formal list of the assumptions in McElreath's notation:
# (You can think of it as happening in reverse order, i.e. from the bottom up)
list_of_assumptions <- alist(
  W ~ dnorm(mu, sigma), # This is adding the error term to the model
  mu <- a + b*H, # This is the core model formula
  a ~ dnorm(intercept_prior_mean, intercept_prior_SD), # This is picking an intercept based on the prior
  b ~ dnorm(slope_prior_mean, slope_prior_sd), # This is picking a slope based on the prior
  sigma ~ dunif(0, sigma_prior_max) # This is picking an error term based on the prior
)

# Let's plot some samples from this prior to make sure it's reasonable.
n_plot <- 100
# We pick some slopes from these priors just like the 'a' and 'b' lines in the model
# assumption list. 
intercepts_at_mean <- rnorm(n_plot, intercept_prior_mean, intercept_prior_SD)
slopes <- rnorm(n_plot, slope_prior_mean, slope_prior_sd)
# We also have a prior for the error but we're going to leave this out here.

# First re-plot the right graph from before: 
par(mfrow=c(1,1))
plot(realdata_z$Weight ~ realdata_z$Height
     , col = alpha(real_data_color, 0.3)
     , pch = 19
     , xlim = c(-3, 3)
     , ylim = c(w_min, w_max)
     , xlab = "Z-Score Height [sd]"
     , ylab = "Weight [kg]"
)
points(Weight ~ Height
       , data = simdata_z
       , bg = sim_data_color
       , pch = 21
       , col = "black"
)
mtext("Standardized x", 3, 2, cex = 1.25)

# Then add n_plot lines corresponding to the prior we picked. Remember the 'prior'
# is really the whole distribution of lines (or a probability distribution, i.e. an
# infinite number of lines).
for ( i in 1:n_plot ) 
  curve(intercepts_at_mean[i] + slopes[i]*x
        , from=-3, to=3
        , add=TRUE 
        , col=col.alpha("black",0.3) 
  )
mtext("Lines are samples from prior", 3, 1)

#### Step 5: Define Bayesian model procedure -------------------
# This is where the model fitting happens, i.e. here we use quap().

# It is helpful to define starting values, as the quap() optimization
# procedure can fail, especially with too few data points. 
# Hopefully what we do here has no influence on the results, just on whether
# the approximation procedure works or fails. 
start <- function(dat) {
  return(
    list(
      a = mean(dat$Weight)
      , b = slope_prior_mean
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

#### Step 6: Run model on simulated data ------------------------
# Ok we defined our model as a function above, now we run it on some data.

# First, we want to check it is working on the simulated data.
post_sim <- W_H_model(simdata_z)
precis(post_sim)

# We actually know what the 'correct' values for a, b, and sigma are - they are our 
# assumptions in the generative model of b (the slope), a (the intercept), and the
# error term (sigma).
slope_b # This is b
error_term # This is sigma
intercept_at_mean # This is a
# Oh! But the slope in slope_b is in units of [kg/cm], while the slope in the model
# is in units of [kg/sd], because we had standardized the x-axis. To convert out input
# parameter, we will want to multiply it with the standard deviation:
corrected_slope_value <- slope_b * sd(simdata$Height)
corrected_slope_value # This is now really the b that is the output of the model. 

# So, these parameters should be pretty close. If they are not, your model does 
# not have the power to find the parameters more accurately than you see here. 
# However, you can simulate with higher sample size, and this should get you
# closer and closer to the actual assumed parameters.

#### Step 7: Run model on real data -----------------------------
# Since we defined everything as functions, all we have to do is this:
post_real <- W_H_model(realdata_z)
precis(post_real)

# This now gives us our answer, namely the 'real' estimated parameter values -
# but actually better, because 'precis' is just a summary, what we actually have 
# is a probability distribution over all possible parameter values showing how
# likely we think each one is, based on prior and data. 

# Illustrating and interpreting the posterior is the next step.  --------------
# Here I'm just going to add this 'average' posterior line into the graph:
# First for simulated data - 
abline(precis(post_sim)[1,1], precis(post_sim)[2,1]
       , lwd = 3
       , col = sim_data_color
)
# Now for real data - 
abline(precis(post_real)[1,1], precis(post_real)[2,1]
       , lwd = 3
       , col = real_data_color
)
# Note that I am sure McElreath wouldn't like this as the point of having a 
# posterior is to understand the distribution and thus the certainty. But
# I am leaving the better illustration for the next example. 
# End example workflow without standardizing y-axis --------------

## EXAMPLE 2: same model, more versions of simulated data ---------------------------------------------
#### Generating simulated data -------------------------
# Everything is pretty much the same as above. 
# So I am assuming
# -sim_weight() 
# -the parameters for the hypothesis
# have already been defined in Step 1 above, and
# - zscore()
# has been defined also (above it's in Step 3). 

# I'm going to assume that to properly test our analysis workflow, we're going
# to simulate data with different sample sizes. Lets say
N_1 <- 3
N_2 <- 10
N_3 <- 100
# Now, we're on the way to generating three sets of fake data for our model:
heights1 <- rnorm(N_1, mean = mean_height, sd = sd_height)
heights2 <- rnorm(N_2, mean = mean_height, sd = sd_height)
heights3 <- rnorm(N_3, mean = mean_height, sd = sd_height)
# After this, we can simulate corresponding weights
weights1 <- sim_weight(heights1, slope_b, intercept_at_mean, error_term) 
simdata1 <- data.frame(heights1, weights1)
colnames(simdata1) <- c("Height", "Weight")
weights2 <- sim_weight(heights2, slope_b, intercept_at_mean, error_term) 
simdata2 <- data.frame(heights2, weights2)
colnames(simdata2) <- c("Height", "Weight")
weights3 <- sim_weight(heights3, slope_b, intercept_at_mean, error_term) 
simdata3 <- data.frame(heights3, weights3)
colnames(simdata3) <- c("Height", "Weight")

# If you wanted to experiment with this, you could instead simulate three different
# parameter values, or data derived from different qualitative hypotheses (e.g.
# different sim_weights functions).

#### Standardize data (both real and simulated) ----------------------------
simdata1_z <- data.frame(zscore(simdata1$Height), simdata1$Weight)
simdata2_z <- data.frame(zscore(simdata2$Height), simdata2$Weight)
simdata3_z <- data.frame(zscore(simdata3$Height), simdata3$Weight)
realdata_z <- data.frame(zscore(realdata$Height), realdata$Weight)
colnames(simdata1_z) <- c("Height", "Weight")
colnames(simdata2_z) <- c("Height", "Weight")
colnames(simdata3_z) <- c("Height", "Weight")
colnames(realdata_z) <- c("Height", "Weight")

#### Run model on simulated data ------------------------
# Ok we defined our model as a function above, now we can run it on any data we want.
post_sim1 <- W_H_model(simdata1_z)
post_sim2 <- W_H_model(simdata2_z)
post_sim3 <- W_H_model(simdata3_z)

# Let's make a comparison table:
inputpars <- c(intercept_at_mean, slope_b, error_term)
c1 <- round(precis(post_sim1)[,1], 2)
c2 <- round(precis(post_sim2)[,1], 2)
c3 <- round(precis(post_sim3)[,1], 2)
# Remember though that our slope values have to be corrected, so 
c1[2] <- round(c1[2] / sd(simdata1$Height), 2)
c2[2] <- round(c2[2] / sd(simdata2$Height), 2)
c3[2] <- round(c3[2] / sd(simdata3$Height), 2)
outcomes <- data.frame(inputpars, c1, c2, c3)
colnames(outcomes) <- c("Inputvals"
                        , paste("Sim_N=", N_1, sep="")
                        , paste("Sim_N=", N_2, sep="")
                        , paste("Sim_N=", N_3, sep=""))
##### Results of simulated data analysis -----------------------
# Show it:
outcomes
# If what you did is simulate datasets with different sample size, the higher
# sample size ones should enable you to better estimate the original parameters.

# End example workflow with outcomes for multiple simulated datasets ------------

## ILLUSTRATE RESULTS ------------------------------------


##### Plot the results for simulated data to test working of the model ------------

# Defining a nice color set
sim_sets_colors <- viridis(3, alpha = 0.7, begin = 0.1, end = 0.9)
# Determining extent of axes
w_max <- max(simdata1$Weight, simdata2$Weight, simdata3$Weight, realdata$Weight)
w_min <- min(simdata1$Weight, simdata2$Weight, simdata3$Weight, realdata$Weight)
# Since height is standardized, we just plot from -3 to +3 - most data will be
# within 3 standard deviations of the mean. 
mtext(paste("3 simulated sets with N1=", N_1, ", N2=", N_2, ", N3=", N_3, sep = ""), 3, 1)

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


