## CODE TO GO WITH BOOKCLUB
## ON Richard McElreath's 'Statistical Rethinking'
# Anna Dornhaus, 2025

########### PART IVb: Curves and splines ##########
## I recommend 'closing' all sections for better readability, and only opening the section
## you are working on: click on the small triangles by the line numbers to close/open
##
## LIBRARIES  -----------------------------
library(rethinking)
library(scales)
library(splines)
library(viridisLite)

##############################################################################
## Loading the data we'll use -----------------
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]
zscore <- function(dat) {
  z <- (dat - mean(dat)) / sd(dat)
  return(z)
}
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

## THis is the third part of chp 4 and video 4
## 2nd edition of the book, videos from 2023
## Using the Howell human growth data, this time with kids:
## fitting a curve rather than linear relationship

##### Setup ----------------------
# We need:
# the data
data(Howell1)
d <- Howell1
# the z score function
zscore <- function(dat) {
  z <- (dat - mean(dat)) / sd(dat)
  return(z)
}
# a color set
sim_sets_colors <- viridis(3, alpha = 0.7, begin = 0.1, end = 0.9)

##### Define generative model -----------------------
###### Setting up knots -------------------------
# We need to define the knots, as this determines the parameters in the model.
# The number of knots here is arbitrary: more knots means more parameters but
# better fit. 
num_knots <- 5
knot_list <- quantile(d$height, probs=seq(0,1,length.out=num_knots))
# In this particular case, perhaps a better approach would have been to choose
# knots based on biological knowledge, e.g. known age groups:
# If we pick the knots approximately where the age groups little kid, big kid, adult
# separate, perhaps we would use instead:
num_knots <- 4
knot_list <- c(0,80,140,180)
plot(d$height ~ d$age)
for (i in 0:num_knots) 
  abline(h = knot_list[i], col = "slateblue")
# (fewer knots but at sensible and interpretable inflection points).
degree <- 2
# Degree determines how many 'splines' are combined in each point.
####### Optionally, ------------------------ 
## you can use this to plot where your knots are and what height 
## ranges the 5 parameters we will fit will affect:
# This function is needed to set up the splines
test_basis_matrix <- bs(d$height,
                   knots=knot_list[-c(1,num_knots)], # This is just interior knots
                   degree=degree, intercept=TRUE)
plot(NULL 
     , xlim=range(d$height) 
     , ylim=c(0,1) 
     , xlab="Height" 
     , ylab="basis value"
)
for (i in 1:ncol(test_basis_matrix)) 
  points(d$height, test_basis_matrix[,i])
# Using 'lines()' as for the cherry blossoms doesn't work here because the data 
# aren't sorted. 
# Just for fun, here are the data points - we can see that the last two spline
# functions are likely to be associated with larger slopes than the first two.
points(d$weight/max(d$weight) ~ d$height
       , col = "slateblue"
       , pch = 1)
# However, we need to set up the actual basis_matrix with the H vector we actually
# want to use - see below.

###### Hypothesized parameters for simulated data --------------------------------
# Note that these are not priors. In fact you could put almost arbitrary values
# here - they only serve to generate a fake dataset and then to try to recover
# what we know to be the correct parameters with our analysis workflow. 
# Only after we've convinced ourselves that our workflow does that do we let it
# loose on the actual data (where we don't know the 'true' parameter values).
# Note that the following assumes you stuck with 4 knots.
sim_error_term <- 10
####### The following two vectors need to have the same length as -----------
ncol(test_basis_matrix)
## (!!!) 

sim_splineM <- c(1, 0.5, 2, 1, 1)
sim_splineI <- c(0, 0, 0, 0, 0) # In range of 0-50 doesn't make much of a difference
# #####Note that these are clearly not local slopes#####
# What, intuitively, are they?
###### Actual generative model --------------------------------
# Note that we won't standardize the data here since we essentially are 
# treating different sections of the data as separate sets with their own fit.

# The generative model should be able to take in our assumed parameter values
# and then simulate values for weight based on given values for height. 
# When using splines, these essentially reflect different slopes at different
# sections of the graph, which smoothly blend into each other.
spline_weight <- function(H, knots, splineM, splineI, error_term) {
  indiv_var <- rnorm(length(H), 0, error_term)
  bmatrix <- bs(H
                , knots=knots[-c(1,num_knots)] # This is just interior knots
                , degree=degree
                , intercept=TRUE
                )
  W <- (bmatrix %*% splineM)*H + (bmatrix %*% splineI) + indiv_var
  # This is matrix multiplication, which is the same as
  # W <- sapply(1:nrow(bmatrix), function(i) sum(bmatrix[i,]*splineI[i])),
  return(W)
}

sim_H_seq <- runif(544, min = min(d$height), max = max(d$height))
sim_W_points <- spline_weight(sim_H_seq, knot_list, sim_splineM, sim_splineI, sim_error_term)
plot(sim_W_points ~ sim_H_seq
     , col = alpha("seagreen", 0.5)
     , pch = 19)
sim_data <- data.frame(height=sim_H_seq, weight=sim_W_points)
####### Optionally again: what are the parameters doing?  ----------------
# To test more what is happening here, I want to extract the bmatrix from the
# function above. 
colorscale <- viridis(5)
B_sim <- bs(sim_H_seq
  , knots=knot_list[-c(1,num_knots)] # This is just interior knots
  , degree=degree
  , intercept=TRUE
)
for (i in 1:ncol(B_sim)) {
  points(sim_H_seq, max(sim_W_points)*rowSums(B_sim)
         , col = "purple"
         , pch = 19
         , cex = 0.1
         )
  points(sim_H_seq, max(sim_W_points)*B_sim[,i] # Just multiplying to make it
         # visible on this plot
         , pch = 19
         , cex = 0.1
         )
  points(sim_H_seq, sim_H_seq*B_sim[,i]*sim_splineM[i]
       , pch = 19
       , col = alpha(colorscale[i], 0.5)
       )
  points(sim_H_seq, B_sim[,i]*sim_splineI[i]
         , pch = 1
         , col = alpha(colorscale[i], 0.5)
         , cex = 0.5
  )
  mtext(paste("SplineM ", i, ": ", sim_splineM[i], sep = ""), side = 3, line = 1, cex = 0.5, at = 40+25*i)
  mtext(paste("SplineI ", i, ": ", sim_splineI[i], sep = ""), side = 3, line = 0, cex = 0.5, at = 40+25*i)
}

##### Define Priors and Bayesian model --------------------------

# To know which parameters will be fit and that we need priors for, the priors
# and the model kind of need to be built together. Both should be informed by 
# the generative model we already defined. 
sM_prior_mean <- 1
sM_prior_SD <- 2
sI_prior_mean <- 0
sI_prior_SD <- 2
sigma_prior_max <- 20

# Here is the formal list of the assumptions:
list_of_assumptions <- function(B) {
  alist(
  W ~ dnorm(mu, sigma),
  mu <- (B %*% sM) * H + B %*% sI,
  sM ~ dnorm(sM_prior_mean, sM_prior_SD),
  sI ~ dnorm(sI_prior_mean, sI_prior_SD),
  sigma ~ dunif(0, sigma_prior_max)
  )
}

W_H_spline_model <- function(dat, B) {
  quap(
    list_of_assumptions(B)
    , data = list(W=dat$weight, H=dat$height, B=B)
    , start = list(sM=rep(1, ncol(B)), sI=rep(1, ncol(B)), sigma=sigma_prior_max/2)
  )
}

##### Run model on several simulated data and make sure it recovers parameters ---------------
post_sim <- W_H_spline_model(sim_data, B_sim)
precis(post_sim, depth=2)
# Looks good!

##### Run model on real data -------------------------
B_real = bs(d$height
            , knots=knot_list[-c(1,num_knots)]
            , degree=degree, intercept=TRUE)

post_real <- W_H_spline_model(d, B_real)
precis(post_real, depth = 2)

##### Illustrate & interpret results, e.g. parameters, prediction intervals --------------

## For real data
# We want to plot
# frame
# real data
# posterior prediction data
# spline contributions sM just for mean
# spline contributions sI just for mean
# knot points vertical lines
# curve of fitted function (sum of sM and sI contributions)

## For sim data in addition
# the actual curve without sigma error

# Can we also plot the prior interval or distribution?
# Sample individual points from prior? Plot a bunch of curves without sigma?

## Actual plot setup is best in reverse order, to put important info on top

# frame ----------------
par(mfrow=c(1,1), mar=c(4,4,1,1), oma=c(0,0,0,0)) #b,l,t,r
y_max <- max(sim_data$weight)
plot(NULL
     , xlab = "Height [cm]"
     , ylab = "Weight [kg]"
     , ylim = c(0, y_max)
     , xlim = c(min(knot_list), max(knot_list))
     , col = alpha("grey", 0.1) # we'll plot these again later, this is just a guide
     )
# knot points vertical lines ----------------
for (i in 1:num_knots) 
  abline(v = knot_list[i])
# Plot prior ----------------------
# First plot a bunch of curves from prior without sigma
n_plot <- 100
prior_H_seq <- seq(from = min(knot_list), to = max(knot_list), length.out = 500)
B_priorH = bs(prior_H_seq
            , knots=knot_list[-c(1,num_knots)]
            , degree=degree, intercept=TRUE
            )
for (i in 1:n_plot) {
  prior_sM_sample <- rnorm(num_knots+1, sM_prior_mean, sM_prior_SD)
  prior_sI_sample <- rnorm(num_knots+1, sI_prior_mean, sI_prior_SD)
  prior_W_seq <- (B_priorH %*% prior_sM_sample) * prior_H_seq + (B_priorH %*% prior_sI_sample)
  lines(prior_H_seq, prior_W_seq
        , lty=2
        , lwd=2
        , col = alpha("violetred2", 0.3)
        )
  # => Priors start at 0,0 but otherwise are all over the place!
  
  # Then sample individual points from each curve plotted
  for (j in 1:nrow(prior_W_seq)) {
    prior_W_sample <- rnorm(1, prior_W_seq[j], runif(1, 0, sigma_prior_max))
    # This is using the curve from prior_W_seq as mu, and is picking a sigma from
    # the uniform prior for sigma. It's choosing 10 points for each prior_W_seq.
    points(prior_H_seq[j], prior_W_sample
           , pch = 19
           , col = alpha("violetred3", 0.03)
           )
    }
}

prior_mean_W_seq <- (B_priorH %*% rep(sM_prior_mean, num_knots+1)) * prior_H_seq + (B_priorH %*% rep(sI_prior_mean, num_knots+1))
# In addition, I'll plot just points around the average prior for sM and sI
# and with average sigma
n_plot <- 10
for (j in 1:nrow(prior_mean_W_seq)) {
  prior_W_sample <- rnorm(n_plot, prior_mean_W_seq[j], sigma_prior_max/2)
  points(rep(prior_H_seq[j], n_plot), prior_W_sample
       , pch = 19
       , col = alpha("violetred3", 0.1)
       )
}

# Plot prior mean line
lines(prior_H_seq, prior_mean_W_seq
      , lty=2
      , lwd=3
      , col = alpha("violetred2", 1)
)

# Spline contributions sM and sI just for mean ----------------
for (i in 1:ncol(B_priorH)) {
  lines(prior_H_seq, prior_H_seq*B_priorH[,i]*sM_prior_mean
        , lty = 3
        , lwd = 4
        , col = colorscale[i]
  )
  lines(prior_H_seq, B_priorH[,i]*sI_prior_mean
        , lty = 4
        , lwd = 4
        , col = colorscale[i]
  )
}

# Posterior for simulation data ----------------------------
n_plot <- 500
# For sim
post_samples_S <- extract.samples(post_sim, n_plot)
post_H_seq <- prior_H_seq
post_B <- B_priorH
for (i in 1:nrow(post_samples_S$sM)) {
post_sampleWs_S <- (post_B %*% post_samples_S$sM[i,]) * post_H_seq + 
  post_B %*% post_samples_S$sI[i,]
post_sampleWs_S_plus_error <- post_sampleWs_S + rnorm(length(post_H_seq), 0, post_samples_S$sigma)
# Posterior prediction data 
points(post_H_seq, post_sampleWs_S_plus_error
       , pch = 19
       , col = alpha("purple3", 0.008)
       )
# Posterior function lines
lines(post_H_seq, post_sampleWs_S
      , lwd = 4
      , col = alpha("purple4", 0.03)
      )
}

# Data for simulation --------------------
points(sim_data$height, sim_data$weight
#       , data = sim_data
       , pch = 1
       , col = alpha("darkblue", 0.8))
# and for sims also: the actual curve without sigma error
sim_W_no_noise <- spline_weight(post_H_seq, knot_list, sim_splineM, sim_splineI, 0)
lines(sim_W_no_noise ~ post_H_seq
      , col = "black"
      , lwd = 2
      , lty = 1
      )

# Posterior and data for real dataset -----------------------------
post_samples <- extract.samples(post_real, n_plot)
for (i in 1:nrow(post_samples$sM)) {
  post_sampleWs <- (post_B %*% post_samples$sM[i,]) * post_H_seq + 
    post_B %*% post_samples$sI[i,]
  post_sampleWs_plus_error <- post_sampleWs + rnorm(length(post_H_seq), 0, post_samples$sigma)
  # Posterior prediction data 
  points(post_H_seq, post_sampleWs_plus_error
         , pch = 19
         , col = alpha("purple3", 0.008)
  )
  # Posterior function lines
  lines(post_H_seq, post_sampleWs
        , lwd = 4
        , col = alpha("purple4", 0.03)
  )
}
points(d$height, d$weight
       , pch = 1
       , col = alpha("darkblue", 0.8))
#
