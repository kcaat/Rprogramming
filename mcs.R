### week 9 MCS ###

library(SimDesign)

### in-class

# Rewrite the below script using SimDesign

# fit models accounting for confounder
# Z is confounder, focal relationship is X->Y
# Z is smoking, X is coffee, Y is lung cancer

## set simulation parameters
n <- 100 # sample size
pz <- 0.2 # probability of Z = 1
alpha0 <- 0 # logit probability of x = 1 in non-smokers (z = 0)
#probability = exp(0)/(1+exp(0)) = 50%
alpha1 <- 1 # log odds ratio of x = 1 in smokers (z = 1) vs non-smokers
beta0 <- -3 # logit probability of y = 1 in non-coffee drinkers (x = 0) and non-smoke (z=0)
#probability = exp(-3)/(1+exp(-3)) = 5%
beta1 <- 0
beta2 <- 2
simnum <- 1000 # number of iterations to run in the simulation
#empty vector

# empty vectors to store the values
unadj.p <- adj.p <- rep(NA, simnum)

for(s in 1:simnum){
  ## generate confounder Z from a binomial distribution
  z <- rbinom(n, size = 1, prob = pz)
  #a vector of 0s and 1s
  #table(z) shows approx 20% z=1
  
  ## compute probability of observing X = 1 from the inverse logit function
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  #generates a vector of 1000 items, but there are only 2 possible values (all items are either     .5 or .73)
  #1) probability for x=1 and x=0?
  
  ## randomly generate binary variable X from the above probability
  x <- rbinom(n, size = 1, prob = px)
  #uses px to generate x
  #can also use options other than rbinom
  #x is a vector of 1s and 0s
  
  ## repeat above to randomly generate binary variable Y
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  #py is a vector of 0.047s and 0.26s
  y <- rbinom(n, size = 1, prob = py)
  #y is a vector of 0s and 1s
  
  ## combine three random variables into a data frame
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  #data frame has either 0/1 for lung, coffee, and smoke
  
  ## fit unadjusted logistic regression model
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.coef <- summary(unadj.mod)$coef
  ## fit adjusted logistic regression model
  #adds smoke variable
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.coef <- summary(adj.mod)$coef
  ## save p-values for coffee from both models in a vector
  unadj.p[s] <- unadj.coef[2,4]
  adj.p[s] <- adj.coef[2,4]
  ## save other values
  #unadj.est[s] <- unadj.coef[2,1]
  #unadj.se[s] <- unadj.coef[2,2]
  #etc.
  ## show simulation progress
  print(s)
}

## calculate the type 1 error rate from each model
# type 1 error should be 0.05 for true relationship??????????
#assigns 1 if <0.05, 0 if not
#computes mean of vector of 0/1 for all 1000 sims
mean(ifelse(unadj.p < 0.05, 1, 0)) #0.037
#p-value for beta0 should be larger, bc true relationship is null
mean(ifelse(adj.p < 0.05, 1, 0)) #0.011
#adj.p is a vector of 1000 ps from each simulation
#null H0 is beta1=0
#if alpha=0.05
#type 1 error = probability of rejected H0, given that H0 is true
#in our simulation, H0 is true
#so we should reject H0 approx 5% of the time, in 1000 trials

### adj vs unadj
#sd should be similar
#mean for unadj should be close to 0 (we do not expect coffee to increase lung cancer)

### instructions

#a. We will focus on comparing the empirical Type I ERROR rate of the effect of coffee drinking on lung cancer between the model adjusted for smoking and model unadjusted for smoking. In another words, out of 1,000 simulations, how many times do we REJECT the null hypothesis: there is no effect of coffee drinking on lung cancer, given that the true effect is null.

#b. Note that you can use the EDR() function in SimDesign package to compute the above probability in the Summarise step.

#c. Note also that we need to fit two models, unadjusted and adjusted. We can fit two models in one Analyze function or write two separate Analyze functions like in this example.

#d. We will investigate the type I error rate by varying the sample size, probability of smokers in the sample, probability of coffee drinkers in the sample, and effect of smoking on coffee drinking.
#i. 𝑛 = (50, 500)
#ii. 𝑝𝑧 = (0.2, 0.8)
#iii. 𝛼0 = (−1, 0, 1)
#iv. 𝛼1 = (0, 0.5, 1)

SimFunctions()

#Design <- createDesign(factor1 = NA, factor2 = NA)

#createDesign vs expand.grid?

Design <- createDesign(n = c(50, 500),
                       pz = c(0.2, 0.8),
                       alpha0 = c(-1, 0, 1),
                       alpha1 = c(0, 0.5, 1))

head (Design)

#-------------------------------------------------------------------

Generate <- function (condition, fixed_objects=NULL){
  #Attach (condition) # Attach() makes the variables in condition directly accesible
  
  alpha0 <- condition$alpha0
  alpha1 <- condition$alpha1
  pz <- condition$pz
  n <- condition$n
  
  beta0 <- -3
  beta1 <- 0
  beta2 <- 2
  
  z <- rbinom(n, size = 1, prob = pz)
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  x <- rbinom(n, size = 1, prob = px)
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  
  dat <- data.frame(lung = y, coffee = x, smoke = z)
}

#-----------------------------------------

#c. Note also that we need to fit two models, unadjusted and adjusted. We can fit two models in one Analyze function or write two separate Analyze functions like in this example.

Analyse <- function(condition, dat, fixed_objects = NULL) {
  #example
  #ret   <-   mean (dat)
  #ret
  
  ## fit unadjusted logistic regression model
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.coef <- summary(unadj.mod)$coef
  
  ## fit adjusted logistic regression model
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.coef <- summary(adj.mod)$coef
  
  ## save p values into vectors
  unadj.p <- unadj.coef[2,4]
  adj.p <- adj.coef[2,4]
  ret <- c(unadjp=unadj.p, adjp=adj.p)
  ret
  
}

#---------------------------------------------
#b. Note that you can use the EDR() function in SimDesign package to compute the above probability in the Summarise step.

Summarise <- function(condition, results, fixed_objects = NULL) {
  #we want type I error
  #EDR(p, alpha = 0.05, unname = FALSE)
  
  ret <- EDR(results, alpha=0.05)   # create a named vector
  ret
  
}

#-------------------------------------------------------------------
#e. First use a low number of iterations (5-10) to test the functions. When ready, run the full simulation with 1,000 iterations

res <- runSimulation(design=Design, replications=5, generate=Generate, 
                     analyse=Analyse, summarise=Summarise)
res

# run with 1000
res <- runSimulation(design=Design, replications=1000, generate=Generate, 
                     analyse=Analyse, summarise=Summarise)
res

### graph results

#graph: y-axis type I error, x-axis alpha1
#9 graphs: alpha0=-1, 0, 1 vs n=50, 100, 500

library(dplyr)
library(tidyr)

#make data long
reslong <- res %>%
  pivot_longer(cols = c("unadjp","adjp"),
               names_to="model",
               values_to="edr")

library(ggplot2)

ggplot(reslong, aes(x = alpha1, y = edr, color=model,
                    linetype = as.factor(pz), shape=as.factor(pz),
                    group = interaction(model, pz))) +
  geom_point(size = 2)+
  geom_path() + 
  facet_grid(n ~ alpha0) + #facet_grid(rows ~ columns)
  theme_bw()

# alpha0 is probability of coffee drinking, amongst non-smokers
# can add line at y=0.05 (true type I error rate)
# adj model follows true type I error in most cases, but better for large n
# unadj model for large n --> overestimates relationship between x and y
# esp when effect of z is large

#this demonstrate the importance of adjusting for confounders