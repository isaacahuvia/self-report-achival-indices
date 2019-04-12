############################################
##         Child PRO Data Quality         ##
##  Psychometric Synonym Demonstration    ##
############################################

library(careless)

## How psychometric synonym statistics should look - with simulated data
careless <- careless::careless_dataset
careless$psc <- careless::psychsyn(df, critval = .6)
hist(careless$psc) #bimodial distribution with high-quality responses centered around r = .6

## How it works in our data
FR <- read.csv("P:\\Systematic Literature Review\\Raw Data\\FR.csv", header = T, sep = ",", stringsAsFactors = F)
FR$psc <- careless::psychsyn(FR[,7:length(FR)], critval = .6)
hist(FR$psc) #appears to be normally distributed around r = 0 with a positive skew

## This is because our data is highly correlated!
quantile(unique(cor(FR[,7:(length(FR)-1)], use = "pairwise.complete.obs")), probs = seq(0, 1, .2))

## Let's try this out with some simulated data. We'll show how the distribution of psychometric synonym scores looks with different levels of colinearity
complement <- function(y, rho, x) {
  if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}

simulated <- data.frame(seedVariable = round(seq(1, 5, by = .1)))

createSimulatedData <- function(dfSeed, min, max) {
  
  df <- dfSeed
  
  for(i in seq(min, max, length.out = 100)) {
    
    df[[paste0("v", i)]] <- complement(df$seedVariable, i)
    
  }
  
  print(quantile(unique(cor(df)), probs = seq(0, 1, .2)))
  
  df <- df[,-1]
  
  psc <- careless::psychsyn(df, critval = .6)
  
  return(psc)
  
}

hist(createSimulatedData(simulated, -.8, .8), xlim = c(-1, 1)) #a very wide range of correlations between variables
hist(createSimulatedData(simulated, 0, .8), xlim = c(-1, 1))
hist(createSimulatedData(simulated, .7, .8), xlim = c(-1, 1))
hist(createSimulatedData(simulated, .8, .95), xlim = c(-1, 1))
hist(createSimulatedData(simulated, .95, .99), xlim = c(-1, 1)) #all variables are very highly correlated