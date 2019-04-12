##############################
##  Child PRO Data Quality  ##
##    Analysis Functions    ##
##############################

## Establish sample df to test out functions on 
df <- data.frame(
  caseID = seq(1, 100),
  q1 = round(runif(100) * 4 + 1),
  q2 = round(runif(100) * 4 + 1),
  q3 = round(runif(100) * 4 + 1),
  q4 = round(runif(100) * 4 + 1),
  q5 = round(runif(100) * 4 + 1)
) %>%
  dplyr::mutate(
    q6 = round(q1 + rnorm(100, 0, 1)),
    q7 = round(q2 + rnorm(100, 0, 1))
  ) %>%
  rbind(
    c(101, NA,NA,4,2,NA,2),
    c(102, 1,1,1,1,1,2)
  )

df$q6 <- sapply(df$q6, min, 5)
df$q6 <- sapply(df$q6, max, 1)
df$q7 <- sapply(df$q7, min, 5)
df$q7 <- sapply(df$q7, max, 1)

df.complete <- df[complete.cases(df),]
  


####  Omitted Items  ####
omittedItems <- function(x) {
  n <- sum(is.na(x))
  return(n)
}

df$omittedItems <- NA_real_
for(i in 1:nrow(df)) {
  df$omittedItems[i] <- omittedItems(df[i,2:7])
}



####  Longstring  ####
#Can calculate overall longstring, or for each response level
longstring <- function(x, value = NULL) {
  rle <- rle(x)
  
  if(is.null(value)) {
    max <- max(rle$lengths)
  } else {
    if(value %in% x) {
      max <- max(rle$lengths[rle$values == value])
    } else {
      max <- NA
    }
  }
  
  return(max)
}

df$longstring <- NA_real_
for(i in 1:nrow(df)) {
  df$longstring[i] <- longstring(df[i,2:7])
}

df$longstring_1 <- NA_real_
for(i in 1:nrow(df)) {
  df$longstring_1[i] <- longstring(df[i,2:7], value = 1)
}



####  Psychometric Synonyms  ####
library(careless)
df$psc <- careless::psychsyn(df, critval = .6)



####  Psychometric Synonyms - Bootstrapped  ####
#It is not recommended that items be used more than once (Curran p. 11) - but we can try! To the best of my Google-ing this has not been done before.
#Weigh by correlation between two items?



####  Even-odd Consistency  ####
#Cannot do without subscales



####  Resampled Individual Reliability (Bootstrapped "Even-odd")  ####
#Cannot do without subscales



####  IRT: Polytomous Guttman Errors  ####
#See https://rdrr.io/cran/PerFit/man/Gpoly.html
library(PerFit)
dfPerFit <- df[-101,2:7] - 1
PerFit::Gpoly(matrix = dfPerFit,
              Ncat = 5)
PerFit::Gnormed.poly(matrix = dfPerFit,
                     Ncat = 5)


####  IRT: Person Fit Statistic  ####
#However, a tentative general conclusion is that simple group-based PFSs like Ht and U3 do not perform worse, 
#and in some cases perform better, than most model-based PFSs across different types of datasets. 
#See https://www.jstatsoft.org/article/view/v074i05/v74i05.pdf
PerFit::U3poly(matrix = dfPerFit,
               Ncat = 5,
               NA.method = "Pairwise",
               IRT.PModel = "GRM",
               Ability.PModel = "EAP")

####  Univariate Outlier Analysis  ####
#Case 102 is an outlier
df <- df %>%
  dplyr::mutate(score = rowMeans(df[,2:7], na.rm = T),
                z = (score - mean(score)) / sd(score))



####  Mahalanobis Distance  ####
#only works with complete cases
df.complete$mahalanobis <- stats::mahalanobis(df.complete[,2:7],
                                              colMeans(df.complete[,2:7]),
                                              cov(df.complete[,2:7]))
df.complete$mahalanobisSquared <- df.complete$mahalanobis^2



####  Inter-Item Standard Deviation  ####
df$interItemSD <- NA_real_
for(i in 1:nrow(df)) {
  df$interItemSD[i] <- sd(df[i,2:7])
}



####  Person-Total Correlation  ####
df$personTotalCorr <- NA_real_
for(i in 1:nrow(df)) {
  responses <- as.numeric(df[i,2:7])
  personItemCorrelations <- sapply(df[-i,2:7], mean, na.rm = T)
  df$personTotalCorr[i] <- cor(responses, personItemCorrelations)
}
