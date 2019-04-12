###################################
##     Child PRO Data Quality    ##
##  Use Functions & Output Data  ##
###################################

rm(list = ls())

library(dplyr)
library(careless)
library(PerFit)

## Load data
# PROMIS Family Relationships
FR <- read.csv("P:\\Systematic Literature Review\\Raw Data\\FR.csv", header = T, sep = ",", stringsAsFactors = F)
# PROMIS Life Satisfaction (same sample as Meaning and Purpose)
LS <- read.csv("P:\\Systematic Literature Review\\Raw Data\\LS.csv", header = T, sep = ",", stringsAsFactors = F)
# PROMIS Meaning and Purpose (same sample as Life Satisfaction)
MP <- read.csv("P:\\Systematic Literature Review\\Raw Data\\MP.csv", header = T, sep = ",", stringsAsFactors = F)
# PROMIS Physical Activity
PA <- read.csv("P:\\Systematic Literature Review\\Raw Data\\PhysAct.csv", header = T, sep = ",", stringsAsFactors = F)
# PROMIS Psychological Stress Experiences
PS <- read.csv("P:\\Systematic Literature Review\\Raw Data\\PS.csv", header = T, sep = ",", stringsAsFactors = F)

dfList <- c("FR", "LS", "MP", "PA", "PS")

for(dfName in dfList) {
  
  print(paste0(which(dfList == dfName), " of ", length(dfList)))
  
  df <- get(dfName)
  df.analysis <- df[,7:length(df)]
  df.out <- df

  ####  Omitted items  ####
  omittedItems <- function(x) {
    n <- sum(is.na(x))
    return(n)
  }
  df.out$omittedItems <- NA_real_
  for(i in 1:nrow(df)) {
    df.out$omittedItems[i] <- omittedItems(df.analysis[i,])
  }
  
  
  
  ####  Longstring  ####
  #Can calculate overall longstring, or for each response level
  longstring <- function(x, value = NULL) {
    rle <- rle(x)
    if(is.null(value)) {
      max <- max(rle$lengths)
    } else {
      if(value %in% x) {
        max <- max(rle$lengths[rle$values == value], na.rm = T)
      } else {
        max <- 0
      }
    }
    return(max)
  }
  df.out$longstring <- NA_real_
  for(i in 1:nrow(df)) {
    df.out$longstring[i] <- longstring(df.analysis[i,])
  }
  df.out$longstring_1 <- NA_real_
  df.out$longstring_2 <- NA_real_
  df.out$longstring_3 <- NA_real_
  df.out$longstring_4 <- NA_real_
  df.out$longstring_5 <- NA_real_
  for(i in 1:nrow(df)) {
    for(j in 1:5) {
      df.out[[paste0("longstring_", j)]][i] <- longstring(df.analysis[i,], value = j)
    }
  }

  
  
  ####  Psychometric Synonyms  ####
  df.out$psychSynCor <- careless::psychsyn(df.analysis, critval = .6)

  
  
  ####  IRT: Polytomous Guttman Errors  ####
  df.out$guttman <- PerFit::Gpoly(matrix = df.analysis - 1, Ncat = 5)[["PFscores"]][[1]]
  df.out$guttmanNormed <- PerFit::Gnormed.poly(matrix = df.analysis - 1, Ncat = 5)[["PFscores"]][[1]]

  
  
  ####  IRT: Person Fit Statistic  ####
  df.out$IRT <- PerFit::U3poly(matrix = df.analysis - 1,
                               Ncat = 5,
                               NA.method = "Pairwise",
                               IRT.PModel = "GRM",
                               Ability.PModel = "EAP")[["PFscores"]][[1]]
  
  
  
  ####  Univariate Outlier Analysis  ####
  rowMeans <- rowMeans(df.analysis, na.rm = T)
  df.out$zScore <- (rowMeans - mean(rowMeans)) / sd(rowMeans)

  
  
  ####  Mahalanobis Distance  ####
  #only works with complete cases
  df.out$mahalanobis <- NA
  df.out$mahalanobis[complete.cases(df.analysis)] <- stats::mahalanobis(df.analysis[complete.cases(df.analysis),],
                                                                        colMeans(df.analysis[complete.cases(df.analysis),]),
                                                                        cov(df.analysis[complete.cases(df.analysis),]))
  df.out$mahalanobisSquared <- df.out$mahalanobis^2
  
  
  
  ####  Inter-Item Standard Deviation  ####
  df.out$interItemSD <- NA_real_
  for(i in 1:nrow(df)) {
    df.out$interItemSD[i] <- sd(df.analysis[i,], na.rm = T)
  }
  
  
  
  ####  Person-Total Correlation  ####
  df.out$personTotalCorr <- NA_real_
  for(i in 1:nrow(df)) {
    responses <- as.numeric(df.analysis[i,])
    personItemCorrelations <- sapply(df.analysis[-i,], mean, na.rm = T)
    df.out$personTotalCorr[i] <- cor(responses, personItemCorrelations)
  }
  
  ## Output data
  assign(paste0(dfName, ".out"), df.out)
  write.csv(df.out, file = paste0("P:\\Systematic Literature Review\\Clean Data\\", dfName, ".csv"), row.names = F)

}