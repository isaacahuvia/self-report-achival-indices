####################################
##  Self Report Archival Indices  ##
##       Analysis Functions       ##
####################################



####  Omitted Items  ####
omittedItems <- function(df, columns = NULL) {
  
  if(!is.null(columns)) df <- df[, columns]
  
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    out[i] <- sum(is.na(df[i,]))
    
  }
  
  return(out)

}



####  Longstring  ####
#Can calculate overall longstring, or for each response level
longstring <- function(df, columns = NULL, value = NULL) {
  
  if(!is.null(columns)) df <- df[, columns]
  
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    rle <- rle(df[i,])
    
    if(is.null(value)) {
      
      max <- max(rle$lengths)
      
    } else {
      
      if(value %in% df[i,]) {
        
        max <- max(rle$lengths[rle$values == value])
        
      } else {
        
        max <- 0
        
      }
      
    }
    
    out[i] <- max
    
  }
  
  return(out)
  
}



####  Psychometric Synonyms  ####
#See Psychometric Synonyms with Highly Correlated Data Demonstration.R for a demonstration of why this was not as useful in our analysis
psychSyn <- function(df, columns = NULL) {

  require(careless) #see https://cran.r-project.org/web/packages/careless/careless.pdf
  
  if(!is.null(columns)) df <- df[, columns]
  
  out <- c()
  
  out <- careless::psychsyn(df, critval = .6)
  
  return(out)
  
}



####  IRT: Polytomous Guttman Errors  ####
polyGuttmanErrors <- function(df, nCategories, columns = NULL, norm = F) {
  
  require(PerFit) #see https://cran.r-project.org/web/packages/PerFit/PerFit.pdf
  
  if(!is.null(columns)) df <- df[, columns]
  
  #Modify df if needed so that the lowest response level is coded as 0 (required for PerFit functions)
  if(min(df, na.rm = T) != 0) df <- df - min(df, na.rm = T)
  
  out <- c()
  
  if(norm == T) {
    
    out <- PerFit::Gnormed.poly(matrix = df, Ncat = nCategories)[["PFscores"]][[1]]
    
  } else {
    
    out <- PerFit::Gpoly(matrix = df, Ncat = nCategories)[["PFscores"]][[1]]
    
  }

  return(out)
  
}




####  IRT: U3 Person Fit Statistic  ####
#See https://www.jstatsoft.org/article/view/v074i05/v74i05.pdf for rationale for choosing the U3 statistic for use with polytomous data
u3 <- function(df, nCategories, columns = NULL) {
  
  require(PerFit) #see https://cran.r-project.org/web/packages/PerFit/PerFit.pdf
  
  if(!is.null(columns)) df <- df[, columns]
  
  #Modify df if needed so that the lowest response level is coded as 0 (required for PerFit functions)
  if(min(df, na.rm = T) != 0) df <- df - min(df, na.rm = T)
  
  out <- c()
  
  out <- PerFit::U3poly(matrix = df, Ncat = nCategories)[["PFscores"]][[1]]
  
  return(out)
  
}



####  Univariate Outlier Analysis  ####
zScore <- function(df, columns = NULL) {
  
  if(!is.null(columns)) df <- df[, columns]
  
  out <- c()
  
  rowMean <- rowMeans(df, na.rm = T)
  
  z <- (rowMean - mean(rowMean)) / sd(rowMean)
  
  return(z)
  
}



####  Mahalanobis Distance  ####
#only works with complete cases
mahalanobisDist <- function(df, columns = NULL) {
  
  if(!is.null(columns)) df <- df[, columns]
  
  #This procedure requires copmplete cases. Therefore we'll limit the dataset to complete cases, and return a vector which is NA when a case is not complete
  completeCasesIndex <- which(complete.cases(df))
  
  df.complete <- df[completeCasesIndex,]
  
  out.complete <- stats::mahalanobis(df.complete, colMeans(df.complete), cov(df.complete))
  
  out <- rep(NA, nrow(df))
  
  out[completeCasesIndex] <- out.complete
  
  return(out)
  
}



####  Inter-Item Standard Deviation  ####
interItemSD <- function(df, columns = NULL) {
  
  if(!is.null(columns)) df <- df[, columns]
  
  out <- c()
  
  for(i in 1:nrow(df)) out[i] <- sd(df[i,])
  
  return(out)
  
}



####  Person-Total Correlation  ####
personTotalCor <- function(df, columns = NULL) {
  
  if(!is.null(columns)) df <- df[, columns]
  
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    responseSet <- as.numeric(df[i,])
    personItemCorrelations <- sapply(df[-i,], mean, na.rm = T)
    out[i] <- cor(responseSet, personItemCorrelations)
    
  }
  
  return(out)
  
}