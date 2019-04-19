####################################
##  Self Report Archival Indices  ##
##       Analysis Functions       ##
####################################

## See [paper citation] for an explanation of these methods

####  Resampled Individual Consistency  ####
#like even-odd, but instead of arbitrarily using even and odd numbered responses, use a series of random splits
#scaleLookup must be a data frame with two columns, the first being a unique identifier for scales and the second being a list of all variable names in each scale
resampledConsistency <- function(df, scaleLookup, iterations = 100, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #List unique scales
  scales <- unique(scaleLookup[[1]])
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    progress <- round(seq(nrow(df) / 20, nrow(df), length.out = 20))
    if(i %in% progress) print(paste0(round(100 * i / nrow(df)), "% complete"))
    
    cors <- c()
    
    for(n in 1:iterations) {

      a <- c()
      b <- c()
      
      for(k in 1:length(scales)) {
        
        values <- as.numeric(df[i, names(df) %in% scaleLookup[[2]][scaleLookup[[1]] %in% scales[k]]])
        index <- 1:length(values)
        
        index.a <- sample(index, floor(length(index)) / 2, replace = F)
        index.b <- index[!index %in% index.a]
        
        a[k] <- mean(values[index.a], na.rm = T)
        b[k] <- mean(values[index.b], na.rm = T)
        
      }
      
      cors[n] <- cor(a, b)
      
    }
    
    out[i] <- mean(cors)
    
  }
  
  return(out)
  
}



####  Even-Odd Consistency  ####
#scaleLookup must be a data frame with two columns, the first being a unique identifier for scales and the second being a list of all variable names in each scale
evenOdd <- function(df, scaleLookup, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #List unique scales
  scales <- unique(scaleLookup[[1]])
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    evens <- c()
    odds <- c()
    
    for(k in 1:length(scales)) {
      
      values <- df[i, names(df) %in% scaleLookup[[2]][scaleLookup[[1]] %in% scales[k]]]
      values <- as.numeric(values)
      
      evens[k] <- mean(values[seq(2, length(values), by = 2)], na.rm = T)
      odds[k] <- mean(values[seq(1, length(values), by = 2)], na.rm = T)
      
    }
    
    out[i] <- cor(evens, odds)
    
  }
  
  return(out)
  
}



####  Omitted Items  ####
omittedItems <- function(df, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    #For each row, the output is the number of NA values in that row
    out[i] <- sum(is.na(df[i,]))
    
  }
  
  return(out)

}



####  Longstring  ####
#Can calculate overall longstring, or for each response level
longstring <- function(df, columns = NULL, value = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    rle <- rle(df[i,])
    
    if(is.null(value)) {
      
      #If a specific value is not specified, the output is the maximum longstring of any value
      max <- max(rle$lengths)
      
    } else {
      
      if(value %in% df[i,]) {
        
        #If a specific value is specified and the response set includes that value, the output is the maximum longstring of that value
        max <- max(rle$lengths[rle$values == value], na.rm = T)
        
      } else {
        
        #If a specific value is specified but a response set does not include that value, the maximum longstring is 0
        max <- 0
        
      }
      
    }
    
    #For each row, the output is the maximum longstring for that row
    out[i] <- max
    
  }
  
  return(out)
  
}



####  Psychometric Synonyms  ####
#See Psychometric Synonyms with Highly Correlated Data Demonstration.R for a demonstration of why this was not as useful in our analysis
psychSyn <- function(df, critval = .6, columns = NULL) {

  require(careless) #see https://cran.r-project.org/web/packages/careless/careless.pdf
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  out <- careless::psychsyn(df, critval = critval)
  
  return(out)
  
}



####  IRT: Polytomous Guttman Errors  ####
polyGuttmanErrors <- function(df, nCategories, columns = NULL, norm = F) {
  
  require(PerFit) #see https://cran.r-project.org/web/packages/PerFit/PerFit.pdf
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Modify df if needed so that the lowest response level is coded as 0 (required for PerFit functions)
  if(min(df, na.rm = T) != 0) df <- df - min(df, na.rm = T)
  
  if(norm == T) {
    
    #This procedure requires copmplete cases. Therefore we'll limit the dataset to complete cases, and return a vector which is NA when a case is not complete
    completeCasesIndex <- which(complete.cases(df))
    
    #Limit the dataset to complete cases
    df.complete <- df[completeCasesIndex,]
    
    #Calculate the normed polytomous Guttman errors with those complete cases
    out.complete <- PerFit::Gnormed.poly(matrix = df.complete, Ncat = nCategories)[["PFscores"]][[1]]
    
    #Initialize the ouput vector as NA with the full length of nrow(df) so that we can include the complete case (non-NA) values in their right places
    out <- rep(NA, nrow(df))
    
    #Add normed error values back into this NA vector in their right places (so that cases with NA values will have an NA value here)
    out[completeCasesIndex] <- out.complete
    
  } else {
    
    out <- PerFit::Gpoly(matrix = df, Ncat = nCategories)[["PFscores"]][[1]]
    
  }

  return(out)
  
}



####  IRT: U3 Person Fit Statistic  ####
#See https://www.jstatsoft.org/article/view/v074i05/v74i05.pdf for rationale for choosing the U3 statistic for use with polytomous data
u3 <- function(df, nCategories, columns = NULL) {
  
  require(PerFit) #see https://cran.r-project.org/web/packages/PerFit/PerFit.pdf
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Modify df if needed so that the lowest response level is coded as 0 (required for PerFit functions)
  if(min(df, na.rm = T) != 0) df <- df - min(df, na.rm = T)
  
  #This procedure requires copmplete cases. Therefore we'll limit the dataset to complete cases, and return a vector which is NA when a case is not complete
  completeCasesIndex <- which(complete.cases(df))
  
  #Limit the dataset to complete cases
  df.complete <- df[completeCasesIndex,]
  
  #Calculate the normed polytomous Guttman errors with those complete cases
  out.complete <- PerFit::U3poly(matrix = df.complete, Ncat = nCategories)[["PFscores"]][[1]]
  
  #Initialize the ouput vector as NA with the full length of nrow(df) so that we can include the complete case (non-NA) values in their right places
  out <- rep(NA, nrow(df))
  
  #Add normed error values back into this NA vector in their right places (so that cases with NA values will have an NA value here)
  out[completeCasesIndex] <- out.complete
  
  return(out)
  
}



####  Univariate Outlier Analysis  ####
zScore <- function(df, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  rowMean <- rowMeans(df, na.rm = T)
  
  out <- (rowMean - mean(rowMean)) / sd(rowMean) #This is a z-score
  
  return(out)
  
}



####  Mahalanobis Distance  ####
#only works with complete cases
mahalanobisDist <- function(df, columns = NULL) {
  
  require(stats)
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #This procedure requires copmplete cases. Therefore we'll limit the dataset to complete cases, and return a vector which is NA when a case is not complete
  completeCasesIndex <- which(complete.cases(df))
  
  #Limit the dataset to complete cases
  df.complete <- df[completeCasesIndex,]
  
  #Calculate the mahalanobis distance with those complete cases
  out.complete <- stats::mahalanobis(df.complete, colMeans(df.complete), cov(df.complete))
  
  #Initialize the ouput vector as NA with the full length of nrow(df) so that we can include the complete case (non-NA) values in their right places
  out <- rep(NA, nrow(df))
  
  #Add mahalanobis distance values back into this NA vector in their right places (so that cases with NA values will have an NA value here)
  out[completeCasesIndex] <- out.complete
  
  return(out)
  
}



####  Inter-Item Standard Deviation  ####
interItemSD <- function(df, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) out[i] <- sd(df[i,], na.rm = T) #This is simply the standard deviation of the response set, with NA values removed
  
  return(out)
  
}



####  Person-Total Correlation  ####
personTotalCor <- function(df, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    responseSet <- as.numeric(df[i,]) #Take the response set...
    meanResponseSet <- sapply(df[-i,], mean, na.rm = T) #...and the mean set of all other response sets...
    out[i] <- cor(responseSet, meanResponseSet) #...and output their correlation for each row (response set)
    
  }
  
  return(out)
  
}