####################################
##  Self Report Archival Indices  ##
##     Use Analysis Functions     ##
####################################

library(careless)
library(PerFit)

#Load analysis functions
library(devtools)
source_url("https://raw.githubusercontent.com/isaacahuvia/self-report-achival-indices/master/Analysis%20Functions.R")

#Load sample df to test out functions on 
df <- careless::careless_dataset



####  Use Functions  ####
#In one method, we hold out the response-only dataset (df) and establish a second dataset (out) to hold the additional metrics
out <- df

out$interItemSD <- interItemSD(df)
out$longstring <- longstring(df)
for(i in sort(unique(as.vector(as.matrix(df))))) {
  out[[paste0("longstring_", i)]] <- longstring(df, value = i)
}
out$mahalanobisDist <- mahalanobisDist(df)
out$mahalanobisDistSquared <- mahalanobisDist(df)^2
out$omittedItems <- omittedItems(df)
out$personTotalCor <- personTotalCor(df)
out$polyGuttmanErrors <- polyGuttmanErrors(df, nCategories = length(unique(as.vector(as.matrix(df)))))
out$polyGuttmanErrorsNormed <- polyGuttmanErrors(df, nCategories = length(unique(as.vector(as.matrix(df)))), norm = T)
out$psychSyn <- psychSyn(df)
out$u3 <- u3(df, nCategories = length(unique(as.vector(as.matrix(df)))))
out$zScore <- zScore(df)


#In the other method, we only use one dataset (df) and specify which columns include our response data in each function
df$interItemSD <- interItemSD(df, columns = 1:50)
df$longstring <- longstring(df, columns = 1:50)
for(i in 1:5) {
  df[[paste0("longstring_", i)]] <- longstring(df, value = i, columns = 1:50)
}
df$mahalanobisDist <- mahalanobisDist(df, columns = 1:50)
df$mahalanobisDistSquared <- mahalanobisDist(df, columns = 1:50)^2
df$omittedItems <- omittedItems(df, columns = 1:50)
df$personTotalCor <- personTotalCor(df, columns = 1:50)
df$polyGuttmanErrors <- polyGuttmanErrors(df, nCategories = 5, columns = 1:50)
df$polyGuttmanErrorsNormed <- polyGuttmanErrors(df, nCategories = 5, norm = T, columns = 1:50)
df$psychSyn <- psychSyn(df, columns = 1:50)
df$u3 <- u3(df, nCategories = 5, columns = 1:50)
df$zScore <- zScore(df, columns = 1:50)