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

#Establish scale lookup
scaleLookup <- data.frame(
  scale = c(rep("A", 5),
            rep("B", 5),
            rep("C", 5),
            rep("D", 5),
            rep("E", 5),
            rep("F", 5),
            rep("G", 5),
            rep("H", 5),
            rep("I", 5),
            rep("J", 5)),
  variable = names(df)
)

#Establish reversed question lookup - these aren't actually reverse-coded, but for the sake of running the function we'll pretend like they are
reversedItems <- c("scaleA1", "scaleA2", "scaleB1", "scaleB5", "scaleF3")



####  Use Functions  ####
#In one method, we hold out the response-only dataset (df) and establish a second dataset (out) to hold the additional metrics
out <- df

out$evenOdd <- evenOdd(df, scaleLookup = scaleLookup)
out$resampledConsistency <- resampledConsistency(df, scaleLookup = scaleLookup, iterations = 10)
out$interItemSD <- interItemSD(df, scaleLookup = scaleLookup)
out$longstring <- longstring(df)
for(i in sort(unique(as.vector(as.matrix(df[,1:50]))))) {
  out[[paste0("longstring_", i)]] <- longstring(df, value = i)
}
out$mahalanobisDist <- mahalanobisDist(df)
out$mahalanobisDistSquared <- out$mahalanobisDist^2
out$omittedItems <- omittedItems(df)
out$personTotalCor <- personTotalCor(df)
out$polyGuttmanErrors <- polyGuttmanErrors(df, nCategories = 5)
out$polyGuttmanErrorsNormed <- polyGuttmanErrors(df, nCategories = 5, norm = T)
out$psychSyn <- psychSyn(df, critval = .5)
out$u3 <- u3(df, nCategories = 5)
out$zScore <- zScore(df)
out$reversedItemCorrelation <- reversedItemCorrelation(df, scaleLookup = scaleLookup, reversedItems = reversedItems)


#In the other method, we only use one dataset (df) and specify which columns include our response data in each function
df$evenOdd <- evenOdd(df, scaleLookup = scaleLookup, columns = 1:50)
df$resampledConsistency <- resampledConsistency(df, scaleLookup = scaleLookup, columns = 1:50, iterations = 10)
df$interItemSD <- interItemSD(df, scaleLookup = scaleLookup, columns = 1:50)
df$longstring <- longstring(df, columns = 1:50)
for(i in sort(unique(as.vector(as.matrix(df[,1:50]))))) {
  df[[paste0("longstring_", i)]] <- longstring(df, value = i, columns = 1:50)
}
df$mahalanobisDist <- mahalanobisDist(df, columns = 1:50)
df$mahalanobisDistSquared <- df$mahalanobisDist^2
df$omittedItems <- omittedItems(df, columns = 1:50)
df$personTotalCor <- personTotalCor(df, columns = 1:50)
df$polyGuttmanErrors <- polyGuttmanErrors(df, nCategories = 5, columns = 1:50)
df$polyGuttmanErrorsNormed <- polyGuttmanErrors(df, nCategories = 5, norm = T, columns = 1:50)
df$psychSyn <- psychSyn(df, critval = .5, columns = 1:50)
df$u3 <- u3(df, nCategories = 5, columns = 1:50)
df$zScore <- zScore(df, columns = 1:50)
df$reversedItemCorrelation <- reversedItemCorrelation(df, scaleLookup = scaleLookup, reversedItems = reversedItems, columns = 1:50)