####################################
##  Self Report Archival Indices  ##
##     Use Analysis Functions     ##
####################################

library(careless)
library(PerFit)
library(devtools)

#Load analysis functions
devtools::source_url("https://raw.githubusercontent.com/isaacahuvia/self-report-achival-indices/master/Analysis%20Functions.R")

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
df$evenOdd <- evenOdd(df, scaleLookup = scaleLookup, columns = 1:50)
df$interItemSD <- interItemSD(df, scaleLookup = scaleLookup, columns = 1:50)
df$longstring <- longstring(df, columns = 1:50)
for(i in sort(unique(as.vector(as.matrix(df[,1:50]))))) {
  df[[paste0("longstring_", i)]] <- longstring(df, value = i, columns = 1:50)
}
df$mahalanobisDistSq <- mahalanobisDist(df, columns = 1:50)
df$omittedItems <- omittedItems(df, columns = 1:50)
df$personTotalCor <- personTotalCor(df, columns = 1:50)
df$polyGuttmanErrors <- polyGuttmanErrors(df, nCategories = 5, columns = 1:50, scaleLookup = scaleLookup)
df$polyGuttmanErrorsNormed <- polyGuttmanErrors(df, nCategories = 5, norm = T, columns = 1:50)
df$psychSyn <- psychSyn(df, critval = .5, columns = 1:50)
df$resampledConsistency <- resampledConsistency(df, scaleLookup = scaleLookup, columns = 1:50, iterations = 10)
df$reversedItemDifference <- reversedItemDifference(df, scaleLookup = scaleLookup, reversedItems = reversedItems, columns = 1:50)
df$u3 <- u3(df, nCategories = 5, columns = 1:50)
df$zScore <- zScore(df, columns = 1:50)
