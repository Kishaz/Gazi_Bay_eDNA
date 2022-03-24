#This R script implements the probabilistic model of species co-occurrence (Veech 2013) to a set of species distributed among a set of sampled sites.
##The program calculates the observed and expected frequencies of co-occurrence between each pair of species. Expected frequency is based on the distribution of each species being random and independent of the other species. Program also returns the probabilities that a more extreme (either low or high) value of co-occurrence could have been obtained by chance. These probabilities can be interpreted as P-values.

## Load
library(cooccur)
library(visNetwork)

# use first column for row names
df <- data.frame(Project_data, row.names = 1)
##The dataset is arranged “spp_site” (row,col) with 0’s and 1’s representing absences and presences. Use a threshold “thresh=TRUE” to filter out species pairs that an not expected to co-occur more than once based on there occurrences. The dataset has species names in the data.frame row.names.

# Find significant pairwise co-occurrences.
co <- cooccur(df,type = "spp_site", thresh = FALSE, spp_names = TRUE)
#The cooccur function returns a list of class “cooccur” that contains the results of the analysis. We can use the summary method to get a read of number of positive, negative, random, and classifiable species pairs.
#Summary
summary(co)

#To return a table of species pairs and their co-occurrence statistics use the “prob.table()” function. The table has values for “p_lt” and “p_gt” which are the probabilities that the two species do not co-occur less or more frequently than expected.
prob.table(co)

#You can visually interpret these co-occurrence results using plot().
plot(co)

# add "plotrand = TRUE" to include completely random species
plot(co, plotrand=TRUE)
