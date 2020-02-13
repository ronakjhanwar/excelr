book <- read.csv(file.choose())

library(arules)
library(arulesViz)
# Item Frequecy plot
windows()
# count of each item from all the transactions 
barplot(sapply(book,sum),col=1:11)
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(book),parameter = list(support=0.02,confidence=0.5,minlen=3))
inspect(rules[1:4])
plot(rules,jitter=0)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

# Visualizing rules in scatter plot
?plot



library(arulesViz)
plot(rules,method = "scatterplot",jitter=0)
plot(rules,method = "grouped",jitter=0)
plot(rules,method = "graph")
plot(rules,method = "matrix")
