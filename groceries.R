library(arules)
options(warn=-1)
groceries<-read.transactions(file.choose(),format="basket")
View(groceries)
inspect(groceries[1:10])
class(groceries)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=20)
groceries_rules<-apriori(groceries,parameter = list(support = 0.003,confidence = 0.04,minlen=3))
inspect(groceries_rules[1:4])
library(arulesViz)
plot(groceries_rules,method = "scatterplot",jitter=0)
plot(groceries_rules,method = "grouped",jitter=0)
plot(groceries_rules,method = "graph",jitter=0)
plot(groceries_rules,method = "paracoord",jitter=0)
?plot
