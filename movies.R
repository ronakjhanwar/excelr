movies <- read.csv(file.choose())
movies1<-movies[,(1:5),header=FALSE]
movies2<-movies[c(6:15)]
str(movies1)
movies1[] <- lapply(movies1,as.character)
View(movies1)
movies1<-apply(movies1,2,function(x)gsub('\\s+', '',x))
movies1<-as.data.frame(apply(movies1,2,function(x)gsub('\\s+', '',x)))
# Creating a custom fucntion to collapse all the items in a transaction into 
# a single sentence 
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
movies1["new_col"] <- apply(movies1,1,paste_fun)
View(movies1)

install.packages("tm")
# tm package is used to do text manipulation and forming DTM and TDM matrices
library(tm)
x <- Corpus(VectorSource(movies1$new_col)) # Selecting the new column which
# contains all items of a transaction in a single sentence
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(x))
# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

library(arules)
library(arulesViz)
# Item Frequecy plot
windows()
# count of each item from all the transactions 
barplot(sapply(movies2,sum),col=1:10)
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(movies2),parameter = list(support=0.08,confidence=0.8,minlen=4))
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
library(arules)
data <- sapply(movie, function(x) gsub(" ",'',  x))
movie<-read.transactions(file.choose(),format="basket")
View(movie)
str(movie)
movie[] <- lapply(movie,as.character)
inspect(movie[1:5])
class(groceries)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(data,topN=10)
movie_rules<-apriori(movie,parameter = list(support = 0.05,confidence = 0.5,minlen=4))
inspect(movie_rules)
a<-c(1)
