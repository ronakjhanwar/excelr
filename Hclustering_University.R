Uni <- read.csv(file.choose())
View(Uni)
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(Uni[,2:7]) #excluding the city name university before normalizing
?dist
d <- dist(normalized_data, method = "euclidean") # distance matrix
?hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)

?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust
groups <- cutree(fit, k=3) # cut tree into 3 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(Uni, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(Uni[,-1],by=list(final$membership),mean)

