winedata<-read.csv(file.choose()) ## use read.csv for csv files
View(winedata)
winedata <- winedata[,-1]
normalized_data<-scale(winedata[,1:13])

d1 <- dist(normalized_data, method = "euclidean")
fit1<-hclust(d1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram
plot(fit1, hang=-1)
rect.hclust(fit1, k=3, border="red")
groups_1<-cutree(fit1,3) # Cutting the dendrogram for 3 clusters


membership_1<-as.matrix(groups_1) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,winedata ) # binding column wise with orginal data
View(final1)
aggregate(final1,by=list(membership_1),FUN=mean) 


twss<-NULL

for (i in 1:10)
  twss[i]=sum(kmeans(normalized_data,centers=i)$tot.withinss)
plot(1:10,twss,type="b",xlab="number of k",ylab="difference",main="elbow plot for original data")
fit2<-kmeans(normalized_data,3)
fit2


cor(data)
# cor = TRUE use correlation matrix for getting PCA scores
?princomp
pcaObj<-princomp(normalized_data, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)

summary(pcaObj)


plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)



pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
winedata_pca<-cbind(normalized_data,pcaObj$scores[,1:3])
View(data_pca)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-winedata_pca[,14:16]
d2 <- dist(clus_data, method = "euclidean")
fit3<-hclust(d2,method="complete") # method here is complete linkage

plot(fit3) # Displaying Dendrogram
plot(fit3, hang=-1)
rect.hclust(fit3, k=3, border="red")
groups_2<-cutree(fit3,3) # Cutting the dendrogram for 3 clusters


membership_2<-as.matrix(groups_2) # cluster numbering 



final2<-cbind(membership_2,winedata ) # binding column wise with orginal data
View(final1)
aggregate(final2,by=list(membership_2),FUN=mean) # Inferences can be


k<-kselection(clus_data,parallel=TRUE,k_threshold = 0.95,max_centers = 20)
k
twss1<-NULL

for (i in 1:10)
  twss1[i]=sum(kmeans(clus_data,centers=i)$tot.withinss)
plot(1:10,twss1,type="b",xlab="number of ",ylab="difference",main="elbow curve for PCA data")

#elbow curve suggest 3 clusters
fit4<-kmeans(winedata,3)
fit4
