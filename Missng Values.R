
#https://www.youtube.com/watch?v=An7nPLJ0fsg



library(mice)
library(VIM)

data <- read.csv ("environmentaldata.csv", header = T)
summary(data)
str(data)

###########  Percentage of NA's in columns 
p <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,p)

## Plot of Missing Data
md.pattern(data)
md.pairs(data)
marginplot(data[,c('Galium P','Galium N')])


impute <- mice(data[,4:30], m=3, seed = 123)

###########  "" = no misisng value so no method used, pmm = predictive mean matching , polyreg = multinomial logistic regression (for factor variables)
print(impute)

###########  view the imputed values. This will list all the NA rows and see what the 1,2,and 3rd imputations are
impute$imp$Galium.P

summary(data$Galium.P)

###########  Complete data
data <- complete(impute,1)
newdata <-impute$imp$Galium.P

########### Distribution of obsererved/imputed value 
# Blue dots are observed values, which are available. 
# Red dots are the values that are estimated based on the method we used in the each umputaton, 1,2,3
stripplot(impute,pch =2 ,cex =1.2)

xyplot(impute, Subsoil.pH ~ Galium.P | .imp,pch =20, cex =1.4)



############################################ Hierarchical Cluster ################

library(stats)
library(gplots)
library(cluster)

#computes cosine distance function
cosineDist <- function(x){
  as.dist( 1-x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#computes pearson correlation distance function
dist2 <- function(x, ...)
  as.dist(1-cor(t(x),method="pearson"))

hierarchical <- function(x,k){
  d <- dist(data,method ="euclidean")
  #d <-  cosineDist(as.matrix(data))
  h_com <- hclust(d, method = "ward")
  clusters <- cutree(h_com, k = k)
  list(cluster=clusters)
}




#compute distance metric
d <- dist(data,method ="euclidean")
#d <-  cosineDist(as.matrix(data))

#hierarchical clustering
h_com <- hclust(d, method = "complete")
h_avg <- hclust(d, method = "average")
h_single <- hclust(d, method = "single")
h_ward <- hclust(d,method="ward.D2")
#plot the hierarchical tree
plot(h_com)
plot(h_avg)
plot(h_single)


#cut tree at a particular height to clusters
clusters1 <- data.frame(cutree(h_com, h = 600))


#cut tree by providing the desired number of clusters
clusters2 <- data.frame(cutree(h_com, k = 4))

# visualize the dissimilarity matrix
#visualize as a heatmap
#orginal distance matrix before clustering
heatmap.2(as.matrix(d),scale="none",dendrogram="none",trace="none",Rowv=FALSE,Colv=FALSE)
#distance matrix after clustering
heatmap.2(as.matrix(d),scale="none",dendrogram="both",trace="none",Rowv=as.dendrogram(h_com),Colv=as.dendrogram(h_com))

#OR
dst <- data.matrix(d)
dst <- dst[h_com$order,h_com$order]
heatmap.2(as.matrix(dst),scale="none",dendrogram="none",trace="none",Rowv=FALSE,Colv=FALSE)


#visualizing the actual data as a heatmap
heatmap.2(as.matrix(data),scale="col",col=bluered,dendrogram="row",trace="none",Rowv=as.dendrogram(h_com),Colv=FALSE)

#getting average silhouette and sse for different numbers of clusters
#needs cluster library


kmax=4
sil <- rep(0, kmax)
see <- rep(0, kmax)
dst <- data.matrix(d)
for(k in 4:kmax)
{
  clusters <- cutree(h_ward, k = k)
  si <- silhouette(clusters,dmatrix=dst)
  sil[k]= mean(si[,3])
  for(i in 1:k)
  {
    dd <- dst[clusters==i,]
    dd <- dd[,clusters==i]
    see[k] = see[k] + sum(sum(dd))/2 
  }
}
# Plot the  average silhouette width
plot(1:kmax, sil[1:kmax], type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Avg silhouette")
abline(v = which.max(sil[1:kmax]), lty = 2)

plot(1:kmax, see[1:kmax], type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="SSE")
#abline(v = which.min(see[1:kmax]), lty = 2)




#gap statistic (requires custom R function )
gap <- rep(0,kmax)
gap_stat <- clusGap(data,FUNcluster = hierarchical,K.max=kmax)
ELogW <- gap_stat$Tab[,2]
logW <- gap_stat$Tab[,1]
gap <- ELogW - logW
error <- gap_stat$Tab[,4]
plot(1:kmax, gap, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Gap stat")
arrows(1:kmax, gap-error, 1:kmax, gap+error,length=0.05, angle=90, code=3)


#NbClust to determine the number of clusters (limited only to kmeans and hierarchical)

dev.off()















