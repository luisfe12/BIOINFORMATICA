print("PRACTICA GRUPAL")
#if(!requireNamespace("BiocManager"))
    #install.packages("BiocManager")

#BiocManager::install(c("golubEsets"))
# load require package and datasets
require(golubEsets)
data(Golub_Merge)
golub <- data.frame(Golub_Merge)[1:7129]
# calculate variance and rearrange columns by variance decreasingly
golub.rearrange <- golub[, order(apply(golub, 2, var), decreasing = T)]
golub <- golub.rearrange[, 1:150]

# K-Means Cluster Analysis
fit <- kmeans(x = golub, 8)

fit$cluster  # get cluster assignment
fit$centers  # get cluster center
# get cluster means
aggregate(golub, by = list(fit$cluster), FUN = mean)

require(cluster)
fit <- pam(x = golub, k = 8)

fit$clustering  # get cluster assignment
fit$medoids  # get coordinates of each medoid
# summary method
summary(fit)

# Ward Hierarchical Clustering
d <- dist(golub, method = "euclidean")  # distance matrix
fit <- hclust(d, method = "ward")

plot(fit)
groups <- cutree(fit, k = 8)  # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k = 8, border = "red")

require(corrplot)

corrplot(cor(golub.rearrange[, 1:20]))
