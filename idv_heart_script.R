## Target medication for heart disease patients
# Data loading
heart_dis = read.csv("datasets/heart.csv")

# The first five rows of the data set
head(heart_dis, 5)

# check that only numeric variables
lapply(heart_dis, class)


## Quantifying of differences
summary(heart_dis)

# Remove id's
heart_dis = heart_dis[ , !(names(heart_dis) %in% c('id'))]

# Scaling dataset to df
scaled = scale(heart_dis)
summary(scaled)



## Grouping patients
# Seed setting so the results are reproducible
seed_val = 10
set.seed(seed_val)

# Cluster' numbers
k = 5

# Apply first k-means algorithm on 1st cluster
cluster_1 = kmeans(scaled, centers = k, nstart = 1)

# Patients in each group
cluster_1$size



## Another round of k-means
# Seed setting
seed_val = 38
set.seed(seed_val)

# Apply first k-means algorithm on 2nd cluster
k = 5
cluster_2 = kmeans(scaled, k, nstart=1)

# Patients in each group
cluster_2$size




##Analyze patient's 1st and 2nd cluster
# adding cluster assignments to the data
heart_dis['cluster_1'] = cluster_1$cluster
heart_dis['cluster_2'] = cluster_2$cluster

# Loading ggplot2 and 
library(ggplot2)

# generate the plots of age and chol for the 1st clustering algorithm
first_plot = ggplot(heart_dis, aes(x=age, y=chol, color=as.factor(cluster_1))) + geom_point()
first_plot 

# generate the plots of age and chol for the 2nd clustering algorithm
second_plot = ggplot(heart_dis, aes(x=age, y=chol, color=as.factor(cluster_2))) + geom_point()
second_plot



## First hierarchical clustering
# creating hierarchical clustering with complete linkage
hier_clust_1 = hclust(dist(scaled), method= 'complete')

# generate dendrogram
plot(hier_clust_1)

# creating cluster assignments based on the number of selected clusters
hc_1_assign <- cutree(hier_clust_1, 5)




## Second hierarchical clustering
# creating hierarchical clustering with complete linkage
hier_clust_2 = hclust(dist(scaled), method='single')

# generate dendrogram
plot(hier_clust_2)

# creating cluster assignments based on the number of selected clusters
hc_2_assign <- cutree(hier_clust_2,5)




##Comparing the results
# adding assignments of chosen hierarchical linkage
heart_dis['hc_clust'] = hc_1_assign

# remove variables ('sex', 'cluster_1', and 'cluster_2')
hd_simple = heart_dis[, !(names(heart_dis) %in% c('sex', 'cluster_1', 'cluster_2'))]

# generate mean and standard deviation summary statistics
clust_sum = do.call(data.frame, aggregate(. ~hc_clust, data = hd_simple, function(x) c(avg = mean(x), sd = sd(x))))
clust_sum


##Visualizing the contents of cluster
# age and chol
first_plot = ggplot(hd_simple, aes(x=age, y=chol, color=as.factor(hc_clust))) + geom_point()
first_plot 

# oldpeak and trestbps
second_plot = ggplot(hd_simple, aes(oldpeak, trestbps, color=as.factor(hc_clust))) + geom_point()
second_plot


##Conclusion
explore_kmeans = F
explore_hierarch_complete = T
explore_hierarch_single = F