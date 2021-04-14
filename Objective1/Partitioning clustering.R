install.packages("NbClust")
library(NbClust)
library(readxl)
Whitewine <- read_excel("Whitewine.xlsx")
head(Whitewine, n = 3)


#remove missing values
Whitewine <- na.omit(Whitewine)

#overview data
summary(Whitewine)

#data types
head(Whitewine)
str(Whitewine)


#changing data to continuous
cont_whitewine <- as.matrix(Whitewine)
head(cont_whitewine, n = 3)
str(cont_whitewine)
summary(cont_whitewine)
plot(cont_whitewine[,c(1)])

#Scaling
#data without the 12th column scaled
data.train <- scale(Whitewine[-12])
summary(data.train)

#the 12th column data
wineQuality.data = Whitewine$quality
str(wineQuality.data)
plot(wineQuality.data)


#Determination of the best number of clusters
#NbClust method
library(factoextra)
library(NbClust)
# up to 15 clusters
set.seed(1234)
nc15 <- NbClust(data.train,
              min.nc=2, max.nc=15,
              method="kmeans")

table(nc15$Best.n[1,])

# up to 7 clusters
set.seed(1234)
nc7 <- NbClust(data.train,
               min.nc=2, max.nc=7,
               method="kmeans")
table(nc7$Best.n[1,])
# the winning clusters so far are k = 2 and k = 4

#ELBOW method - SSE
wss <- 0
for (i in 1:15){
  wss[i] <-
    sum(kmeans(data.train, centers=i)$withinss)
}
plot(1:15,
     wss,
     type="b",     
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

wss2 <- 0
for (i in 1:7){
  wss2[i] <-
    sum(kmeans(data.train, centers=i)$withinss)
}
plot(1:7,
     wss2,
     type="b",     
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#set up data for kmeans model
set.seed(1234)
cluster.eq.2 <- kmeans(data.train, 2)
cluster.eq.4 <- kmeans(data.train, 4)

# Visualize
#k = 2 
library("factoextra")
fviz_cluster(cluster.eq.2, data = data.train,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
#k = 4
fviz_cluster(cluster.eq.4, data = data.train,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#Not provided in discussion as it was to test if results are same (they are same)
#Euclidean distance
set.seed(1234)
euclideanClust =NbClust(data.train,distance="euclidean", min.nc=2,max.nc=7,method="kmeans",index="all")

#Outliers
boxplot(data.train)

#boxplot column 4
outlier_values_col4 <- boxplot.stats(Whitewine$'residual sugar')$out
boxplot(Whitewine$`residual sugar`, main="Residual Sugar", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_col4, collapse=", ")), cex=0.6)

#boxplot for column 6 "free sulfur dioxide"
outlier_values_col6 <- boxplot.stats(Whitewine$'free sulfur dioxide')$out
boxplot(Whitewine$`free sulfur dioxide`, main="Free Sulfur Dioxide", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_col6, collapse=", ")), cex=0.6)

#boxplot for column 8 "density"
outlier_values_col8 <- boxplot.stats(Whitewine$'density')$out
boxplot(Whitewine$`density`, main="Density", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_col8, collapse=", ")), cex=0.6)

#outlier detection LOF algorithm
install.packages("DMwR")
library(DMwR)
outlier.scores <- lof(data.train, k=c(2:15)
#sort
outliers <- order(outlier.scores, decreasing=T)[1:15]
print(outliers)

#load the data where outliers has been removed
Whitewine_outlierFree <- read_excel("Whitewine_outlierFree.xlsx")
#Scale and remove 12th column
data.withoutOutliers <- scale(Whitewine_outlierFree[-12])

#NbClust without outliers
set.seed(1234)
nc_noOutliers <- NbClust(data.withoutOutliers,
                min.nc=2, max.nc=15,
                method="kmeans")

table(nc_noOutliers$Best.n[1,])

set.seed(1234)
nc_noOutliers7 <- NbClust(data.withoutOutliers,
                         min.nc=2, max.nc=7,
                         method="kmeans")

table(nc_noOutliers7$Best.n[1,])

#ELBOW - SSE method without outliers
#k ranger 1:15
wss_withoutOutliers <- 0
for (i in 1:15){
  wss_withoutOutliers[i] <-
    sum(kmeans(data.withoutOutliers, centers=i)$withinss)
}
plot(1:15,
     wss_withoutOutliers,
     type="b",     
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#k range 1:7
wss_withoutOutliers7 <- 0
for (i in 1:7){
  wss_withoutOutliers7[i] <-
    sum(kmeans(data.withoutOutliers, centers=i)$withinss)
}
plot(1:7,
     wss_withoutOutliers7,
     type="b",     
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#the 12th column - labels plot
wineQuality_wo <- Whitewine_outlierFree$quality
#plot original labels
plot(wineQuality.data)
#plot with outliers removed
plot(wineQuality_wo)

#Fit data to k - means model

#Best number of clusters k = 2 (with outliers)
set.seed(1234)
fit.km_bestNC <- kmeans(data.train, 2)
fit.km_bestNC
#visualisation
library(fpc)
plotcluster(data.train, fit.km_bestNC$cluster)

#Best number of clusters k = 4 (with outliers)
set.seed(1234)
fit.km_bestNC4 <- kmeans(data.train, 4)
fit.km_bestNC4
#visualisation
plotcluster(data.train, fit.km_bestNC4$cluster)

#Best number of clusters k = 2 (without outliers)
set.seed(1234)
fit.km_bestNC_noOutliers <- kmeans(data.withoutOutliers, 2)
fit.km_bestNC_noOutliers
#visualisation
plotcluster(data.withoutOutliers, fit.km_bestNC_noOutliers$cluster)

#Best number of clusters k = 5 (without outliers)
set.seed(1234)
fit.km_bestNC_noOutliers5 <- kmeans(data.withoutOutliers, 5)
fit.km_bestNC_noOutliers5
#visualisation
plotcluster(data.withoutOutliers, fit.km_bestNC_noOutliers5$cluster)

#Confusion matrix to count sensitivity and accuracy
confuseTable.km_2 <- table(Whitewine$quality, fit.km_bestNC$cluster)
confuseTable.km_2

confuseTable.km_4<- table(Whitewine$quality, fit.km_bestNC4$cluster)
confuseTable.km_4

confuseTable.km_noO2<- table(Whitewine_outlierFree$quality, fit.km_bestNC_noOutliers$cluster)
confuseTable.km_noO2

confuseTable.km_noO5<- table(Whitewine_outlierFree$quality, fit.km_bestNC_noOutliers5$cluster)
confuseTable.km_noO5

#calculate mean of each attribute
aggregate(Whitewine_outlierFree, by=list(cluster=fit.km_bestNC_noOutliers$cluster), mean)
