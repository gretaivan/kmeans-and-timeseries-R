Contents
Objective 1: Partitioning Clustering	2
1.	Pre-processing tasks	2
1.1.	Remove missing values in the data set	2
1.2.	Review and get to know the data	2
1.3.	Checking the types of data	3
1.4.	Scaling	4
1.5.	Outliers	5
2.	Ideal number of clusters	8
2.1.	NbClust	8
2.2.	Sum of square error	10
2.3.	Visualising clusters	11
2.4.	Ideal number of clusters with outliers removed	12
3.	K-means with the best two clusters	14
4.	Check consistency of results against 12th column	19
5.	The mean of each attribute for the winner cluster	22
Conclusion	22
Code	22
Objective 2	29
1.	Pre-processing	31
2.	Input	31
2.1.	Stationarity	32
2.2.	Detrending	32
2.3.	Lag values	35
3.	MLP model14	36
Code	39
Bibliography	48


PLEASE NOTE THAT TEXT IN BLUE IS THE CODE, AND TEXT IN GREEN IS CODE RESULTS.
Objective 1: Partitioning Clustering

1.	Pre-processing tasks
Nearest-neighbour techniques like many other are sensitive to noise when training the data. The general rule to execute the well performing clustering analysis requires data pre-processing, otherwise it may produce the wrong predictions. Pre-processing steps involves checking: 
1.1.	Remove missing values in the data set
Whitewine <- na.omit(Whitewine)
1.2.	Review and get to know the data
summary(Whitewine)
fixed acidity    volatile acidity  citric acid     residual sugar     chlorides      
 Min.     : 3.800   Min.   :0.0800   Min.   :0.0000   Min.   : 0.600   Min.   :0.00900  
 1st Qu.: 6.300   1st Qu.:0.2100   1st Qu.:0.2700   1st Qu.: 1.700   1st Qu.:0.03600  
 Median : 6.800   Median :0.2600   Median :0.3200   Median : 5.200   Median :0.04300  
 Mean   : 6.855   Mean   :0.2782   Mean   :0.3342   Mean   : 6.391   Mean   :0.04577  
 3rd Qu.: 7.300   3rd Qu.:0.3200   3rd Qu.:0.3900   3rd Qu.: 9.900   3rd Qu.:0.05000  
 Max.   :14.200   Max.   :1.1000   Max.   :1.6600   Max.   :65.800   Max.   :0.34600  
 free sulfur dioxide total sulfur dioxide    density             pH       
 Min.   :  2.00      Min.   :  9.0        Min.   :0.9871   Min.   :2.720  
 1st Qu.: 23.00      1st Qu.:108.0        1st Qu.:0.9917   1st Qu.:3.090  
 Median : 34.00      Median :134.0        Median :0.9937   Median :3.180  
 Mean   : 35.31      Mean   :138.4        Mean   :0.9940   Mean   :3.188  
 3rd Qu.: 46.00      3rd Qu.:167.0        3rd Qu.:0.9961   3rd Qu.:3.280  
 Max.   :289.00      Max.   :440.0        Max.   :1.0390   Max.   :3.820  
   sulphates         alcohol         quality     
 Min.   :0.2200   Min.   : 8.00   Min.   :3.000  
 1st Qu.:0.4100   1st Qu.: 9.50   1st Qu.:5.000  
 Median :0.4700   Median :10.40   Median :6.000  
 Mean   :0.4898   Mean   :10.51   Mean   :5.878  
 3rd Qu.:0.5500   3rd Qu.:11.40   3rd Qu.:6.000  
 Max.   :1.0800   Max.   :14.20   Max.   :9.000
  
#the 12th column data
wineQuality.data = Whitewine$quality
str(wineQuality.data)
plot(wineQuality.data)
Assigning 12th column “quality” data to the new variable, also checking how is the data distributed.
 

We can see that there are 7 groups, with most data density in quality indexes: 5, 6 and 7. We have very little data in the 9th column, this indicates that we need to be careful with outlier removal. Group 9 also has just 4 samples in the data set.
1.3.	Checking the types of data
head(Whitewine)
# A tibble: 6 x 12
  `fixed acidity` `volatile acidi~ `citric acid` `residual sugar` chlorides
            <dbl>            <dbl>         <dbl>            <dbl>     <dbl>
1             7               0.27          0.36             20.7     0.045
2             6.3             0.3           0.34              1.6     0.049
3             8.1             0.28          0.4               6.9     0.05 
4             7.2             0.23          0.32              8.5     0.058
5             7.2             0.23          0.32              8.5     0.058
6             8.1             0.28          0.4               6.9     0.05 
# ... with 7 more variables: `free sulfur dioxide` <dbl>, `total sulfur dioxide` <dbl>,
#   density <dbl>, pH <dbl>, sulphates <dbl>, alcohol <dbl>, quality <dbl>

str(Whitewine)
Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	4898 obs. of  12 variables:
 $ fixed acidity       : num  7 6.3 8.1 7.2 7.2 8.1 6.2 7 6.3 8.1 ...
 $ volatile acidity    : num  0.27 0.3 0.28 0.23 0.23 0.28 0.32 0.27 0.3 0.22 ...
 $ citric acid         : num  0.36 0.34 0.4 0.32 0.32 0.4 0.16 0.36 0.34 0.43 ...
 $ residual sugar      : num  20.7 1.6 6.9 8.5 8.5 6.9 7 20.7 1.6 1.5 ...
 $ chlorides           : num  0.045 0.049 0.05 0.058 0.058 0.05 0.045 0.045 0.049 0.044 ...
 $ free sulfur dioxide : num  45 14 30 47 47 30 30 45 14 28 ...
 $ total sulfur dioxide: num  170 132 97 186 186 97 136 170 132 129 ...
 $ density             : num  1.001 0.994 0.995 0.996 0.996 ...
 $ pH                  : num  3 3.3 3.26 3.19 3.19 3.26 3.18 3 3.3 3.22 ...
 $ sulphates           : num  0.45 0.49 0.44 0.4 0.4 0.44 0.47 0.45 0.49 0.45 ...
 $ alcohol             : num  8.8 9.5 10.1 9.9 9.9 10.1 9.6 8.8 9.5 11 ...
 $ quality             : num  6 6 6 6 6 6 6 6 6 6 ...
We can see that attributes has the same data type, and is numerical. This means that there is no need to remove any columns at the moment. Later the last attribute “quality” will be removed as this is the label column. 
However, it is double and not continues type of data. Partitioning clustering and in particular k-means must be performed on the continuous data as values needs to be measure not counted, otherwise it might not be as efficient and accurate as expected and in some cases attempt to execute k-means may return the errors.
For instance, when you are trying to plot the view of the attribute you get plot such as this: 
plot(Whitewine[,c(1)])
  
Therefore, the data type needs to be changed to the one which allows the distance measurements.
cont_whitewine <- as.matrix(Whitewine)
plot(cont_whitewine[,c(1)])
 

head(cont_whitewine, n = 3)
   fixed acidity volatile acidity
[1,]           7.0             0.27
[2,]           6.3             0.30
[3,]           8.1             0.28
     citric acid residual sugar chlorides
[1,]        0.36           20.7     0.045
[2,]        0.34            1.6     0.049
[3,]        0.40            6.9     0.050
     free sulfur dioxide
[1,]                  45
[2,]                  14
[3,]                  30
     total sulfur dioxide density   pH
[1,]                  170  1.0010 3.00
[2,]                  132  0.9940 3.30
[3,]                   97  0.9951 3.26
     sulphates alcohol quality
[1,]      0.45     8.8       6
[2,]      0.49     9.5       6
[3,]      0.44    10.1       6

The change of the dataset allows to review and analyse the data which helps to understand the data and analyse the dimensionality, sparseness, potential outliers and etc. 

1.4.	Scaling

The scaling is important because the data set attributes may be measured on different measurement  scales. This affects the distance and similarity between two objects. In the white wine data set it is clear that almost every attribute uses different measurement units and some of them has low variability. In addition to that as discussed in section 1.3. the data types needs to enable distance measurements. Below is the code that executes scaling function. 
data.train <- scale(Whitewine[-12])
summary(data.train)
fixed acidity      volatile acidity   citric acid      residual sugar   
 Min.   :-3.61998   Min.   :-1.9668   Min.   :-2.7615   Min.   :-1.1418  
 1st Qu.:-0.65743   1st Qu.:-0.6770   1st Qu.:-0.5304   1st Qu.:-0.9250  
 Median :-0.06492   Median :-0.1810   Median :-0.1173   Median :-0.2349  
 Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
 3rd Qu.: 0.52758   3rd Qu.: 0.4143   3rd Qu.: 0.4612   3rd Qu.: 0.6917  
 Max.   : 8.70422   Max.   : 8.1528   Max.   :10.9553   Max.   :11.7129  

   chlorides       free sulfur dioxide total sulfur dioxide
 Min.   :-1.6831   Min.   :-1.95848    Min.   :-3.0439     
 1st Qu.:-0.4473   1st Qu.:-0.72370    1st Qu.:-0.7144     
 Median :-0.1269   Median :-0.07691    Median :-0.1026     
 Mean   : 0.0000   Mean   : 0.00000    Mean   : 0.0000     
 3rd Qu.: 0.1935   3rd Qu.: 0.62867    3rd Qu.: 0.6739     
 Max.   :13.7417   Max.   :14.91679    Max.   : 7.0977  
   
    density               pH             sulphates          alcohol        
 Min.   :-2.31280   Min.   :-3.10109   Min.   :-2.3645   Min.   :-2.04309  
 1st Qu.:-0.77063   1st Qu.:-0.65077   1st Qu.:-0.6996   1st Qu.:-0.82419  
 Median :-0.09608   Median :-0.05475   Median :-0.1739   Median :-0.09285  
 Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.00000  
 3rd Qu.: 0.69298   3rd Qu.: 0.60750   3rd Qu.: 0.5271   3rd Qu.: 0.71974  
 Max.   :15.02976   Max.   : 4.18365   Max.   : 5.1711   Max.   : 2.99502

Data scaling and centring for training has been performed, now the data is suitable for distance measurements and mapping. Also, the 12th “quality” column has been removed.

1.5.	Outliers

Whilst outliers can strongly affect the generated results and any sample that has abnormal data is best to remove, for example if the measurements of the spoilt wine has been included in the data set, using this to train your classification model with affect its performance of classification. Nonetheless, a great caution should be taken when removing outliers as it may contain important information and give a good insight to the data. 
Number of sources does recommend to analyse the data first without outlier removal and detect them in the process. In addition to that I want to see if outlier removal does affect the data and affect the detection of best number of clusters (NbClust and SSE methods) and accuracy. Therefore, first I worked on the data without removing outliers as well as executed the detection of clustering algorithms, then I visualised the data with suggested cluster numbers. This is where the possible outliers has been detected. See the section  2.3 to see the how visualising has been done. However, the produced graph is also provided in this section below. In this section I will only provide the code with results for outlier removal. Finding of ideal number of clusters for outlier free data set will be in the section 2.3. 
The graph below suggests that there are potentially 2 – 3 outliers.  
Let’s plot the data differently where we can see the data distribution for each attribute. The plot method that I am going to use is boxplot, it allows to see data distribution and see median as well as how the values distributed from it and is great to detect the extreme values.
boxplot(train.data)
 

According to the boxplot graph we can see that there are 3 attributes namely (columns: 4, 6 and 8), that contain at least one sample that is very far away from the most of data. I will plot every column and extract the outlier value, then create the data sample that is “outlier free”. 
#boxplot column 4
outlier_values_col4 <- boxplot.stats(Whitewine$'residual sugar')$out
boxplot(Whitewine$`residual sugar`, main="Residual Sugar", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_col4, collapse=", ")), cex=0.6)
 
There are 7 suggested outliers in this data column however we do not want to remove all of them. For our work we want to get rid off only very extreme values, in this feature it would be value 65.8, in our original Whitewine.xlsx this is the sample in 2783 row.
 

#boxplot for column 6 "free sulfur dioxide"
outlier_values_col6 <- boxplot.stats(Whitewine$'free sulfur dioxide')$out
boxplot(Whitewine$`free sulfur dioxide`, main="Free Sulfur Dioxide", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_col6, collapse=", ")), cex=0.6)
 
Following our outlier principle in free sulfur dioxide column we also select only the most distant value, here it is 289. In the original data it is row 4747.

#boxplot for column 8 "density"
outlier_values_col8 <- boxplot.stats(Whitewine$'density')$out
boxplot(Whitewine$`density`, main="Density", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_col8, collapse=", ")), cex=0.6)
 
Here the value the most extreme value is 1.03898 and this is the same row (2783) as highlighted in residual sugar. So it is very likely that this sample may be misleading. According the analysis two samples in total should be removed from this data set. I removed it in the excel and saved the file as Whitewine_outlierFree.xlsx.
Furthermore there are algorithms that automatically detects outliers, such as LOF (Local Outlier Factor): 

#outlier detection LOF algorithm
install.packages("DMwR")
library(DMwR)
outlier.scores <- lof(data.train, k=c(2:15)
#sort
outliers <- order(outlier.scores, decreasing=T)[1:15]
print(outliers) 
[1] 2782 4746 4481  746 1527 1654 1664 2128 2335  485 3216 3266 3153  947  2669
However, I chose not to use this algorithm in my work, simply because it did not pickup the most
radical values that were most distant from the other data or perhaps I did not understand how to 
use it properly.

2.	Ideal number of clusters

2.1.	NbClust

Choosing the right number of clusters is very important task.  As choosing too small k number (neighbourhood size), e.g. k =  1 is often prone to overfitting and becomes sensitive to the noise in the training data. If k is too large, there is high chance of misclassification as the list of the nearest neighbours may include data sets that are too far away from the centroid. Therefore, there has been created number of algorithms that automatically detect the best number of clusters, they are very effective but sensitive to the noise in data (for example outliers).  One of the methods is NbClust function, below there is the code how to execute this method. The output will provide the voting for the best number of clusters. I will run method twice with different k ranges: 2 : 15 and 2 : 7. 
#NbClust method
library(factoextra)
library(NbClust)
set.seed(1234)
nc15 <- NbClust(data.train,
              min.nc=2, max.nc=15,
              method="kmeans")
	      table(nc7$Best.n[1,])

******************************************************************* 
* Among all indices:                                                
* 10 proposed 2 as the best number of clusters 
* 4 proposed 3 as the best number of clusters 
* 4 proposed 4 as the best number of clusters 
* 1 proposed 5 as the best number of clusters 
* 2 proposed 6 as the best number of clusters 
* 1 proposed 9 as the best number of clusters 
* 2 proposed 15 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is 2 
 
 
******************************************************************* 
 
table(nc15$Best.n[1,])

 0  2  3  4  5  6  9 15 
 2 10  4  4  1  2  1  2 


nc7 <- NbClust(data.train,
              min.nc=2, max.nc=7,
              method="kmeans")
 

******************************************************************* 
* Among all indices:                                                
* 10 proposed 2 as the best number of clusters 
* 4 proposed 3 as the best number of clusters 
* 5 proposed 4 as the best number of clusters 
* 1 proposed 5 as the best number of clusters 
* 2 proposed 6 as the best number of clusters 
* 2 proposed 7 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  2 

table(nc7$Best.n[1,])

 0  2  3  4  5  6  7 
 2 10  4  5  1  2  2   

K = 2 has been proposed by majority, in both NbCluster processes. Two second bests are k = 3 and k = 4. However, NbCluster process ranging k = 2 to k = 7 results are that 5 proposed 4 as the best number of clusters, thus the winning clusters at the moment are k = 2 and k = 4. This is surprising result as we know that data set has 7 classes, so manually I would of chosen 7 clusters. However, the further check needs to be done. Such as SSE (sum of square error). 
2.2.	Sum of square error

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
In the diagram above we need to look where the most significant drop ends (the point of the elbow). In the plot we see that the suggested number is 2 as well.  The same code has been ran with 1 : 7 ratio and the same results has been shown, see below visualisation of 2 graphs along-side each other.  Conclusion is that the best number of clusters is 2. This has been proposed by NbClust and SSE methods.
2.3.	Visualising clusters

It is advisable to overview the clustered data and check if there are no anomalies that could of affected the cluster selection. Below is the code that plots the elected number of clusters:  k = 2 and k = 4.
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
 
Looking in both plots, the main thing what draws attention are 2 data variables: 4746 and 2782 that  are located very far away from the other data, these potentially could be outliers. The process when outliers are detected is explained in the section 1.5.

2.4.	Ideal number of clusters with outliers removed

The following step is to check if the ideal number for clusters for data that is outlier free is the same. 

#load the data where outliers has been removed
Whitewine_outlierFree <- read_excel("Whitewine_outlierFree.xlsx")
#Scale and remove 12th column
data.withoutOutliers <- scale(Whitewine_outlierFree[-12])

#NbClust without outliers
set.seed(1234)
nc_noOutliers <- NbClust(data.withoutOutliers,
                min.nc=2, max.nc=15,
                method="kmeans")

 
Among all indices:                                                
* 11 proposed 2 as the best number of clusters 
* 3 proposed 3 as the best number of clusters 
* 2 proposed 4 as the best number of clusters 
* 4 proposed 5 as the best number of clusters 
* 1 proposed 9 as the best number of clusters 
* 1 proposed 10 as the best number of clusters 
* 2 proposed 15 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  2 
 
 
******************************************************************* 
table(nc_noOutliers$Best.n[1,])

 0  2  3  4  5  9 10 15 
 2 11  3  2  4  1  1  2 

set.seed(1234)
nc_noOutliers7 <- NbClust(data.withoutOutliers,
                         min.nc=2, max.nc=7,
                         method="kmeans")

 
table(nc_noOutliers7$Best.n[1,])
******************************************************************* 
* Among all indices:                                                
* 11 proposed 2 as the best number of clusters 
* 4 proposed 3 as the best number of clusters 
* 2 proposed 4 as the best number of clusters 
* 5 proposed 5 as the best number of clusters 
* 2 proposed 7 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  2 
******************************************************************* 

We can see that even after removal of outliers majority still proposed 2 as the best number of clusters, but the number of proposals increased. The second best number of cluster, however, has changed and  voting is less ambiguous than in the data containing outliers. The second best number of clusters is 5. 
Next step is to check, if SSE method will provide the same results.

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


 

Elbow method results matches NbClust results. To conclude the k=2 is the best number of clusters     and second best after outlier removal is k = 5, whilst before outlier removal was k = 4. 
It is interesting to notice that none of the methods suggested 7 groups. Yet 5 clusters, is a reasonable choice, due to the fact that even though we have 7 types of qualities/groups in our data set, the amount of samples for quality 9 wine and quality 3 wine, is very low. In fact, there are only 4 
samples of quality 9 wine and around 19 samples of quality 3 wine, see original data and data with outliers removed plot below. 

#the 12th column - labels plot
wineQuality_wo <- Whitewine_outlierFree$quality
#plot original labels
plot(wineQuality.data)
#plot with outliers removed
plot(wineQuality_wo)

  
3.	K-means with the best two clusters

As I have two sets of data (data with and without outliers) I will fit 4 k-means models: best two clusters for each data set. Firstly, the finding in section 2 suggest that the best number of clusters are k = 2 and k = 4.

#Fit data to k - means model
#Best number of clusters k = 2 (with outliers)
set.seed(1234)
fit.km_bestNC <- kmeans(data.train, 2)
fit.km_bestNC

K-means clustering with 2 clusters of sizes 2941, 1957

Cluster means:
  fixed acidity volatile acidity citric acid residual sugar  chlorides free sulfur dioxide
1    -0.1170435      -0.03119428  -0.1558422     -0.5642251 -0.2650460          -0.3973986
2     0.1758942       0.04687908   0.2342012      0.8479233  0.3983139           0.5972147

  total sulfur dioxide    density         pH   sulphates    alcohol
1           -0.5094513 -0.6303970  0.1340887 -0.03983286  0.5261637
2            0.7656087  0.9473672 -0.2015099  0.05986124 -0.7907243

Clustering vector:
   [1] 2 1 1 2 2 1 1 2 1 1 1 1 1 1 2 1 1 1 1 2 1 1 1 2 1 2 1 2 1 1 2 1 1 1 2 1 1 1 2 2 2 2 2 1 1
  [46] 1 2 2 2 2 1 1 1 1 2 1 2 2 1 1 2 2 1 1 1 1 1 2 1 2 2 2 2 1 1 2 1 1 2 1 1 2 2 2 2 2 2 2 2 2
  [91] 2 2 1 1 1 2 2 1 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 1 1 1 2 2 1 2 2 1 2 1 1 2 1 1 1 2 2 2 2
 [136] 2 1 2 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 2 2 1 1 1 1 2 1 2 2 2 2 1 2 2 1 1 1 1 2 1 1 2 1 2
 [181] 2 2 2 2 2 2 2 1 1 2 2 2 1 1 2 2 2 2 2 2 2 2 2 1 1 2 2 2 1 1 1 1 2 1 1 2 2 2 2 1 2 2 1 1 1
 [226] 2 1 2 1 2 2 2 2 2 2 2 2 2 1 2 2 1 1 2 2 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 2 1 2 1 2 2 1 1 1
 [271] 2 1 2 2 2 2 2 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 2 1 2 1 2 2 2 1 1 1 1 1 2 2 2 2 1 1 1 1 1 2 2
 [316] 2 1 1 1 1 1 1 1 2 1 2 2 1 2 1 1 1 1 2 1 1 1 2 1 1 2 1 2 1 1 1 1 2 2 2 1 1 1 1 2 2 2 1 1 1
 [361] 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 1 2 1 2 2 1 1 1 2 2 1 1 2 1 1 2 1 2 1
 [406] 2 1 1 1 1 2 2 1 1 2 2 1 2 1 1 1 2 2 2 2 2 2 2 1 2 2 1 2 1 1 1 1 1 2 1 1 1 1 2 2 1 1 2 2 1
 [451] 2 1 1 1 1 2 1 2 1 2 2 2 2 1 2 2 2 1 2 2 2 2 1 1 1 2 1 1 2 1 2 1 2 2 2 1 1 1 1 2 1 1 2 1 1
 [496] 1 2 1 1 2 2 2 1 1 2 2 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 2 2 2 2 2 2 2 1 2
 [541] 1 2 1 2 2 1 2 1 1 2 2 1 1 2 1 1 2 1 2 1 2 1 2 1 2 2 1 2 2 2 2 1 2 1 1 2 2 1 1 1 2 1 2 1 1
 [586] 1 2 1 1 1 2 1 2 1 1 1 2 1 1 1 2 1 1 1 1 1 2 2 2 1 1 1 2 2 2 2 1 1 1 1 2 2 1 1 1 2 2 2 2 1
 [631] 2 2 1 2 1 1 1 2 2 2 1 2 2 2 2 2 1 2 2 2 2 2 2 2 1 1 1 1 1 2 1 1 2 1 2 1 1 2 1 2 2 1 2 1 2
 [676] 2 2 1 1 1 1 1 2 2 1 2 1 2 2 1 1 2 2 2 2 2 1 2 2 2 2 1 1 1 1 1 2 2 2 1 2 1 1 2 1 1 2 2 1 1
 [721] 2 1 1 1 1 1 1 2 1 2 2 2 1 2 2 1 2 2 1 1 1 1 1 2 2 1 2 1 2 1 1 2 2 2 2 1 2 2 1 1 2 2 2 2 2
 [766] 1 2 1 2 2 1 2 2 2 2 1 1 2 2 2 1 2 2 2 2 2 2 2 1 2 2 1 1 1 2 2 2 2 1 1 2 2 2 2 1 2 2 2 2 2
 [811] 2 1 1 2 2 1 1 2 1 2 1 2 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 2 1 2 1 1 2 1 2 1
 [856] 1 2 2 2 2 1 2 1 1 1 1 1 2 2 1 2 2 1 1 1 1 1 1 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2
 [901] 1 2 2 2 2 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 2 2 2 2 1 1 1 2 2 2 1 2 2 2 2 2 1 2 2 2 2 2 1 1 2
 [946] 1 2 1 1 1 1 2 1 1 2 1 1 1 1 2 2 1 2 1 2 1 1 2 1 1 1 1 2 1 1 1 1 2 1 1 1 1 1 1 1 1 2 2 1 1
 [991] 1 1 2 1 2 1 1 2 2 2
 [ reached getOption("max.print") -- omitted 3898 entries ]

Within cluster sum of squares by cluster:
[1] 23413.09 19126.87
 (between_SS / total_SS =  21.0 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
[7] "size"         "iter"         "ifault"  

library(fpc)
plotcluster(data.train, fit.km_bestNC$cluster)
 
We can see two clusters very close to each other but there is no strong overlapping. 
The same process is going to be done for 4 clusters. 

#Best number of clusters k = 4 (with outliers)
set.seed(1234)
fit.km_bestNC4 <- kmeans(data.train, 4)
fit.km_bestNC4
K-means clustering with 4 clusters of sizes 1164, 1014, 1140, 1580

Cluster means:
  fixed acidity volatile acidity citric acid residual sugar   chlorides free sulfur dioxide
1    -0.5087467      -0.36529056  -0.2651359     -0.4984365 -0.03346393          0.01398691
2    -0.6614074       0.46121223  -0.3661372     -0.5588581 -0.50217792         -0.44834977
3     0.9172901      -0.16461204   0.2474706     -0.4682799 -0.08553872         -0.56723950
4     0.1374288       0.09189034   0.2517499      1.0637351  0.40865479          0.68670818
  total sulfur dioxide    density         pH    sulphates    alcohol
1           0.05791035 -0.1956808  0.8477108  0.513452028 -0.1066781
2          -0.77939984 -1.0941777  0.2578959 -0.253576052  1.2398265
3          -0.51193558 -0.3263398 -0.6703469 -0.286890461  0.2179181
4           0.82690529  1.0818329 -0.3063585 -0.008529695 -0.8743275

Clustering vector:
   [1] 4 1 3 4 4 3 1 4 1 3 3 3 3 1 4 1 2 2 3 1 2 2 1 3 1 4 1 1 3 2 3 3 3 1 4 3 1 3 4 4 4 4 4 1 1
  [46] 1 4 4 4 1 1 1 1 1 3 1 4 4 2 1 4 4 2 2 3 1 2 1 1 3 4 4 1 3 1 3 2 2 3 3 1 4 4 4 4 4 4 4 4 4
  [91] 4 4 2 2 3 4 4 3 3 4 4 4 4 4 4 4 4 4 4 1 4 4 4 4 4 1 2 3 4 4 2 4 3 1 1 1 3 1 1 1 2 1 4 4 4
 [136] 4 3 4 3 3 2 4 3 3 3 3 1 2 2 3 3 4 3 3 3 4 4 2 2 2 2 4 2 4 4 4 4 2 4 3 3 3 3 2 4 3 3 4 2 1
 [181] 1 4 4 4 4 4 4 2 2 4 4 4 3 1 4 4 4 4 4 4 4 4 4 1 1 4 4 3 2 1 2 1 4 1 2 4 3 4 4 3 3 4 2 1 1
 [226] 4 1 4 1 4 4 4 4 4 4 4 4 4 3 1 4 3 3 1 4 1 2 2 2 1 1 4 4 2 1 1 2 2 2 2 1 4 3 4 1 4 4 1 1 1
 [271] 4 1 4 4 4 4 4 3 3 2 2 2 4 4 4 4 4 4 4 4 4 1 4 1 4 1 4 1 4 3 2 3 2 1 4 4 4 4 2 1 2 2 1 1 1
 [316] 4 3 3 1 1 1 3 3 3 3 4 4 1 4 3 2 3 2 4 2 2 2 4 1 1 4 3 4 1 1 1 2 4 4 4 1 3 1 3 1 4 4 1 3 1
 [361] 4 3 1 4 1 1 1 1 3 3 1 3 2 3 2 2 1 3 1 1 3 4 4 3 2 2 1 4 1 4 4 2 2 3 4 1 3 1 4 2 1 4 1 4 1
 [406] 1 2 3 1 3 4 4 3 3 4 4 3 4 3 1 2 4 4 4 1 4 4 4 3 4 4 2 1 3 2 2 1 1 1 1 2 1 2 4 4 2 2 4 4 1
 [451] 4 1 1 1 1 4 2 3 1 4 3 4 4 3 4 4 4 3 4 4 4 1 2 1 1 4 3 3 3 1 4 1 4 4 4 1 1 2 1 4 2 2 4 1 1
 [496] 2 4 2 1 4 4 4 1 1 4 4 3 1 2 1 1 3 3 2 3 1 1 3 1 3 3 3 3 2 2 1 3 2 2 1 1 4 4 4 4 4 4 4 1 1
 [541] 1 4 1 1 4 1 1 3 1 4 4 3 3 4 3 3 4 1 4 3 1 1 4 1 4 3 3 1 1 1 4 2 4 2 3 3 3 2 2 2 4 1 4 3 1
 [586] 3 4 1 1 1 4 1 4 2 2 1 4 3 1 3 4 3 2 2 3 3 1 1 4 1 2 3 4 4 4 4 3 1 1 3 4 4 1 3 1 1 4 4 4 2
 [631] 4 4 1 4 1 1 1 4 1 1 1 4 4 4 4 4 3 4 1 4 4 4 4 4 1 3 3 3 1 4 3 1 4 3 4 3 1 4 1 4 4 2 1 1 4
 [676] 4 4 2 2 2 1 3 4 4 3 4 1 4 4 1 1 1 1 1 4 4 2 4 4 4 4 2 2 2 2 3 1 1 4 2 4 3 3 4 1 1 4 4 1 1
 [721] 4 1 2 2 3 2 1 3 1 1 1 4 3 4 4 1 4 4 3 1 2 2 1 4 3 3 4 3 4 3 3 4 4 4 1 2 4 4 1 1 4 4 4 4 1
 [766] 3 4 2 1 1 1 4 4 4 3 1 3 4 4 4 2 4 4 4 4 4 4 4 1 4 4 1 1 1 3 4 4 4 1 2 4 4 4 4 3 4 4 4 4 4
 [811] 4 1 1 4 4 3 3 4 3 4 2 4 1 1 3 1 1 2 3 3 1 3 2 2 1 2 3 3 3 3 3 4 4 1 1 1 4 3 4 2 1 4 1 4 1
 [856] 1 4 4 4 4 3 4 1 2 1 2 1 4 4 1 4 4 2 3 3 1 2 1 1 1 1 3 3 3 4 2 1 1 1 1 1 2 1 2 2 1 4 3 3 4
 [901] 1 4 4 4 4 3 3 2 4 4 1 1 1 2 3 2 1 3 2 3 1 4 4 4 1 1 2 4 4 4 3 4 4 4 4 4 2 4 4 4 4 4 2 2 1
 [946] 1 4 3 2 3 3 3 1 3 3 3 3 1 1 4 4 3 4 2 4 3 1 4 1 2 2 2 4 1 1 3 1 4 1 3 3 1 2 2 1 1 3 3 1 3
 [991] 3 3 4 1 3 3 2 1 4 3
 [ reached getOption("max.print") -- omitted 3898 entries ]

Within cluster sum of squares by cluster:
[1]  7696.326  6407.179  8374.791 14270.456
 (between_SS / total_SS =  31.8 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
[7] "size"         "iter"         "ifault"      


#visualisation
plotcluster(data.train, fit.km_bestNC4$cluster)

 
Next, I will work on data where outliers has been removed.
#Best number of clusters k = 2 (without outliers)
> set.seed(1234)
> fit.km_bestNC_noOutliers <- kmeans(data.withoutOutliers, 2)
> fit.km_bestNC_noOutliers
K-means clustering with 2 clusters of sizes 2954, 1942

Cluster means:
  fixed acidity volatile acidity citric acid residual sugar  chlorides free sulfur dioxide
1     -0.112356      -0.02959231  -0.1513890     -0.5701911 -0.2583746          -0.4045070
2      0.170906       0.04501323   0.2302796      0.8673247  0.3930167           0.6153005
  total sulfur dioxide    density         pH   sulphates    alcohol
1           -0.5081607 -0.6393329  0.1308788 -0.03951066  0.5214037
2            0.7729695  0.9724971 -0.1990814  0.06010014 -0.7931136

Clustering vector:
   [1] 2 1 1 2 2 1 1 2 1 1 1 1 1 1 2 1 1 1 1 2 1 1 1 2 1 2 1 2 1 1 2 1 1 1 2 1 1 1 2 2 2 2 2 1 1
  [46] 1 2 2 2 2 1 1 1 1 2 1 2 2 1 1 2 2 1 1 1 1 1 2 1 2 2 2 2 1 1 2 1 1 2 1 1 2 2 2 2 2 2 2 2 2
  [91] 2 2 1 1 1 2 2 1 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 1 1 1 2 2 1 2 2 1 1 1 1 2 1 1 1 2 2 2 2
 [136] 2 1 2 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 2 2 1 1 1 1 2 1 2 2 2 2 1 2 2 1 1 1 1 2 1 1 2 1 2
 [181] 2 2 2 2 2 2 2 1 1 2 2 2 1 1 2 2 2 2 2 2 2 2 2 1 1 2 2 2 1 1 1 1 2 1 1 2 2 2 2 1 2 2 1 1 1
 [226] 2 1 2 1 2 2 2 2 2 2 2 2 2 1 2 2 1 1 2 2 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 2 1 2 1 2 2 1 1 1
 [271] 2 1 2 2 2 2 2 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 2 1 2 1 2 2 2 1 1 1 1 1 2 2 2 2 1 1 1 1 1 2 2
 [316] 2 1 1 1 1 1 1 1 2 1 2 2 1 2 1 1 1 1 2 1 1 1 2 1 1 2 1 2 1 1 1 1 2 2 2 1 1 1 1 2 2 2 1 1 1
 [361] 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 1 2 1 2 2 1 1 1 2 2 1 1 2 1 1 2 1 2 1
 [406] 2 1 1 1 1 2 2 1 1 2 2 1 2 1 1 1 2 2 2 2 2 2 2 1 2 2 1 2 1 1 1 1 1 2 1 1 1 1 2 2 1 1 2 2 1
 [451] 2 1 1 1 1 2 1 2 1 2 2 2 2 1 2 2 2 1 2 2 2 2 1 1 1 2 1 1 1 1 2 1 2 2 2 1 1 1 1 2 1 1 2 1 1
 [496] 1 2 1 1 2 2 2 1 1 2 2 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 2 2 2 2 2 2 2 1 2
 [541] 1 2 1 2 2 1 2 1 1 2 2 1 1 2 1 1 2 1 2 1 2 1 2 1 2 2 1 2 2 2 2 1 2 1 1 2 2 1 1 1 2 1 2 1 1
 [586] 1 2 1 1 1 2 1 2 1 1 1 2 1 1 1 2 1 1 1 1 1 2 2 2 1 1 1 2 2 2 2 1 1 1 1 2 2 1 1 1 2 2 2 2 1
 [631] 2 2 1 2 1 1 1 2 2 2 1 2 2 2 2 2 1 2 2 2 2 2 2 2 1 1 1 1 1 2 1 1 2 1 2 1 1 2 1 2 2 1 2 1 2
 [676] 2 2 1 1 1 1 1 2 2 1 2 1 2 2 1 1 2 2 2 2 2 1 2 2 2 2 1 1 1 1 1 2 2 2 1 2 1 1 2 1 1 2 2 1 1
 [721] 2 1 1 1 1 1 1 2 1 2 2 2 1 2 2 1 2 2 1 1 1 1 1 2 2 1 2 1 2 1 1 2 2 2 1 1 2 2 1 1 2 2 2 2 2
 [766] 1 2 1 2 2 1 2 2 2 2 1 1 2 2 2 1 2 2 2 2 2 2 2 1 2 2 1 1 1 2 2 2 2 1 1 2 2 2 2 1 2 2 2 2 2
 [811] 2 1 1 2 2 1 1 2 1 2 1 2 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 2 1 2 1 1 2 1 2 1
 [856] 1 2 2 2 2 1 2 1 1 1 1 1 2 2 1 2 2 1 1 1 1 1 1 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2
 [901] 1 2 2 2 2 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 2 2 2 2 1 1 1 2 2 2 1 2 2 2 2 2 1 2 2 2 2 2 1 1 2
 [946] 1 2 1 1 1 1 2 1 1 2 1 1 1 1 2 2 1 2 1 2 1 1 2 1 1 1 1 2 1 1 1 1 2 1 1 1 1 1 1 1 1 2 2 1 1
 [991] 1 1 2 1 2 1 1 2 2 2
 [ reached getOption("max.print") -- omitted 3896 entries ]

Within cluster sum of squares by cluster:
[1] 23855.02 18450.69
 (between_SS / total_SS =  21.4 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
[7] "size"         "iter"         "ifault"     

#visualisation
plotcluster(data.withoutOutliers, fit.km_bestNC_noOutliers$cluster)

 

#Best number of clusters k = 5 (without outliers)
> set.seed(1234)
> fit.km_bestNC_noOutliers5 <- kmeans(data.withoutOutliers, 5)
> fit.km_bestNC_noOutliers5
K-means clustering with 5 clusters of sizes 1158, 107, 1129, 1514, 988

Cluster means:
  fixed acidity volatile acidity citric acid residual sugar   chlorides free sulfur dioxide
1    -0.5085470      -0.36711900  -0.2716100     -0.5002851 -0.05467898          0.03255074
2    -0.1911338       0.39100609   0.8864972     -0.3508633  5.35030554          0.29501676
3     0.9096974      -0.17682062   0.1962854     -0.4837153 -0.20444970         -0.58001530
4     0.1640645       0.06602454   0.2408262      1.1393340  0.14636996          0.68107671
5    -0.6741834       0.48882133  -0.3709999     -0.5687902 -0.50601704         -0.45098522
  total sulfur dioxide    density         pH    sulphates    alcohol
1           0.06635654 -0.1940627  0.8516221  0.516275187 -0.1054911
2           0.13669038  0.1280916 -0.6076100 -0.221655371 -0.7699928
3          -0.51210667 -0.3598259 -0.6630591 -0.289931112  0.2515583
4           0.83129728  1.1411537 -0.2959265  0.001158729 -0.8684948
5          -0.78125748 -1.1239317  0.2788080 -0.251570477  1.2504451

Clustering vector:
   [1] 4 1 3 4 4 3 1 4 1 3 3 3 3 1 4 1 5 5 3 1 5 5 1 3 1 4 1 1 3 5 3 3 3 1 4 3 1 3 4 4 2 2 4 1 1
  [46] 1 1 4 4 1 1 1 1 1 2 1 4 4 5 1 3 4 5 5 3 1 5 1 1 3 4 4 1 3 1 3 5 5 3 3 1 4 4 4 4 4 4 4 4 4
  [91] 4 4 5 5 3 4 4 3 3 4 4 4 4 4 4 4 4 4 4 1 4 4 4 4 4 1 5 3 4 4 5 4 3 1 1 1 3 1 1 1 5 1 4 4 4
 [136] 4 3 4 3 3 5 4 3 3 3 3 1 5 5 3 3 4 3 3 3 4 4 5 5 5 5 4 5 4 4 4 4 5 4 3 3 3 3 5 4 3 3 4 5 1
 [181] 1 4 4 4 4 4 4 5 5 4 4 4 3 1 2 2 2 4 4 4 4 4 4 1 1 4 4 3 5 1 5 1 4 1 5 4 3 4 4 3 3 4 5 1 1
 [226] 4 1 4 1 4 4 4 4 4 4 4 4 4 3 1 4 3 3 1 4 1 5 5 5 1 1 4 4 5 1 1 5 5 5 5 1 4 3 4 1 4 4 1 1 1
 [271] 4 1 4 4 4 4 4 3 3 5 5 5 4 4 4 4 4 4 4 4 4 1 4 1 4 1 4 1 4 3 5 3 5 1 4 4 4 4 5 1 5 5 1 1 1
 [316] 2 3 3 1 1 1 3 3 3 3 4 4 1 4 3 5 3 5 4 5 5 5 4 1 1 4 3 4 1 1 1 5 4 4 4 1 3 1 3 1 4 4 1 3 1
 [361] 4 3 1 4 1 1 1 1 3 3 1 3 5 3 5 5 1 3 1 1 3 4 4 3 5 5 1 4 1 4 4 5 5 3 4 1 3 1 4 5 1 4 1 4 1
 [406] 1 5 3 1 3 4 4 3 3 4 4 3 4 3 1 5 4 4 4 1 4 4 4 3 4 4 5 1 2 5 5 1 1 1 1 5 1 5 4 4 5 5 4 4 1
 [451] 4 1 1 1 1 4 5 3 1 4 3 4 4 3 4 4 4 3 4 4 4 1 5 1 1 4 3 3 3 1 4 1 4 4 2 1 1 5 1 4 5 5 4 1 1
 [496] 5 4 5 1 4 4 4 1 1 4 4 3 1 5 1 1 3 3 5 3 1 1 3 1 3 3 3 3 5 5 2 3 5 5 1 1 2 4 4 4 4 4 4 1 1
 [541] 1 4 1 1 4 1 1 3 1 4 4 3 3 4 3 3 4 1 4 3 1 1 4 3 4 3 3 1 1 1 4 5 4 5 3 3 3 5 5 5 4 1 4 3 1
 [586] 3 4 1 1 1 4 1 4 5 5 1 4 3 1 3 2 3 5 5 3 3 1 1 4 1 5 3 4 4 4 4 3 1 1 3 4 4 1 3 1 1 4 4 4 5
 [631] 4 4 1 4 1 1 1 4 1 1 1 4 4 4 4 4 3 4 1 4 4 4 4 4 1 3 3 3 1 4 3 1 4 3 4 3 1 4 1 4 4 5 1 1 4
 [676] 4 4 5 5 5 1 3 4 2 3 4 1 2 4 1 1 1 1 1 4 4 5 4 4 4 4 5 5 5 5 3 1 1 4 5 4 3 3 4 1 1 4 4 1 1
 [721] 4 1 5 5 3 5 1 3 1 2 1 4 3 4 4 1 4 4 3 1 5 5 1 4 3 3 4 3 4 3 3 4 4 4 2 5 4 4 1 1 4 4 4 4 1
 [766] 3 4 3 1 1 1 2 2 4 3 2 3 4 4 4 5 4 4 4 4 4 4 4 1 4 4 1 1 1 3 4 4 4 1 5 4 4 4 4 3 4 4 4 4 4
 [811] 4 1 1 4 4 3 3 4 3 4 5 4 1 1 3 1 1 5 3 3 1 3 5 5 1 5 3 3 3 3 3 4 4 1 1 1 4 3 4 5 1 4 1 4 1
 [856] 1 4 4 4 2 3 4 1 5 1 5 1 4 4 1 4 4 5 3 3 1 5 2 2 1 1 3 3 3 4 5 1 1 1 1 1 5 1 5 5 1 4 3 3 4
 [901] 1 4 4 4 4 3 3 5 4 4 1 1 1 5 3 5 1 3 3 3 1 4 4 4 1 1 5 4 4 4 3 4 4 4 4 4 5 4 4 4 4 4 5 5 1
 [946] 1 4 3 5 3 3 4 1 3 4 3 3 1 1 4 4 3 4 5 4 3 1 4 1 5 5 5 4 1 1 3 1 4 1 2 2 1 5 5 1 1 3 3 1 3
 [991] 3 3 4 1 3 3 5 1 4 3
 [ reached getOption("max.print") -- omitted 3896 entries ]

Within cluster sum of squares by cluster:
[1]  7652.350  1362.525  7398.658 10735.938  6281.353
 (between_SS / total_SS =  37.9 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
[7] "size"         "iter"         "ifault"      
> #visualisation
> plotcluster(data.withoutOutliers, fit.km_bestNC_noOutliers5$cluster)

 
The visualisation is very different for the 5 clusters, yet because the data us so dense it is difficult to judge which clustering is best. Therefore, I provided the table below, that shows sum of squares by cluster.
2 clusters (incl.outliers)	4 clusters (incl. outliers)	2 clusters(no outliers)	5 clusters (no outliers)
23413.09 19126.87
 (between_SS / total_SS =  	7696.326  6407.179  8374.791 14270.456
 (between_SS / total_SS =  	23855.02 18450.69
 (between_SS / total_SS =  	7652.350  1362.525  7398.658 10735.938  6281.353  (between_SS / total_SS =  
21.0 %	31.8 %	21.4 %	37.9 %
The sum of squares by cluster,  means variance in the data, typically the objective is to maximise the received percentage (closer to  100%), this could be achieved by using the same number of clusters and classes  in the original data. However, that is not the point in the clustering, so we cannot judge what is the best cluster only on these indications. Let’s check the accuracy and sensitivity.

4.	Check consistency of results against 12th column

confuseTable.km_2 <- table(Whitewine$quality, fit.km_bestNC$cluster)
confuseTable.km_2
   
       1    2
  3    8   12
  4  110   53
  5  609  848
  6 1338  860
  7  725  155
  8  147   28
  9    4    1
confuseTable.km_4<- table(Whitewine$quality, fit.km_bestNC4$cluster)
confuseTable.km_4
   
      1   2   3   4
  3   3   3   5   9
  4  28  37  62  36
  5 286  96 356 719
  6 583 425 519 671
  7 227 365 166 122
  8  37  84  31  23
  9   0   4   1   0
confuseTable.km_noO2<- table(Whitewine_outlierFree$quality, fit.km_bestNC_noOutliers$cluster)
confuseTable.km_noO2
   
       1    2
  3    8   11
  4  112   51
  5  612  845
  6 1345  852
  7  726  154
  8  147   28
  9    4    1
confuseTable.km_noO5<- table(Whitewine_outlierFree$quality, fit.km_bestNC_noOutliers5$cluster)
confuseTable.km_noO5
   
      1   2   3   4   5
  3   3   1   5   7   3
  4  29   4  61  34  35
  5 283  50 350 679  95
  6 579  48 506 654 410
  7 225   2 171 121 361
  8  39   2  35  19  80
  9   0   0   1   0   4


This output provides the data to do calculations, I have fitted this into the tables to ease the calculations and visualise what numbers are used for accuracy counting and what for sensitivity. The numbers highlighted in yellow are numbers to count sensitivity. To count sensitivity the highest number in the row has to be selected and then divided by total amount of that row. To count the accuracy the numbers were highlighted in green, all these numbers has to be added up and divided by total amount of numbers in the table (excluding top row and left column those are headers.)




DATA WITH OUTLIERS:						
k=2				k=4				
 	1	2		 	1	2	3	4
3	8	12		3	3	3	5	9
4	110	53		4	28	37	62	36
5	609	848		5	289	96	356	719
6	1338	860		6	583	425	519	671
7	725	155		7	227	365	166	122
8	147	28		8	37	84	31	23
9	4	1		9	0	4	1	0
								
Sum: 	3184			Sum: 	2246			
Total: 	4898			Total: 	4901			
Accuracy	0.650061	65%		Accuracy:	0.458274	46%		
								
Sensitivity:				Sensitivity:				
Quality 3:	0.4	40%		Quality 3:	0.45	45%		
Quality 4:	0.674847	67%		Quality 4:	0.380368	38%		
Quality 5:	0.582018	58%		Quality 5:	0.492466	49%		
Quality 6:	0.608735	61%		Quality 6:	0.305278	31%		
Quality 7:	0.823864	82%		Quality 7:	0.414773	41%		
Quality 8: 	0.84	84%		Quality 8: 	0.48	48%		
Quality 9:	0.8	80%		Quality 9:	0.8	80%		
DATA WITHOUT OUTLIERS:							
k=2				k=5					
 	1	2		 	1	2	3	4	5
3	8	11		3	3	1	5	7	3
4	112	51		4	29	4	61	34	35
5	612	845		5	283	50	350	679	95
6	1345	852		6	579	48	506	654	410
7	726	154		7	225	2	171	121	361
8	147	28		8	39	2	35	19	80
9	4	1		9	0	0	1	0	4
									
Sum: 	3190			Sum: 	2224				
Total: 	4896			Total: 	4896				
Accuracy	0.651552	65%		Accuracy:	0.454248	45%			
									
Sensitivity:				Sensitivity:					
Quality 3:	0.421053	42%		Quality 3:	0.368421	37%			
Quality 4:	0.687117	69%		Quality 4:	0.374233	37%			
Quality 5:	0.579959	58%		Quality 5:	0.466026	47%			
Quality 6:	0.612198	61%		Quality 6:	0.297679	30%			
Quality 7:	0.825	83%		Quality 7:	0.410227	41%			
Quality 8: 	0.84	84%		Quality 8: 	0.457143	46%			
Quality 9:	0.8	80%		Quality 9:	0.8	80%			



   Wine Quality	Data with outliers	Data without outliers
	2 clusters 	4 clusters	2 clusters 	5 clusters
Quality 3	40%	45%	42%	37%
Quality 4	67%	38%	69%	37%
Quality 5	58%	49%	58%	47%
Quality 6	61%	31%	61%	30%
Quality 7	82%	41%	83%	41%
Quality 8	84%	48%	84%	46%
Quality 9	80%	80%	80%	80%
Accuracy:	65%	46%	65%	45%

This proves that the NbClust and Elbow method suggestion of 2 clusters was correct and 2 clusters provides the best results, and outlier removal increases the performance we can see this in the table above.

5.	The mean of each attribute for the winner cluster
The winner cluster is k = 2 in data set where outliers has been removed. 
#calculate mean of each attribute
aggregate(Whitewine_outlierFree, by=list(cluster=fit.km_bestNC_noOutliers$cluster), mean)

cluster fixed acidity volatile acidity citric acid residual sugar  chlorides
1       1      6.759936        0.2751354   0.3158395       3.528318 0.04012119
2       2      6.998970        0.2826210   0.3620134      10.717714 0.05435324

  free sulfur dioxide total sulfur dioxide   density       pH sulphates   alcohol  quality
1            28.54096             116.8064 0.9921505 3.207932 0.4852674 11.155748 6.058226
2            45.48507             170.9807 0.9968597 3.158120 0.4966323  9.537899 5.605046


Conclusion 
K-means is very sensitive to the outliers or/and noisy data. Great attention should be paid when pre-processing the data. Through the analysis we have seen what the big difference in performance is when incorrect number of clusters has been chosen and that removal of outliers definitely increases performance. The big problem is to understand how many outliers should be removed. Especially in this data set, where we had 2 classes that contained small amount of data, it is very likely that if I was stricter with my outlier removal criteria those two classes (quality 3 and 9) were completely removed. This would increase the classification performance, however I did not know where is the line for wine criteria.  If the goal was simply to achieve the highest performance, more outliers that appeared distant should have been removed. However this is not how the real data analysis works, and when training it is desirable to have as diverse data as possible. 

Code

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
Objective 2
First of all the data provided is not a time-series data, we can only use the last column for our analysis, yet the exchange rate needs to have timing index, therefore I have created the date set, which has been converted into the date object and then transformed into understandable values by R. Finally, exchange rate needs to be turned into time series object by combining date string vector with exchange rate values.10, 11
#load the data
library(readxl)
ExchangeUSD <- read_excel("ExchangeUSD.xlsx")

#library(xts)
#exchangeUSD_xts <- as.xts(ExchangeUSD)
install.packages("rmetrics")
install.packages(c("quadprog","Rglpk","fBasics"), repos = "http://cran.r-project.org")
update.packages()
install.packages("slam")
library("slam")
library(fBasics)
library(quadprog)
library(Rglpk)

#create date as a separate vector
date <- as.Date(ExchangeUSD$`YYYY/MM/DD`,"%d/%m/%y")
#turn date into understandable format
ts_date <- fix(date)
ts_date
#combine exchange rate and date to timeSeries data
ts_data = as.timeSeries(ExchangeUSD$`USD/EUR`, ts_date)
ts_data
#view specific day
ts_data["2011-10-13",]
plot(ts_data)
summary(ts_data)
 

Anoher way is to store as Extensible time series, it provides slightly easier manipulation.

#another time series data format XTS
usd <- ExchangeUSD$`USD/EUR`
names(usd) <- ts_date
xts_data <- as.xts(usd)

It also, provides nicer plotting functionality. 

 

One of the problems when working with financial data, is that data is noisy, not stationary and not even seasonal, which means that data cannot be modelled or forecasted. MLP are not bad in handling noisy data, but the forecasting performance will be poor if data is not pre-processed. 

1.	Pre-processing

Normalizing data,  as 0 – 1 range is has more accurate prediction. 

#Normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
norm_ts_data <- sapply(ts_data2, normalize)
norm_ts_data
class(norm_ts_data)
ts_norm_data <- ts(norm_ts_data, start = decimal_date(as.Date("2011-10-13")), frequency = 365)
class(ts_norm_data)
plot(ts_norm_data)

 

2.	Input

There can be many combinations of inputs for MLP models. Such as detrended data, de-seasonalised data, or both of action performed on the data. The reason why is this important is that very anomaly in the data has an impact on the forecasting model performance. Every model suffer more or less from noisy or not processed, or poorly processed data. 

The are 3 problems with the data: 
•	Non-stationary or stationary?
•	There is trend component
•	There seems to be monthly seasonal trends

2.1.	Stationarity
Although in the plotting we could see that data is non-stationary, and this is the general fact about the financial data, in my research I found the unit root test, called Augmented Dickey-Fuller (ADF), the principle is to check the hypothesis of unit root. More negative result mean more likelihood that data is stationary. 
install.packages("tseries")
library(tseries)
adf.test(xts_data)

Augmented Dickey-Fuller Test

data:  xts_data
Dickey-Fuller = -2.9261, Lag order = 7, p-value = 0.1863
alternative hypothesis: stationary

By standard results support that data is non-stationary as Dickey-Fuller = -2.9261 is not that far on negativity scale and p-value = 0.1863. Read below:
•	“Null Hypothesis (H0): If failed to be rejected, it suggests the time series has a unit root, meaning it is non-stationary. It has some time dependent structure.
•	Alternate Hypothesis (H1): The null hypothesis is rejected; it suggests the time series does not have a unit root, meaning it is stationary. It does not have time-dependent structure.
•	p-value > 0.05: Fail to reject the null hypothesis (H0), the data has a unit root and is non-stationary.
•	p-value <= 0.05: Reject the null hypothesis (H0), the data does not have a unit root and is stationary”
(Brownlee, 2019)

2.2.	Detrending

From the data plot it is difficult to note the trend pattern, however, there are dedicated methods to detect seasonality such as Fourier Transforms.13
#Detect the trend using Fourier Transforms
install.packages("TSA")
library(TSA)

# compute the Fourier Transform
#requires matrix component so use xts
p = periodogram(xts_data)

 
Here we are looking for the biggest spike, however it is impossible to extract frequency number out this plot, for that we can use the following function. 

#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
   freq       spec
1 0.002 0.25822817
2 0.004 0.11349405
3 0.006 0.06342945
4 0.008 0.05061843

The frequencies that indicates the seasonality are above and they need to be converted to the time periods.
#convert to time
> freq_time = 1/top4_freq$f
> freq_time
[1] 500.0000 250.0000 166.6667 125.0000

#Detrend the data
install.packages("forecast")
library(forecast)
seasonality_ts_data = ma(ts_data2, order = 250, centre = T)
plot(as.ts(ts_data2))
lines(seasonality_ts_data)
 

detrend_ts_data = ts_data2-seasonality_ts_data
plot(as.ts(detrend_ts_data))
 

So we lost, seasonality and trend alternative method is to use decomposition function, that automatically removes trend and seasonality.
#Normalized
frequency_ts_normalized_data = ts(ts_norm_data, frequency = 365)
decompose_data_norm = decompose(ts_norm_data, "additive")
plot(as.ts(decompose_data_norm))
decompose_data_norm
plot(decompose_data_norm)

$figure
  [1] -0.1263175862 -0.1205329988 -0.1314750231 -0.1267512680 -0.1247776265 -0.0432324729 -0.0537200403
  [8] -0.0145120938 -0.0310140739 -0.0009186380 -0.0157365306 -0.0230039460  0.0046452132 -0.0387372447
 [15] -0.0273978953 -0.0328513784 -0.0404732702 -0.0594384069 -0.0677679530 -0.0652593508 -0.0561494372
 [22] -0.0264941752  0.0110972031  0.0208031045 -0.0399357126 -0.0197954802 -0.0421924158 -0.0341121717
 [29]  0.0083561786 -0.0269096787 -0.0994461975 -0.0881821581 -0.0945666286 -0.1481899488 -0.1303038227
 [36] -0.1694780095 -0.1262825281 -0.1484340571 -0.1140420555 -0.1237882088 -0.1170518588 -0.1492040369
 [43] -0.1155015114 -0.0694221763 -0.0963727703 -0.0451061339 -0.0568064520 -0.0509413608  0.0233363773
 [50]  0.0209108758  0.0170402012  0.0377595416  0.0736915029  0.0546341764  0.0613432591  0.0664046107
 [57]  0.0978582237  0.0957508420  0.0059553476 -0.0421651484 -0.0534733351 -0.0586580394 -0.0945562410
 [64] -0.0897078349 -0.0928020373 -0.0902233189 -0.0960247861 -0.1383139506 -0.1795461781 -0.1647945063
 [71] -0.2074745037 -0.1737265325 -0.0792046862 -0.0787567215 -0.0826040239 -0.0333460872 -0.0566934870
 [78] -0.0654281491 -0.0348990314 -0.0109166903 -0.0021210012  0.0057501928  0.0083821476  0.0251918600
 [85]  0.0209264571  0.0223820177  0.0346731299 -0.0024040629  0.0220768824  0.0332331506  0.0482068571
 [92]  0.0779062663  0.0527799922  0.0398941907  0.0082458105  0.0117269505  0.0152613270  0.0426456028
 [99]  0.0554418113  0.0914984236  0.0650801934  0.0619626189  0.0764714808  0.0659618398  0.0754158423
[106]  0.0479030201  0.0002213997 -0.0134980056 -0.0285885724 -0.0048620256 -0.0482808401 -0.0251892346
[113]  0.0198136869  0.0216886463  0.0410472128  0.0487028643  0.0311465441  0.0670733116  0.0712789859
[120]  0.0693118366  0.1537005920  0.1530890229  0.1391462844  0.1384542115  0.1609459339  0.1372375654
[127]  0.1628715328  0.1620755839  0.1627637616  0.1912127650  0.2155794468  0.1892754800  0.1838206985
[134]  0.1903350139  0.1585879516  0.2629143816  0.3257333129  0.2834558345  0.2610692864  0.2890664299
[141]  0.2422989170  0.3387255872  0.3687872635  0.3605200428  0.3276757917  0.4873524780  0.4856826735
[148]  0.3846491086  0.2575595805  0.3189891722  0.2935772394  0.3021573862  0.2959806673  0.3183879906
[155]  0.2287158488  0.2319918340  0.2454463559  0.1988255677  0.1917840823  0.1999331441  0.2010952554
[162]  0.2039726170  0.1926410582  0.1215769798  0.1191683581  0.0740278010  0.1180711692  0.1227040329
[169]  0.1761247951  0.1924397987  0.1446594961  0.1822586651  0.1515425709  0.1473420904  0.1186437849
[176]  0.1396215165  0.0547172771  0.0106661182 -0.0467330897 -0.0268239811 -0.0205511771 -0.0247334792
[183] -0.1183055107 -0.1599422048 -0.3524488594 -0.3441284023 -0.3633766007 -0.3673939999 -0.3897792496
[190] -0.3711205467 -0.3539161060 -0.3679757048 -0.3542524041 -0.3547497099 -0.3917308174 -0.4148535857
[197] -0.4443738091 -0.4102570785 -0.3387606168 -0.2961351542 -0.3511101591 -0.3197513329 -0.3265655899
[204] -0.3964350958 -0.2819963501 -0.2658981874 -0.2929786263 -0.3331461261 -0.3231350893 -0.3026338883
[211] -0.3073096008 -0.3269940778 -0.2939875207 -0.3135174824 -0.2997876895 -0.2381659275 -0.2369194171
[218] -0.1840232280 -0.2065097566 -0.2147977524 -0.1904115939 -0.2021534624 -0.2205862352 -0.1825858457
[225] -0.1873875077 -0.1696571954 -0.1579010440 -0.0787216634 -0.0792890853 -0.0485587081 -0.0324618439
[232] -0.0241998170  0.0845491280  0.0762403570  0.0416587820  0.0501415451 -0.0085963631  0.0114347997
[239] -0.0284340571 -0.0117113407 -0.0561961813 -0.0468538454 -0.0558144375 -0.0359598637 -0.0121294411
[246] -0.0319814180  0.0107037732  0.0383139790 -0.0496117497 -0.0435389068 -0.0212250718 -0.0157469182
[253] -0.0136694008  0.0273524780  0.0741953009  0.0625365331  0.0218756229  0.0408498487 -0.0022015050
[260] -0.0139550595 -0.0087041343 -0.0181983887 -0.0332967461  0.0018301772 -0.0080328365 -0.0175725366
[267] -0.0612510408 -0.0879380498 -0.0749003299 -0.1031013297 -0.1121917666 -0.1245400104 -0.1236427826
[274] -0.1128422892 -0.0925696151 -0.1246373941 -0.0809095488 -0.0773258314 -0.0752963566 -0.0486833591
[281] -0.0080795807 -0.0049191574 -0.0176374590 -0.0259799897 -0.0114243836  0.0147484399  0.0409913795
[288]  0.0477381172  0.0454476543 -0.0068889035 -0.0192449381 -0.0243556308  0.0050412400  0.0264604440
[295]  0.0454528481  0.0724722599  0.0886717016  0.1147847964  0.1266136609  0.1161728377  0.0877173420
[302]  0.0938369291  0.1170427981  0.1127345465  0.0955521794  0.0997643459  0.0589320405  0.0285132909
[309]  0.0527501278  0.0369934571  0.0401772524  0.1078887372  0.1703103434  0.1772323716  0.1639531402
[316]  0.1463202116  0.1695312744  0.1444932948  0.1490586392  0.1430494202  0.1442452911  0.1767818100
[323]  0.2134162318  0.2136837122  0.2249906005  0.2617626579  0.2700779211  0.3196396948  0.2398669233
[330]  0.2591268077  0.2373180692  0.1669473621  0.1579737856  0.1793787067  0.1960390975  0.1928345270
[337]  0.1372245809  0.1491806933  0.1596254119  0.1406849457  0.0706180756  0.0507089670  0.0529968330
[344] -0.0048308629  0.0170531857  0.0042998260 -0.0403577083 -0.0370960060 -0.0271836513 -0.0420054393
[351]  0.0080549386 -0.0432947985 -0.0340082958 -0.0321398287 -0.0674108798 -0.0536343427 -0.0132902539
[358] -0.0649607077 -0.0996773214 -0.0712127365 -0.0829208453 -0.0508881244 -0.1061435942 -0.1175076142
[365] -0.1540407570

$type
[1] "additive"

attr(,"class")
[1] "decomposed.ts"

plot(decompose_data_norm)
 
At the moment we have 5 inputs:
•	ts_norm_data – data without trend and seasonality removal
•	detrend_ts_norm_data – de-trended and normalised data
•	detrend_ts_data – de-trended, not normalised
•	decompose_data_norm – normalised data with trend, seasonality removed
•	decompose_data – data with trend and seasonality removed, but not normalised.
In addition to that, there is lag factor, in other works how much past data should be used to get an output/forecast. This is called LAGS.  The input needs to be a set of previous day values for example 5, so if our goal is to achieve this: (Output:) t k+1  = tk, tk -1, t
2.3.	Lag values
#PREPARE find best lag values
a<-acf(ts_norm_data, lag.max = 20, plot = TRUE)
b<-acf(log(ts_norm_data))
c<-acf(diff(log(ts_norm_data)))
plot(a)
plot(b)
plot(c)

#lagged value up to 5
  plot(xts_data)
  xts_data %>%           
    lag.xts(k = 1:5)
  
library(timetk)
  
  xts_data  %>%
    tk_xts(silent = TRUE) %>%
    lag.xts(k = 1:5)
#convert to xts  
  lagged5_xts <- xts_data%>%
    tk_xts(silent = TRUE)

# Get original values and lags in xts
my_lagged_time_series_xts5 <- 
  merge.xts(lagged5_xts, lag.xts(lagged5_xts, k = 1:5))

my_lagged_time_series_xts5

             x    x.5    x.1    x.2    x.3    x.4
2011-10-13 1.3730     NA     NA     NA     NA     NA
2011-10-14 1.3860 1.3730     NA     NA     NA     NA
2011-10-17 1.3768 1.3860 1.3730     NA     NA     NA
2011-10-18 1.3718 1.3768 1.3860 1.3730     NA     NA
2011-10-19 1.3774 1.3718 1.3768 1.3860 1.3730     NA
2011-10-20 1.3672 1.3774 1.3718 1.3768 1.3860 1.3730
2011-10-21 1.3872 1.3672 1.3774 1.3718 1.3768 1.3860
2011-10-24 1.3932 1.3872 1.3672 1.3774 1.3718 1.3768
2011-10-25 1.3911 1.3932 1.3872 1.3672 1.3774 1.3718
2011-10-26 1.3838 1.3911 1.3932 1.3872 1.3672 1.3774
2011-10-27 1.4171 1.3838 1.3911 1.3932 1.3872 1.3672
2011-10-28 1.4164 1.4171 1.3838 1.3911 1.3932 1.3872
2011-10-31 1.3947 1.4164 1.4171 1.3838 1.3911 1.3932
2011-11-01 1.3675 1.3947 1.4164 1.4171 1.3838 1.3911

X here is desired outcome and xk are lagged values.  The model above, allows to fit 5 prepared data  types, by changing the highlighted sections.  To change lag values or how much far back to look ,     in the parts highlighted in green the numbers should be changed. To look up to 4 values k = 1:4, up to 3 values k= 1 : 3 and two values k = 1 : 2

3.	MLP model14

Input is prepared but it needs to be reformatted in the way that MLP can interpretate.
#input_lag
#reformat
library(tseries)
#remove missing values
lagged_dates <-na.remove(my_lagged_time_series_xts5)
my_lagged_time_series_xts5
             x    x.5    x.1    x.2    x.3    x.4
2011-10-13 1.3730     NA     NA     NA     NA     NA
2011-10-14 1.3860 1.3730     NA     NA     NA     NA
2011-10-17 1.3768 1.3860 1.3730     NA     NA     NA
2011-10-18 1.3718 1.3768 1.3860 1.3730     NA     NA
2011-10-19 1.3774 1.3718 1.3768 1.3860 1.3730     NA
2011-10-20 1.3672 1.3774 1.3718 1.3768 1.3860 1.3730
2011-10-21 1.3872 1.3672 1.3774 1.3718 1.3768 1.3860
2011-10-24 1.3932 1.3872 1.3672 1.3774 1.3718 1.3768
2011-10-25 1.3911 1.3932 1.3872 1.3672 1.3774 1.3718
2011-10-26 1.3838 1.3911 1.3932 1.3872 1.3672 1.3774
2011-10-27 1.4171 1.3838 1.3911 1.3932 1.3872 1.3672
2011-10-28 1.4164 1.4171 1.3838 1.3911 1.3932 1.3872
2011-10-31 1.3947 1.4164 1.4171 1.3838 1.3911 1.3932
2011-11-01 1.3675 1.3947 1.4164 1.4171 1.3838 1.3911
2011-11-02 1.3801 1.3675 1.3947 1.4164 1.4171 1.3838
2011-11-03 1.3744 1.3801 1.3675 1.3947 1.4164 1.4171
2011-11-04 1.3759 1.3744 1.3801 1.3675 1.3947 1.4164
2011-11-07 1.3743 1.3759 1.3744 1.3801 1.3675 1.3947
2011-11-08 1.3787 1.3743 1.3759 1.3744 1.3801 1.3675

lagged_dates
  x    x.5    x.1    x.2    x.3    x.4
  [1,] 1.3672 1.3774 1.3718 1.3768 1.3860 1.3730
  [2,] 1.3872 1.3672 1.3774 1.3718 1.3768 1.3860
  [3,] 1.3932 1.3872 1.3672 1.3774 1.3718 1.3768
  [4,] 1.3911 1.3932 1.3872 1.3672 1.3774 1.3718

Model cannot understand NA values thus the data needed to be processed again to get rid of missing values and format it into the understandable input. The 1st column is the target value and  columns 2:6  are the input value.
rates<-lagged_dates
rates
rates <- rates[sample(1:nrow(rates),length(1:nrow(rates))),1:ncol(rates)]
rates
#prepare input
rateValues <- rates[,2:6]
#output
rateTargets <- decodeClassLabels(rates[,1])
rates <- splitForTrainingAndTest(rateValues, rateTargets, ratio = 0.2)


Model with 2 hidden layers 5  nodes and 10
#fit in model
model<- mlp(rates$inputsTrain, rates$targetsTrain, size = c(5, 10), learnFuncParams = c(0.1), inputsTest = rates$inputsTest, targetsTest = rates$targetsTest)
model
Class: mlp->rsnns
Number of inputs: 5 
Number of outputs: 393 
Maximal iterations: 400 
Initialization function: Randomize_Weights 
Initialization function parameters: -0.3 0.3 
Learning function: Std_Backpropagation 
Learning function parameters: 0.2 0 
Update function:Topological_Order
Update function parameters: 0 
Patterns are shuffled internally: TRUE 
Compute error in every iteration: TRUE 
Architecture Parameters:
$size
[1] 10 5

All members of model:
 [1] "nInputs"               "maxit"                 "initFunc"             
 [4] "initFuncParams"        "learnFunc"             "learnFuncParams"      
 [7] "updateFunc"            "updateFuncParams"      "shufflePatterns"      
[10] "computeIterativeError" "snnsObject"            "archParams"           
[13] "IterativeFitError"     "IterativeTestError"    "fitted.values"        
[16] "fittedTestValues"      "nOutputs"       

predictions <- predict(model,rates$inputsTest)
plotRegressionError(predictions[,2],rates$targetsTest[,2])
plotROC(fitted.values(model)[,2], rates$targetsTrain[,2])
plotROC(predictions[,2], rates$targetsTest)
 

cor(results, rates$targetsTest) 
          [,1]
[1,] 0.9769692

Model 2 same data, different amount internal structures 3 layers
#MODEL2 with 3 hidden layers: 7, 10, 5
#remove missing values
lagged_dates <-na.remove(my_lagged_time_series_xts5)
my_lagged_time_series_xts5
lagged_dates
rates<-lagged_dates
rates
rates <- rates[sample(1:nrow(rates),length(1:nrow(rates))),1:ncol(rates)]
rates

#prepare input
rateValues <- rates[,2:6]
rateValues
#output
rateTargets <- (rates[,5])
rateTargets
rates <- splitForTrainingAndTest(rateValues, rateTargets, ratio = 0.2)

#fit in model
model2<- mlp(rates$inputsTrain, rates$targetsTrain, size = c(7,10,5),linOut = T, inputsTest = rates$inputsTest, targetsTest = rates$targetsTest, maxit=400)
results2<-predict(model2, rates$inputsTest)
results2

model2

Class: mlp->rsnns
Number of inputs: 5 
Number of outputs: 1 
Maximal iterations: 400 
Initialization function: Randomize_Weights 
Initialization function parameters: -0.3 0.3 
Learning function: Std_Backpropagation 
Learning function parameters: 0.2 0 
Update function:Topological_Order
Update function parameters: 0 
Patterns are shuffled internally: TRUE 
Compute error in every iteration: TRUE 
Architecture Parameters:
$size
[1]  7 10  5

All members of model:
 [1] "nInputs"               "maxit"                 "initFunc"             
 [4] "initFuncParams"        "learnFunc"             "learnFuncParams"      
 [7] "updateFunc"            "updateFuncParams"      "shufflePatterns"      
[10] "computeIterativeError" "snnsObject"            "archParams"           
[13] "IterativeFitError"     "IterativeTestError"    "fitted.values"        
[16] "fittedTestValues"      "nOutputs"            

cor(results2, rates$targetsTest)   
          [,1]
[1,] 0.9835916


Accuracy
Model	Internal structure	Accuracy
Model 1	2 hidden layers of 5 nodes and 10	0.9769692

Model 2	3 hidden layers of 7, 10 and 5 nodes	0.9835916

		

Code

data("AirPassengers")
#load the data
library(readxl)
ExchangeUSD <- read_excel("ExchangeUSD.xlsx")

library(xts)
#exchangeUSD_xts <- as.xts(ExchangeUSD)
install.packages("rmetrics")
install.packages(c("quadprog","Rglpk","fBasics"), repos = "http://cran.r-project.org")
update.packages()
install.packages("slam")
library(slam)
library(fBasics)
library(quadprog)
library(Rglpk)

#create date as a separate vector
date <- as.Date(ExchangeUSD$`YYYY/MM/DD`,"%y/%m/%d")
#turn date into understandable format
ts_date <- fix(date)
ts_date
dataWeekdays <- isWeekday(ts_date)
dataWeekdays


#combine exchange rate and date to timeSeries data
ts_data = as.timeSeries(ExchangeUSD$`USD/EUR`, dataWeekdays)#change back to ts_date
ts_data

#ts_data2 <- ts(ExchangeUSD$`USD/EUR`, start=c(2011, 10,13), end = c(2013,10,9), frequency = 365)
ts_data2
rates<-ts(ExchangeUSD$`USD/EUR`, start=decimal_date(ymd("2011-10-13")))
rates
plot(rates)
plot(ts_data2)

experiment <- msts(ExchangeUSD$`USD/EUR`, seasonal.periods=c(48,336))
experiment

experiment.fit <- tbats(experiment)
plot(forecast(experiment.fit))
??tbats

#view specific day
ts_data["2011-10-13",]
ts_data2["2011-10-13",]
plot(ts_data)
plot(ts_data2)
summary(ts_data)

#another time series data format XTS
usd <- ExchangeUSD$`USD/EUR`
names(usd) <- ts_date
xts_data <- as.xts(usd)
plot(xts_data)
periodicity(xts_data)

class(ExchangeUSD)
class(xts_data)
as.xts(read.table("ExchangeUSD"))
library(lubridate)
#rates<-msts(ExchangeUSD$`USD/EUR`, seasonal.periods = c(12,365.25), start = decimal_date(as.Date("2011-10-13")))
         #   frequency = 500, start = c(2011,10,13))
#rates
ts_data2
#test if data is stationary
install.packages("tseries")
library(tseries)
adf.test(xts_data)

head(xts_data)
head(ts_data)

#Normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
norm_ts_data <- sapply(ts_data2, normalize)
norm_ts_data
class(norm_ts_data)
ts_norm_data <- ts(norm_ts_data, start = decimal_date(as.Date("2011-10-13")), frequency = 365)
class(ts_norm_data)
plot(ts_norm_data)

#Detect the trend using Fourier Transforms
install.packages("TSA")
library(TSA)

# compute the Fourier Transform
#p = periodogram(ts_data)
#requires matrix component so use xts
p = periodogram(xts_data)
#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
#convert to time
freq_time = 1/top4_freq$f
freq_time

#requires matrix component so use xts
p = periodogram(ts_data2)
#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
#convert to time
freq_time = 1/top4_freq$f
freq_time


# compute the Fourier Transform
#p = periodogram(ts_data)
#requires matrix component so use xts
p = periodogram(xts_data)
#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
#convert to time
freq_time = 1/top4_freq$f
freq_time

#requires matrix component so use xts NORMALIZED
p = periodogram(ts_norm_data)
#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
#convert to time
freq_time = 1/top4_freq$f
freq_time



#Detrend the data
install.packages("forecast")
library(forecast)
seasonality_ts_data = ma(ts_data2, order = 250, centre = T)
plot(as.ts(ts_data2))
lines(seasonality_ts_data)
plot(as.ts(seasonality_ts_data))
detrend_ts_data = ts_data2-seasonality_ts_data
plot(as.ts(detrend_ts_data))
detrend_ts_data


seasonality_xts_data = ma(xts_data, order = 250, centre = T)
plot(as.ts(xts_data))
lines(seasonality_xts_data)
plot(as.ts(seasonality_xts_data))
detrend_xts_data = xts_data-seasonality_xts_data
plot(as.ts(detrend_xts_data))

frequency_ts_data = ts(ts_data2, frequency = 365)
decompose_data = decompose(ts_data2, "additive")
plot(as.ts(decompose_data))
decompose_data

plot(as.ts(decompose_data$seasonal))
plot(as.ts(decompose_data$trend))
plot(as.ts(decompose_data$random))
plot(decompose_data)

#Normalized detrenting
#Detrend the data
seasonality_ts_norm_data = ma(ts_norm_data, order = 250, centre = T)
plot(as.ts(ts_norm_data))
lines(seasonality_ts_norm_data)
plot(as.ts(seasonality_ts_norm_data))
detrend_ts_norm_data = ts_data2-seasonality_ts_norm_data
plot(as.ts(detrend_ts_norm_data))
detrend_ts_norm_data

frequency_ts_normalized_data = ts(ts_norm_data, frequency = 250)
decompose_data_norm = decompose(ts_norm_data, "additive")
plot(as.ts(decompose_data_norm))
decompose_data_norm
plot(decompose_data_norm)


#PREPARE find best lag values
a<-acf(ts_norm_data, lag.max = 20, plot = TRUE)
b<-acf(log(ts_norm_data))
c<-acf(diff(log(ts_norm_data)))
plot(a)
plot(b)
plot(c)

#lagged value up to 5
plot(xts_data)
xts_data %>%           
  lag.xts(k = 1:5)

library(timetk)

xts_data  %>%
  tk_xts(silent = TRUE) %>%
  lag.xts(k = 1:5)
#convert to xts  
lagged5_xts <- xts_data%>%
  tk_xts(silent = TRUE)

# Get original values and lags in xts
my_lagged_time_series_xts5 <- 
  merge.xts(lagged5_xts, lag.xts(lagged5_xts, k = 1:5))

my_lagged_time_series_xts5

# Convert back to tbl
my_lagged_time_series_xts5 %>%
  tk_tbl() 

my_lagged_time_series_xts5




#Model fitting
values <-my_lagged_time_series_xts5[,2:6]
targets <- my_lagged_time_series_xts5[,1]
training <- my_lagged_time_series_xts5[1:400]
#testing <- my_lagged_time_series_xts5[401:500]

model_data <- splitForTrainingAndTest(values, targets, )
model_data <- normTrainingAndTestSet(model_data)
model_data$inputsTrain
training
testing
install.packages("RSNNS")
library(RSNNS)

model <- mlp(model_data$inputsTrain, model_data$targetsTrain, size=c(10,5,1), maxit = 500, learnFuncParams = c(0.1))
model_data$inputsTrain

#input_lag = lag(ts_norm_data, 1,2)
#input_lag

#reformat
library(tseries)
#remove missing values
lagged_dates <-na.remove(my_lagged_time_series_xts5)
my_lagged_time_series_xts5
lagged_dates
rates<-lagged_dates
rates
rates <- rates[sample(1:nrow(rates),length(1:nrow(rates))),1:ncol(rates)]
rates

#prepare input
rateValues <- rates[,2:6]
#output
rateTargets <- rates[,1]
rateTargets
class(rateTargets)
rateTargets
rates <- splitForTrainingAndTest(rateValues, rateTargets)
rates
#fit in model
model<- mlp(rates$inputsTrain, rates$targetsTrain, size = c(10,5), inputsTest = rates$inputsTest, targetsTest = rates$targetsTest)
results <- predict(model, rates$targetsTest)
results

results<-predict(model, rates$inputsTest)
results

cor(results, rates$targetsTest) 

library(nnet)
library(network)
plot.network(model)
install.packages("gmodels")
library(gmodels)

summary(model)
model
weightMatrix(model)
extractNetInfo(model)

par(mfrow=c(2,2))
plotIterativeError(model)

predictions <- predict(model,rates$inputsTest)
plotRegressionError(predictions[,2],rates$targetsTest[,2])


plotROC(fitted.values(model)[,2], rates$targetsTrain[,2])
plotROC(predictions[,2], rates$targetsTest)

confusionMatrix(rates$targetsTrain, encodeClassLabels(fitted.values(model))                                                     )
confusionMatrix(prediction, targets)  

library(neuralnet)
plotnet(model)
plot(model)




#lagged value up to 4
xts_data %>% lag.xts(k = 1:4)

xts_data  %>%
  tk_xts(silent = TRUE) %>%
  lag.xts(k = 1:4)
#convert to xts  
lagged4_xts <- xts_data%>%
  tk_xts(silent = TRUE)

# Get original values and lags in xts
my_lagged_time_series_xts4 <- 
  merge.xts(lagged4_xts, lag.xts(lagged4_xts, k = 1:4))

my_lagged_time_series_xts4

# Convert back to tbl
my_lagged_time_series_xts4 %>%
  tk_tbl() 

my_lagged_time_series_xts4





#MODEL2 with 3 hidden layers: 7, 10, 5
#remove missing values
lagged_dates <-na.remove(my_lagged_time_series_xts5)
my_lagged_time_series_xts5
lagged_dates
rates<-lagged_dates
rates
rates <- rates[sample(1:nrow(rates),length(1:nrow(rates))),1:ncol(rates)]
rates

#prepare input
rateValues <- rates[,2:6]
rateValues
#output
rateTargets <- (rates[,5])
rateTargets
rates <- splitForTrainingAndTest(rateValues, rateTargets, ratio = 0.2)

#fit in model
model2<- mlp(rates$inputsTrain, rates$targetsTrain, size = c(7,10,5),linOut = T, inputsTest = rates$inputsTest, targetsTest = rates$targetsTest, maxit=400)
results2<-predict(model2, rates$inputsTest)
results2
model2

cor(results2, rates$targetsTest)   

library(nnet)
library(network)
plot.network(model)
install.packages("gmodels")
library(gmodels)

summary(model)
model
weightMatrix(model)
extractNetInfo(model)

par(mfrow=c(2,2))
plotIterativeError(model)

predictions <- predict(model,rates$inputsTest)
plotRegressionError(predictions[,2],rates$targetsTest[,2])


plotROC(fitted.values(model)[,2], rates$targetsTrain[,2])
plotROC(predictions[,2], rates$targetsTest)

confusionMatrix(rates$targetsTrain, encodeClassLabels(fitted.values(model))                                                     )
confusionMatrix(prediction, targets)  

library(neuralnet)
plotnet(model)
plot(model)







head(stepBack)
plot(xts_data)
plot(ts_data)
plot(ts_data2)
#moving average with 2 lags
#MA2 <- arima.sim(ts_data,model = list(order = c(1,1,2), ma = c(1,1)))+500
#plot(MA2)
#acf(MA2)
#
#??numeric
#format the data
# install.packages('timeDate')
#require(timeDate)

# A ’timeDate’ Sequence
#dateRange <- timeSequence(as.Date("2011/10/13"), as.Date("2013/10/9"))
#dateRange
# Subset to weekdays only
#dataWeekdays <- dateRange[isWeekday(dateRange)]; dataWeekdays
#dayOfWeek(dataWeekdays)




#visualising
#plot(ExchangeUSD$`USD/EUR`~date, type="l", col="red")
#box()
#axis(1, date, format(date,"%m-%y"))

#only exchange rates needed
#tsData <-ExchangeUSD[,3]
#ts(ExchangeUSD, frequency = 365.25, start = c(2011,10,13))
#timeseries_data <- ts(tsData, start=c(2011, 10), end=c(2013, 10), frequency=12)



#decomposedRes <- decompose(tsData, type="mult")

#library(lubridate)
#n as in 500 data
#n <- 500
#head(ExchangeUSD)
#assign last column with rates to vector x
#x <- ExchangeUSD$`USD/EUR`
#dates <- date(("2013-10-09"):(n-1))

plot(xts_data)
class(xts_data)
acf(log(xts_data))
plot(ts_data2)
acf(log(ts_data2))
acf(diff(log(ts_data2)))
(fit <- arima(log(ts_data2), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(ts_data2,2.718^pred$pred, log = "y", lty = c(1,3))

Bibliography

1.	Tan, P., Steinbach, M. and Kumar, V. (2015). Introduction to data mining. Dorling Kindersley: Pearson, pp.223 - 227, 569 - 667.
2.	Witten, I., Frank, E. and Hall, M. (2011). Data mining. 3rd ed. Burlington, Mass.: Morgan Kaufmann Publishers, pp.273 -293, 566 - 585.
3.	Medium. (2019). Outlier-Aware Clustering: Beyond K-Means. [online] Available at: https://towardsdatascience.com/outlier-aware-clustering-beyond-k-means-76f7bf8b4899 [Accessed 3 Nov. 2019].
4.	Barai, A. and Dey, L. (2019). Outlier Detection and Removal Algorithm in K-Means and Hierarchical Clustering. [online] Pdfs.semanticscholar.org. Available at: https://pdfs.semanticscholar.org/4c68/4a9ba057fb7e61733ff554fe2975a2c91096.pdf [Accessed 3 Nov. 2019].
5.	Medium. (2019). Clustering Analysis in R using K-means. [online] Available at: https://towardsdatascience.com/clustering-analysis-in-r-using-k-means-73eca4fb7967 [Accessed 1 Nov. 2019].
6.	Magallanes Reyes, J. (2017). Introduction to data science for social and policy research. Cambridge: Cambridge University Press, pp.205 - 2012.
7.	Rabie, H. (2008). Exploring Input Selection for Time Series Forecasting. [online] academia.edu. Available at: https://www.academia.edu/8810489/Exploring_Input_Selection_for_Time_Series_Forecasting [Accessed 5 Nov. 2019].
8.	G. J. Schinasi and P. A. Swamy, (1989).“The out-of-sample forecasting performance of exchange rate models when coeﬃcients are allowed to change”.   Journal of International Money and Finance, pp.373 – 390. Available at: https://www.sciencedirect.com/science/article/pii/0261560689900041 [Accessed 5 Nov. 2019].
9.	Huang, W. (2019). NEURAL NETWORKS IN FINANCE AND ECONOMICSFORECASTING. [online] Research Gate. Available at: https://www.researchgate.net/publication/23751172_Neural_Networks_in_Finance_and_Economics_Forecasting [Accessed 6 Nov. 2019].
10.	Wurtz, D. and Setz, T. (2019). Basic R for Finance. 2015: Rmetrics Association & Finance Online Publishing, pp.3 - 63,105-139.
11.	WÜRTZ, D., Ellis, A. and Chalabi, Y. (2019). FINANCIAL MARKET DATA FOR R/RMETRICS. Rmetrics Association & Finance Online, pp.2 -15, 32 - 50.
12.	Brownlee, J. (2019). How to Check if Time Series Data is Stationary with Python. [online] Machine Learning Mastery. Available at: https://machinelearnhttps://machinelearningmastery.com/time-series-data-stationary-python/ingmastery.com/time-series-data-stationary-python/ [Accessed 6 Nov. 2019].
13.	Anomaly. (2019). Detecting Seasonality Using Fourier Transforms in R - Anomaly. [online] Available at: https://anomaly.io/detect-seasonality-using-fourier-transform-r/index.html [Accessed 6 Nov. 2019].
14.	Rdocumentation.org. (2019). mlp function | R Documentation. [online] Available at: https://www.rdocumentation.org/packages/RSNNS/versions/0.4-12/topics/mlp [Accessed 6 Nov. 2019].
15.	national Money and Finance 8(1989) 373–390Works Cited
 
