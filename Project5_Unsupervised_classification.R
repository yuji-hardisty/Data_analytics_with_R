
library(ggplot2)    #for graphics and the dataset for this example session
library(cluster)    #provides more cluster algorithms than base R (e.g. PAM)
library(useful)     #provides a plot function for clusters and "FitKMeans" and "PlotHartigan" functions
library(NbClust)    #provides tons of tools for identifying the "right" number of clusters
library(rgl)        #for 3D rotating plots
library(tidyverse)
# 1. Importing <seismic attributes> data 
train_raw <- read_csv("training_data_9000.csv")
train <- sample_frac(train_raw, 0.05)
dim(train)
colnames(train)<-c('index','RMS_Amp','Total_Energy','Rel_Ac_Imp','Inst_Env','Inst_Freq','Inst_Phase',
                   'Variance','Dip_Mag','Dip_Azim','K1_Curv','K2_Curv','K_Curvedness',
                   'Abberancy_Mag','Abberancy_Azim','GLCM_Entropy','GLCM_H','Chaos','Peak_Mag','Peak_Freq',
                   'Peak_Phase','class')
train_in = train[,2:21]
dim(train_in)
glimpse(train_in)

p<-qplot(data=train, x=Total_Energy, y=Variance, color=factor(class))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g) +labs(title="class: seismic facies")


train_in<-scale(train_in)

train_in=data.frame(train_in)

# 2. Test 3 methods and
# 3. Compare with visualization
#kmeans is a function in the standard R package "stats"
KM <- kmeans(train_in,10, centers = 3, nstart=3)          
KM$centers 
KM$cluster <-fct_shift(factor(KM$cluster))

p<-qplot(data=train_in, x=Total_Energy, y=Variance, color=factor(KM$cluster))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g) +labs(title="k-means clustering")


#k-medoids partition
set.seed(42)
KMED <- pam(train_in, 3)
KMED$centers 
p<-qplot(data=train_in, x=Total_Energy, y=Variance, color=factor(KMED$cluster))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g) +labs(title="k-medoids clustering")

# hierarchical clustering
di <- dist(train_in, method="euclidean")   # with hiearchical clustering, only need distance matrix

hc <- hclust(di, method="ward")
plot(hc, labels=FALSE)

rect.hclust(hc, k=3, border="red")     #if we were to "cut" at k=3, what are the groups?
train$hcluster <- as.factor(cutree(hc, k=3))   #cutting at k=3, here are the assignments
train$hcluster <-(cutree(hc, k=3)) 

#and a quick check to see how our 'natural clusters' align with the species data
table( train$class, train$hcluster)

p<-qplot(data=train_in, x=Total_Energy, y=Variance, color=factor(train$hcluster))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g) +labs(title="hierarchical clustering")

# 4. Interpretation
# 3D plot of k-medoids result and actual class(seismic facies)
plot3d(train[,c("Total_Energy","Variance","Dip_Mag")], col=KMED$cluster, main="k-medoids clusters")
plot3d(train[,c("Total_Energy","Variance","Dip_Mag")], col=factor(train$class), main="k-medoids clusters")




