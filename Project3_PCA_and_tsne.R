
# 1 Glass data
library(mlbench) # load mlbench package
data(Glass)  # load the Glass data
head(Glass)
Glass<-Glass[!duplicated(Glass),] # remove duplicated row
glass_label <-Glass[,10]
glass<-Glass[,1:9]
colnames(glass)=colnames(Glass[,1:9])

# 1 (a) Mathematics of PCA
# 1 (a) i.
cor(glass) 
corMat = cor(glass)  # create correlation matrix and store the results
corMat

# 1 (a) ii.
ev<-eigen(corMat)
# extract components
values <-ev$values    # extract eigenvalues
values 
vectors <-ev$vectors  # extract eigenvectors
vectors

# 1 (a) iii
# use prcomp to compute principal components
glass_pca <- prcomp(glass, scale.=TRUE)
glass_pca 

# 1 (a) iv 
# compute inner product of first and second principal component
dot_first_second = glass_pca["rotation"][[1]][1:9,1] %*% glass_pca["rotation"][[1]][1:9,2]
dot_first_second


# 1 (b) Application of PCA
# 1.(b) i.
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
glass_biplot <- ggbiplot(glass_pca,obs.scale = 1, var.scale = 1,
         varname.size = 4, labels.size = 2.5,
         circle = TRUE,groups= glass_label)+
         ggtitle('PCA of glass dataset')
glass_biplot

# 1.(b) iii.
# compute explained variance
glass_tf <- glass_pca$x
# get the variance of each sample
vars_tf <-apply(glass_tf,2,var)
# divide by total variance
vars_tf/sum(vars_tf)
# plot the explained variance using factoextra package
library(factoextra)
fviz_eig(glass_pca)

# 1.(c) i.
library(MASS)
glass_lda <- lda(formula = glass_label~ ., data=glass)  # apply lda to glass dataset
glass_lda

# 1.(c) iii.
glass_lda_values <- predict(glass_lda)
par(mar=c(4,2,1,5))
ldahist(glass_lda_values$x[,1], g = glass_label) # hist for LD1
ldahist(glass_lda_values$x[,2], g = glass_label) # hist for LD2


# 2. Facebook metrics
FB <- read.csv(file="FB-metrics.csv", header=TRUE, sep=",")
head(FB)

fb <-FB[,8:18]
head(fb)
colnames(fb)

#2(a) PCA analysis
# use prcomp to compute principal components
fb_pca <- prcomp(fb, scale.=TRUE)
fb_pca 
library(ggbiplot)
fb_biplot <- ggbiplot(fb_pca,obs.scale = 1, var.scale = 1,
                         varname.size = 4, labels.size = 2.5,
                         circle = TRUE, g=fb[,7])+   #G =3, 7??
  ggtitle('PCA of Facebook metrics') +
  scale_color_gradientn(colours = rainbow(5))
fb_biplot

fb_tf <- fb_pca$x
# get the variance of each sample
fb_vars_tf <-apply(fb_tf,2,var)
# divide by total variance
fb_vars_tf/sum(fb_vars_tf)


fviz_eig(fb_pca)
set.seed(42) # Set a seed if you want reproducible results

#2(b) tsne analysis
library("Rtsne")
fb_tsne <- Rtsne(fb) # Run TSNE
fb_tsne <- Rtsne(fb,perplexity=10,theta=0.7,max_iter = 5000)

# Show the objects in the 2D tsne representation
#plot(fb_tsne$Y,col=fb[,3])

df<-data.frame(x=fb_tsne$Y[,1],y=fb_tsne$Y[,2], type=fb[,7])
ggplot(data=df,aes(x=x,y=y,group=type,color=type))+geom_point() +
  scale_color_gradientn(colours = rainbow(5))



