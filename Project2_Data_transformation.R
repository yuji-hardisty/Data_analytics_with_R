
# 1 Glass data transformation
library(mlbench) # load mlbench package
data(Glass)  # load the Glass data
head(Glass)
Glass<-Glass[!duplicated(Glass),] # remove duplicated row
glass<-Glass[,1:9]
colnames(glass)=colnames(Glass[,1:9])

#1 i.
library("car")        #<-- used to get Prestige dataset; and 'symbox' function
library(tidyr)
library(ggplot2)    # packages to generate histogram of each feature
glass %>% gather() %>% head()
ggplot(gather(glass),aes(value)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~key, scales = 'free_x')
# --> Al, Ca, Na, are skewed.

par(mfrow=c(3,4))

hist(glass$Al, main='hist. before transformation: Al') 
boxplot(glass$Al, main='boxplot before transformation: Al') 
symbox(glass$Al, data=glass, powers=c(3,2,1,0,-0.5,-1,-2), main='boxplot with multiple lambda: Al')
hist(log(glass$Al),main='hist. after transformation: Al')

hist(glass$Ca, main='hist. before transformation: Ca')     
boxplot(glass$Ca, main='boxplot before transformation: Ca')  
symbox(glass$Ca, data=glass, powers=c(3,2,1,0,-0.5,-1,-2), main='boxplot with multiple lambdas: Ca')
hist(log(glass$Ca),main='hist. after transformation: Ca') 

hist(glass$Na, main='hist. before transformation: Na') 
boxplot(glass$Na, main='boxplot before transformation: Na') 
symbox(glass$Na, data=glass, powers=c(3,2,1,0,-0.5,-1,-2), main='boxplot with multiple lambdas: Na')
hist(log(glass$Na),main='hist. after transformation: Na')

library("EnvStats")   #<-- used to get "boxcox" function
par(mfrow=c(3,2))

hist(glass$Al, main='hist. before transformation: Al',breaks = 40)
# can use boxcox to search for the optimal lambda value
boxcox(glass$Al, optimize = TRUE, lambda=c(-3,3))    
lambda = 0.465156 
hist((glass$Al**lambda-1)/lambda, breaks = 40)  # display histogram after transformation

hist(glass$Ca, main='hist. before transformation: Ca', breaks = 40)     
boxcox(glass$Ca, optimize = TRUE, lambda=c(-3,3))    
lambda=-0.8648311
hist((glass$Ca**lambda-1)/lambda, breaks = 40) 


hist(glass$Na, main='hist. before transformation: Na',breaks = 40) 
boxcox(glass$Na, optimize = TRUE, lambda=c(-3,3))    
lambda = 0.02956477
hist((glass$Na**lambda-1)/lambda, breaks = 40) 

# 2. Missing data

library(mice)
data(msleep)
head(msleep)

library(tidyvese)
library(dplyr)
msleep %>% select_if(is.numeric) %>% mutate_all(is.na) %>% summarise_all(mean)

#2 (a)
# use VIM package
library(VIM)  # Use VIM package
a <-aggr(msleep)
summary(a)   # summary of missing data
marginplot(msleep[c("sleep_rem","sleep_cycle")]) # marginplot of two variables
scattmatrixMiss(msleep) # scatter plot

# use mice package
#library(mice)
md.pattern(msleep)
md.pairs(msleep)


# 2 (b)

msleep_num <- msleep %>% select_if(is.numeric) # only take features with numeric values 
pairs(msleep_num)

# histogram of all numerical variable
msleep_num %>% gather() %>% head()
ggplot(gather(msleep_num),aes(value)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~key, scales = 'free_x')

# transformation of sleep cycle, bodywt and brainwt with log()

hist(msleep_num$sleep_cycle, main='hist. before transformation: sleep_cycle') 
boxplot(sleep_cycle, main='boxplot before transformation: sleep_cycle')  
symbox(sleep_cycle, data=msleep_num, powers=c(3,2,1,0,-0.5,-1,-2), main='boxplot with multiple lambdas: sleep_cycle')
hist(log(msleep_num$sleep_cycle),main='hist. after transformation: sleep_cycle') 
msleep_num$sleep_cycle <- log(msleep_num$sleep_cycle)

hist(msleep_num$bodywt, main='hist. before transformation: bodywt') 
boxplot(msleep_num$bodywt, main='boxplot before transformation: bodywt')  
symbox(msleep_num$bodywt, data=msleep_num, powers=c(3,2,1,0,-0.5,-1,-2), main='boxplot with multiple lambdas: bodywt')
hist(log(msleep_num$bodywt),main='hist. after transformation: bodywt') 
msleep_num$bodywt <- log(msleep_num$bodywt)

hist(msleep_num$brainwt, main='hist. before transformation: brainwt') 
boxplot(msleep_num$brainwt, main='boxplot before transformation: brainwt')  
symbox(msleep_num$brainwt, data=msleep_num, powers=c(3,2,1,0,-0.5,-1,-2), main='boxplot with multiple lambdas: bodywt')
hist(log(msleep_num$brainwt),main='hist. after transformation: brainwt') 
msleep_num$brainwt <- log(msleep_num$brainwt)

pairs(msleep_num) # scatter plot after transformation

# Plot log(bodywt) vs log(sleep_cycle)
library(ggpmisc)
ggplot(msleep_num, aes(x =bodywt, y = sleep_cycle)) +
  geom_point() +
  labs(y = "log(sleep_cycle)") +
  labs(x = "log(bodywt)") +
  geom_smooth(method = "lm", formula = y ~ x,se=FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) 


# 2(c) i use mice to conduct multiple imputation

 
# method: norm.nob (Linear regression ignoring model error)
# create m=5 data sets and impute missing values 
imputed_data <-mice(msleep_num,m=5,meth="norm.nob", maxit = 100, seed = 100)
str(imputed_data) # the output object

#take a look at how the means and variances of the imputed values are (hopefully) converging 
imputed_data$chainMean
imputed_data$chainVar

#can plot those means and variances
plot(imputed_data)

# replce missing value
msleep_MI <- complete(imputed_data)
# perform the third step of MI using the "with" command
# to perform a standard analysis (in this case, a linear regression) on each data set 
fit<-with(imputed_data, lm(bodywt~sleep_total+sleep_rem+sleep_cycle+awake))

#perfrom the fourth step of MI, recombination, using the "pool" command 
est<-pool(fit)

plot(msleep_num)          #pairs plot of available cases

#coefficient estimates based on MICE (recombined estimates)
summary(est)


# 2.(c) ii. Build and evaluate a linear regression model based on the multiply imputed data.
plot_regression(msleep_MI)
summary(missfit_MI<-lm(data=msleep_MI,bodywt~sleep_total+sleep_rem+sleep_cycle+awake))

# make a function to plot regression between y = bodywt and other variables
# call function:  plot_regression(msleep_num)
library(gridExtra)
  plot_regression <- function(df) {
    p1 <- ggplot(df, aes(x =sleep_total, y = bodywt)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se=FALSE) +
      stat_poly_eq(formula = y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) 
    p2 <- ggplot(df, aes(x =sleep_rem, y = bodywt)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se=FALSE) +
      stat_poly_eq(formula = y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)   
    p3 <- ggplot(df, aes(x =sleep_cycle, y = bodywt)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se=FALSE) +
      stat_poly_eq(formula = y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)   
    p4 <- ggplot(df, aes(x =awake, y = bodywt)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se=FALSE) +
      stat_poly_eq(formula = y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)  
    
    grid.arrange(p1,p2,p3,p4, nrow=1)    
  }
  
# 2. (c) iii. compare with complete case
plot_regression(na.omit(msleep_num))
summary(na.omit(msleep_num)) 
#coefficient estimates based on complete cases (no imputation)
summary(missfit<-lm(data=msleep_num,bodywt~sleep_total+sleep_rem+sleep_cycle+awake))


# 2. (c) iv. compare with other methods
# Test other 3 methods:
# pmm: Predictive mean matching
imputed_data_pmm <-mice(msleep_num,m=5,meth="pmm", maxit = 100, seed = 100)
msleep_MI_pmm <- complete(imputed_data_pmm)
summary(missfit_MI_pmm<-lm(data=msleep_MI_pmm,bodywt~sleep_total+sleep_rem+sleep_cycle+awake))
plot_regression(msleep_MI_pmm)
fit_pmm<-with(imputed_data_pmm, lm(bodywt~sleep_total+sleep_rem+sleep_cycle+awake))
est_pmm<-pool(fit_pmm)
summary(est_pmm)



# cart: Classification and regression trees
imputed_data_cart <-mice(msleep_num,m=5,meth="cart", maxit = 100, seed = 100)
msleep_MI_cart <- complete(imputed_data_cart)
summary(missfit_MI_cart<-lm(data=msleep_MI_cart,bodywt~sleep_total+sleep_rem+sleep_cycle+awake))
plot_regression(msleep_MI_cart)  
fit_cart<-with(imputed_data_cart, lm(bodywt~sleep_total+sleep_rem+sleep_cycle+awake))
est_cart<-pool(fit_cart)
summary(est_cart)  

  
# sample: Random sample from observed values
imputed_data_sample<-mice(msleep_num,m=5,meth="sample", maxit = 100, seed = 100)
msleep_MI_sample <- complete(imputed_data_sample)
summary(missfit_MI_sample<-lm(data=msleep_MI_sample,bodywt~sleep_total+sleep_rem+sleep_cycle+awake))
plot_regression(msleep_MI_sample) 
fit_sample <-with(imputed_data_sample, lm(bodywt~sleep_total+sleep_rem+sleep_cycle+awake))
est_sample <-pool(fit_sample )
summary(est_sample)

  