
# 1 Learning ggplot2
# 1 (a) 
# 3.2.4 Exercises #4, #5
library(ggplot2)
data(mpg)
# 4 a scatterplot of hwy vs cyl.
ggplot(data = mpg) + geom_point(mapping = aes(x=hwy, y=cyl))
# 5 a scatterplot of class vs drv
ggplot(data = mpg) + geom_point(mapping = aes(x=class, y=drv))
# Why is the plot not useful?
# --> Class and drv values are not numerical, thus it is hard to find whether 
# two variables are correlated or not.

# 3.3.1 Exercises #3, #4, #6
# 3 Map a continuous variable to color, size, and shape
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = cyl))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = cyl))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = cyl))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))


#4 map the same variable to multiple aesthetics
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = cyl, size = cyl))
# Multiple aesthetics are applied together. eg) Both color and size represents cyl value together 

#6 map an aesthetic to something other than a variable name
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = cyl < 6))
# The plot shows logical values in colors which are equivalent to the answers to the conditions (eg. cyl < 6)

# 3.5.1 Exercises #4
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2) 


# 1 (b) Generate ggplot using mpg data
theme_set(theme_bw())
ggplot(data = mpg) +  # use mpg data
  geom_point(mapping = aes(x = displ, y = hwy), alpha = 0.4, position='jitter', stroke = 0) + # add points with alpha (transparancy) and jitter (adds a small amount of random variation to the location of each point) option
  geom_smooth(mapping = aes(x = displ, y = hwy), method = loess) + # add locally smoothed line regressing the points
  facet_wrap(~drv) + # faceting
  xlab('Displacement')+ylab('Highway MPG')+ # add x and y lables
  geom_smooth(mapping = aes(x = displ, y = hwy),method=lm, se=FALSE,color='black') # add linearly smoothed line regressing the points

# 2 Generating data and advanced density plots
# 2 (a)
# generate random numbers with different ways
a <- rexp(500) # # exponentially distributed random numbers
b <- runif(500) # uniforml distributed random numbers
c <- rnorm(500) # normal distributed random numbers
d <- rnorm(500, mean=50, sd=10) # normal distributed random numbers with a specific mean and sd
# generate data frame
df <- data.frame(a,b,c,d)
head(df)
#reshaping" the data into two columns: groupVar and value.
library(tidyverse)
#library(dplyr)
df2 <- gather(df, "groupVar", "value",a,b,c,d)
head(df2)
tail(df2)

# 2 (b)
# plot df2 using ggplot
ggplot(data = df2, aes(x = groupVar, fill =groupVar,  color=groupVar)) +  # different color with groupVar
  geom_density(alpha = 0.1) # density plot with transparancy

# 3 House prices data
housing <- read.csv(file="housingData.csv", header=TRUE, sep=",")
head(housing)

# 5 plots aiding analysis of the data
# 1) histogram of data
library(gridExtra)
p1 <- ggplot(data = housing, aes(OverallQual)) +  
  geom_histogram()

p2 <- ggplot(data = housing, aes(OverallCond)) +  
  geom_histogram()

p3 <- ggplot(data = housing, aes(GrLivArea)) +  
  geom_histogram()

p4 <- ggplot(data = housing, aes(SalePrice)) +  
  geom_histogram()

grid.arrange(p1,p2,p3,p4, nrow = 2)


#2) density plot      
p1 <-ggplot(data = housing) +
  geom_density(aes(x=SalePrice, fill=MSZoning), alpha=0.45) 

p2 <-ggplot(data = housing) +
  geom_density(aes(x=SalePrice, fill=Neighborhood), alpha=0.45) 

p3 <-ggplot(data = housing) +
  geom_density(aes(x=SalePrice, fill=HouseStyle), alpha=0.45) 

grid.arrange(p1,p2,p3, nrow = 1)


#3) scatter plot   
p1 <- ggplot(data = housing, aes(x = MSZoning, y = SalePrice)) + 
  geom_point()

p2 <- ggplot(data = housing, aes(x = Neighborhood, y = SalePrice)) +  
  geom_point()

p3 <- ggplot(data = housing, aes(x = OverallQual, y = SalePrice)) +  
  geom_point()

p4 <- ggplot(data = housing, aes(x = OverallCond, y = SalePrice)) + 
  geom_point()

grid.arrange(p1,p2,p3,p4, nrow = 2)

#) box plot
p1 <-ggplot(housing,aes(x=MSZoning, y=SalePrice)) + geom_boxplot()
p2 <-ggplot(housing,aes(x=Neighborhood, y=SalePrice)) + geom_boxplot()
grid.arrange(p1,p2, nrow = 2)

#5) facet
require(stats)
reg<-lm(SalePrice ~ OverallQual, data = housing)
coeff=coefficients(reg)
reg

ggplot(data = housing) + 
  geom_point(mapping = aes(x = OverallQual, y = SalePrice), color='grey') + 
  facet_wrap(~ Neighborhood, nrow = 4) +
  geom_smooth(mapping = aes(x = OverallQual, y = SalePrice), method = lm, color='black') + # add locally smoothed line regressing the points
  geom_abline(intercept = coeff[1], slope = coeff[2]  , color='red')

# 4 explore the "missingness" 
# 4(a)
library(Amelia)
data(freetrade)
head(freetrade)

# use mice package
library(mice)
md.pattern(freetrade)
md.pairs(freetrade)

# use VIM package
library(VIM)  # Use VIM package
a <-aggr(freetrade)
summary(a)   # summary of missing data
marginplot(freetrade[c("tariff","polity")]) # marginplot of two variables
scattmatrixMiss(freetrade) # scatter plot

# 4(b) 
df2 = freetrade[,c(2,3)]  #data frame with country and tariff
df2[is.na(df2$tariff),]["tariff"] = 0   #assign 0 to missing values
df2$tariff = ifelse(df2$tariff>=1,1,0)  #assign 1 to other values         
tb = table(df2)
chisq.test(tb)

# remove Nepal
df_drop_Nepal <-df2[!df2$country =="Nepal",]  # exclude variable Nepal in the dataframe
tb_drop_Nepal = table(df_drop_Nepal)
chisq.test(tb_drop_Nepal)

# remove Philippines
df_drop_Philippines <-df2[!df2$country =="Philippines",] # exclude variable Nepal in the dataframe
tb_drop_Philippines = table(df_drop_Philippines)
chisq.test(tb_drop_Philippines)







