## AcTIVITY 4 ##

#install.packages("dplyr")
#install.packages("ggplot2")

library(dplyr)
library(ggplot2)

###############################################################

# preview iris data set
head(iris)


#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length


iris_versicolor <- iris[iris$Species == "versicolor",]
head(iris_versicolor)

#single regession relationship
#lm.out <- lm(iris_versicolor[ ,"Sepal.Width"] ~ iris_versicolor[ ,"Sepal.Length"])

names(iris_versicolor)
x <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
y <- c( "Sepal.Width",  "Petal.Width", "Petal.Length")

lm.out <- list()

for (i in 1:3){
  
  lm.out[i] <- lm( iris_versicolor[ ,y[i]] ~ iris_versicolor[ ,x[i]])
  
  
}



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))


new_iris <- left_join(iris, height, by = "Species")

head(new_iris)







#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
         geom_point()



#3b. make a scatter plot with ggplot and get rid of  busy grid lines

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point() +
  theme_classic()


#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point(aes(size = Species)) +
  theme_classic()
  


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################	









