#create file in class


#make a vector of tree heights in meters
heights <- c(30,41,20,22)

#convert to cm through multiplication on each element
heights_cm <- heights*100
heights_cm


#explore subsetting vector
heights[3] #20
heights_cm[2] #4100


#get more info on the matrix function
help(matrix)


#create matrix with 2 columns and fill by row
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#create matrix with same input vector of numbers but fill by column
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol


#explore subsetting matrix by [row, column]

Mat.bycol[1,2] #











