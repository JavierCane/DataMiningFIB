Introduction to R
Karina Gibert
karina.gibert@upc.es
February 2013


Install 
http://cran.r-project.org 


A first session: 
Clik on R icon 
# a comment 
# Generate 100 observations N(0,1) 
x <- rnorm(100) 
?rnorm
# a look at the content of x 
x
# basic descriptive statistics 
summary(x) 
hist(x) 
#sort values of x
x<-sort(x)
# generate a second variable 
y <- sin(x*pi)+0.1*rnorm(100)  
y  
# basic descriptive statistics for y 
summary(y) 
hist(y) 
# and its relation with x 
plot(x,y) 
# creation of a data.frame with x and y 
dd <- data.frame(x,y) 
# contents of the workpackage
objects() 
# which kind of object is dd? 
class(dd) 
# and x?
class(x)
# structure of objects
attributes(dd) 
attributes(x)
#verify qualitative variables
is.factor(y)
# Apply a function to all objects in a data frame
sapply(dd, is.factor) 
# lets verify the min and max ofall variables in dd 
sapply(dd, range) 
# linear regression of y function of x 
reg1 <- lm(y ~ x, data=dd) 
print (reg1) 
# structure of object produced 
attributes(reg1) 
# access to part of the results 
reg1$coefficients 
reg1$fitted.values
# Check the fit visually 
plot(x,y) 
# and without closing the plot window 
lines(x, reg1$fitted.values)
# Change regression algorithm by a local regression
reg2 <- loess(y ~ x, data=dd) 
# and plot the new fit in the same display
lines(x,reg2$fitted,col= "blue") 
# perform a polinomic regression 
# introducing powers of x: 
reg3 <- lm(y ~ x+x^2, data=dd) 
lines(x,reg3$fitted.values,col= "green")  
#alternatively set n= degree of polynomial to be fitted 
reg4 <- lm(y ~ poly(x,3), data=dd)
lines(x,reg4$fitted.values,col= "yellow")  
reg5 <- lm(y ~ poly(x,4), data=dd)
lines(x,reg5$fitted.values,col= "cyan")  
reg6 <- lm(y ~ poly(x,6), data=dd)
lines(x,reg6$fitted.values,col= "red")  
reg10 <- lm(y ~ poly(x,10), data=dd)
lines(x,reg10$fitted.values,col= "magenta")  


:
:
#What is the degree with a better fit? 
#What is the model you will choose to make predictions? 
# end of estimation step


# Use this model to predict new observations. 




#Simulate new observations following the model plus some noise
xnew <- rnorm(100, mean=0.3)
xnew <- sort(xnew)
ynew <- sin(xnew*pi*0.9)+0.2*rnorm(100) 


# Inspect new dataset 
plot(xnew,ynew) 
# Use the chosen model to predict this new data 
ypred1 <- predict(reg10,new.data=xnew) 
lines(xnew,ypred1) 
ypred2 <- predict(reg2,new.data=xnew) 
lines(xnew,ypred2, col="red") 

reg14 <- lm(y ~ poly(x,14), data=dd)
ypred3 <- predict(reg14,new.data=xnew) 
lines(xnew,ypred3, col="pink") 


:
:
:
#Some objective way to assess the quality of the fit 
rss <- sum((ynew-ypred1)^2) 
rss 
# Who got the best model? 
# is this measure rss reliable?

