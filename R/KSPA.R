KSPA <-
function(Error1, Error2, method = c("abs", "sqe", "biqc"))
{
par = hist = legend = c()
# Input the forecast errors from two models. Let Error1 show errors from the model with the lower error based on some loss function.
method = match.arg (method)
Error1<- Error1
Error2<- Error2

# Convert the raw forecast errors into absolute values or squared values depending on the loss function.(abs or sqe or biq)
if(method == 'abs')
{
abs1<-abs(Error1)
abs2<-abs(Error2)
# Two-sided KSPA test:(output)
ks2 = ks.test(abs1,abs2)

# One-sided KSPA test:(output)
ks1 = ks.test(abs1,abs2, alternative = c("greater"))

#OPTIONAL GRAPHS FOR MORE INFORMATION
# Draw histograms for the forecast errors from each model.
opar = par(mfrow=c(1,3))
on.exit(par(opar))

hist(abs1, xlab="Model 1 Absolute Errors", main="")
hist(abs2, xlab="Model 2 Absolute Errors",main="")

# Plot the cdf of forecast errors from each model*.
plot(ecdf(abs1),do.points=T,col="red",xlim=range(abs1,abs2),main="")
plot(ecdf(abs2),do.points=T,col="blue",add=TRUE, main="")
legend("bottomright",legend=c("Model 1 Absolute Errors","Model 2
Absolute Errors"), lty=1, col=c("red","blue"))
mylist = list(ls.func = "Used loss function is 'absolute errors'", ks.oneside = ks1, ks.twoside = ks2)
return(mylist)
}
if(method == 'sqe')
{
sqe1<-(Error1)^2
sqe2<-(Error2)^2

# Two-sided KSPA test:(output)
ks2 = ks.test(sqe1,sqe2)

# One-sided KSPA test:(output)
ks1 = ks.test(sqe1,sqe2, alternative = c("greater"))

#OPTIONAL GRAPHS FOR MORE INFORMATION
# Draw histograms for the forecast errors from each model.
opar = par(mfrow=c(1,3))
on.exit(par(opar))

hist(sqe1, xlab="Model 1 Square Errors", main="")
hist(sqe2, xlab="Model 2 Square Errors",main="")

# Plot the cdf of forecast errors from each model*.
plot(ecdf(sqe1),do.points=T,col="red",xlim=range(sqe1, sqe2),main="")
plot(ecdf(sqe2),do.points=T,col="blue",add=TRUE, main="")
legend("bottomright",legend=c("Model 1 Square Errors","Model 2
Square Errors"), lty=1, col=c("red","blue"))
mylist = list(ls.func = "Used loss function is 'square errors'", ks.oneside = ks1, ks.twoside = ks2)
return(mylist)
}
if(method == 'biqc')
{
biqc1<- (Error1)^4
biqc2<- (Error2)^4

# Two-sided KSPA test:(output)
ks2 = ks.test(biqc1,biqc2)

# One-sided KSPA test:(output)
ks1 = ks.test(biqc1,biqc2, alternative = c("greater"))

#OPTIONAL GRAPHS FOR MORE INFORMATION
# Draw histograms for the forecast errors from each model.
opar = par(mfrow=c(1,3))
on.exit(par(opar))

hist(biqc1, xlab="Model 1 Fourth Power of Errors", main="")
hist(biqc2, xlab="Model 2 Fourth Power of Errors",main="")

# Plot the cdf of forecast errors from each model*.
plot(ecdf(biqc1),do.points=T,col="red",xlim=range(biqc1, biqc2),main="")
plot(ecdf(biqc2),do.points=T,col="blue",add=TRUE, main="")
legend("bottomright",legend=c("Model 1 Fourth Power of Errors","Model 2
Fourth Power of Errors"), lty=1, col=c("red","blue"))
mylist = list(ls.func = "Used loss function is 'fourth power of errors'", ks.oneside = ks1, ks.twoside = ks2)
return(mylist)
}
}
