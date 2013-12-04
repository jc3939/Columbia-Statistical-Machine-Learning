library(e1071)
library(rgl)

class=as.matrix(read.table("/home/james/R/HW1/uspscl.txt"))
data=as.matrix(read.table("/home/james/R/HW1/uspsdata.txt"))
random_sample=sample(40)
test_set=data[random_sample,]
train_set=data[-random_sample,]

test_class=class[random_sample]
train_class=class[-random_sample]
train=cbind(train_set, train_class)
linear_result=NULL
radial_result=NULL
for (cos in 10^seq(-5,2,1)){
  linear_model=svm(train_set, train_class, type="C-classification", kernel="linear",cost=cos, cross=10)
  pred=predict(linear_model,test_set)
  linear_result=c(linear_result,(table(pred, test_class)[1,2]+table(pred, test_class)[2,1])/40)
}



for (cos in 2^c(-5:-1)){
  for (ga in 10^c(-5:-1)){
    rbf_model=svm(train_set, train_class, type="C-classification", kernel="radial",cost=cos, gamma=ga, cross=10)
    pred=predict(rbf_model,test_set)
    radial_result=c(radial_result,(table(pred, test_class)[1,2]+table(pred, test_class)[2,1])/40)
  }
}
plot(c(-5:2),linear_result, xlab='cost')
lines(c(-5:2),linear_result, xlab='cost')
plot3d(c(-5:-1),log(2)*c(-5:-1),radial_result, col='red', xlab='cost', ylab='gamma', zlab='misclassification')
