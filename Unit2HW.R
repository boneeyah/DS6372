library(car)
library(ISLR)
library(leaps)
Auto <- Auto[,-9]
#remove 3 & 5 cyls
str(as.factor(Auto$cylinders))
Auto <- Auto[Auto$cylinders %in% c(4,6,8),]
#set factors
Auto$cylinders <- as.numeric(Auto$cylinders)
Auto$origin <- as.numeric(Auto$origin)

#50-50 split
set.seed(1234)
indx <- sample(1:nrow(Auto),round(nrow(Auto)*.5))
train <- Auto[indx,]
test <- Auto[-indx,]

#perform forward selection
fwd <- regsubsets(log(mpg)~.,data=train,method = 'forward')
predict.regsubset = function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

testASE <- c()
for (i in 1:7) {
  predictions <- predict.regsubset(object=fwd,newdata = test,id=i)
  testASE[i] <- mean((log(test$mpg)-predictions)^2)
  
}
par(mfrow=c(1,1))
plot(1:7,testASE,type='l',xlab = "num of predictors",ylab = "test vs train ASE",
     ylim = c(.014,.05))
index <- which(testASE==min(testASE))
points(index,testASE[index],col='red',pch=10)
rss <- summary(fwd)$rss
lines(1:7,rss/100,lty=3,col="blue")
coef(fwd,5)

model <- lm(log(mpg)~horsepower+weight+year+origin,data = Auto)
summary(model)
