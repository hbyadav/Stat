
#2.a

library("ISLR")

lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)

#2.a.iv
predict(lm.fit, data.frame(horsepower = c(85)), interval ="confidence")
predict(lm.fit, data.frame(horsepower = c(85)), interval ="confidence")

#2.b

attach(Auto)
plot(mpg~horsepower, main =" MPG vs Horsepower", xlab = " Horsepower", ylab ="MPG")
abline(coef = coef(lm.fit), col ="red")
detach(Auto)


#2.c
par(mfrow=c(2,2))
plot(lm.fit)

#3.a
library("ISLR")
pairs(Auto)
cor(Auto[, names(Auto) !="name"])

#3.b
model = lm(mpg ~. -name, data = Auto)
summary(model)

#3.c
par(mfrow = c(2,2))
plot(model)

#3.d
model = lm(mpg ~.-name+displacement:weight, data = Auto)
summary(model)

#3.e
model = lm(mpg ~.-name+displacement:cylinders+displacement:weight+acceleration:horsepower, data=Auto)
summary(model)

#3.f
model = lm(mpg ~.-name+displacement:cylinders+displacement:weight+year:origin+acceleration:horsepower, data=Auto)
summary(model)

#3.g
model = lm(mpg ~.-name-cylinders-acceleration+year:origin+displacement:weight+
             displacement:weight+acceleration:horsepower+acceleration:weight, data=Auto)
summary(model)

#4.a
library("ISLR")
head(Carseats)
str(Carseats)
lm.fit = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(lm.fit)


#4.d
lm.fit2 = lm(Sales ~ Price+US, data= Carseats)
summary(lm.fit2)

#4.g
confint(lm.fit2)

#4.h
par(mfrow=c(2,2))
plot(lm.fit2)


#5.a
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
slr<-lm(y~x+0)
summary(slr)

#5.b
revslr<-lm(x~y+0)
summary(revslr)


#5.d
n=length(x)
t=sqrt(n - 1)*(x %*% y)/sqrt(sum(x^2) * sum(y^2) - (x %*% y)^2)
as.numeric(t)

#5.f
revslr1<-lm(x~y)
summary(revslr1)




