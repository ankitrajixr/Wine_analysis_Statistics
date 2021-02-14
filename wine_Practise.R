d<-read.csv("wine.csv")
d$class<-factor(d$class)
summary(d)

boxplot(d$Alcohol, main="Alcohol")
boxplot(d$Malic.acid, main="Malic.acid")
boxplot(d$Ash, main="Ash")
boxplot(d$Alcalinity.of.ash, main="Alcalinity.of.ash")
boxplot(d$Total.phenols, main="Total.phenols")
boxplot(d$Flavanoids, main="Flavanoids")
boxplot(d$Nonflavanoid.phenols, main="Nonflavanoid.phenols")

m1<-lm(Alcohol~Malic.acid+Alcalinity.of.ash+Magnesium+Proanthocyanins+ Color.intensity+Proline,data=d)
summary(m1)
m1.step<-step(m1,trace=0)
summary(m1.step)
install.packages("e1071")

require(e1071)
m2<-svm(Alcohol~Malic.acid+Alcalinity.of.ash+Magnesium+Proanthocyanins+ Color.intensity+Proline,data=d)
summary(m2)

plot(d$Alcohol, fitted(m2))
abline(h=axTicks(2), v=axTicks(1), col="lightgray")


require(party)
m3<-ctree(Alcohol~Malic.acid+Alcalinity.of.ash+Magnesium+Proanthocyanins+ Color.intensity+Proline,data=d)

m3

plot(m3)

#Using RMSE for comapring the three models:

rmse<-function(e){ sqrt(mean(e^2))}

##Linear Model 
rmse(residuals(m1))

##SVM
rmse(m2$residuals)

##CTREE

rmse(d$Alcohol - predict(m3))

#solution D

s <- round(nrow(d)*.7, 0)
s
## separete data in train and test
set.seed(12345) # to repliacte results, you do not need this
i <- sample(1:nrow(d), s)
train<-d[i,]
test<-d[-i,]

fit.svm <- svm(class ~ ., data=train)
summary(fit.svm)

## Preditions of training data
pred.train<-predict(fit.svm)
table("observed"=train$class,"predicted"=pred.train)

## Preditions test data
pred.test <- predict(fit.svm, newdata=test)
table("observed"=test$class,"predicted"=pred.test)

###Solution of e
s <- subset(d, select=-class)
fit.pca <- prcomp(s)
plot(fit.pca)


require(factoextra)
get_eig(fit.pca)

fviz_cos2(fit.pca, choice = "var", axes = 1:2)

fviz_pca_var(fit.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE) # Avoid text overlapping, not necessary here

nd <- data.frame(class=d$class, fit.pca$x[,1])
str(nd)

## separete data in train and test
train <- nd[i,]
test <- nd[-i, ]
fit.svm <- svm(class ~ ., data=train)
## summary of SVM model
summary(fit.svm)

## Preditions of training data
pred.train <- predict(fit.svm)
f1 <- table("observed"=train$class,"predicted"=pred.train)
list("Table"=f1,"Accuracy"=sum(diag(f1))/sum(f1))
pred.test <- predict(fit.svm, newdata=test)
f2 <- table("observed"=test$class,"predicted"=pred.test)
list("Table"=f2,"Accuracy"=sum(diag(f2))/sum(f2))
head(d)
install.packages("MASS")
data(cats, package = "MASS")
head(cats)
