#1.1
Credit = read.csv("Credit.csv",header=TRUE)
Credit<-Credit[-c(1)]
X = model.matrix(Balance~.*.,Credit)[,-1]
y = Credit$Balance
dim(X)

#1.2
set.seed(13579)
train = sample(1:nrow(X),nrow(X)/2)
test = -train

#1.3
library(glmnet)
grid = 10^seq(3,-1,length=100)
cv1.out = cv.glmnet(X[train,],y[train],alpha=0,lambda=grid,nfolds=10,thresh=1e-10)
cv1.out$lambda.min
bestlam1 = cv1.out$lambda.min
plot(cv1.out)
predict(cv1.out,type="coefficients",s=bestlam1)[1:66,]

#1.4
cv2.out = cv.glmnet(X[train,],y[train],alpha=1,lambda=grid,nfolds=10,thresh=1e-10)
cv2.out$lambda.min

bestlam2 = cv2.out$lambda.min
out = glmnet(X,y,alpha=1,lambda=grid,thresh=1e-10)
result = predict(out,type="coefficients",s=bestlam2)[1:66,]
selected_result = result[which(predict(out,type="coefficients",s=bestlam2)[1:66,]!=0)]
names(selected_result)
num=sum(as.numeric(result!=0))-1
num
plot(cv2.out)
predict(cv2.out,type="coefficients",s=bestlam1)[1:66,]

#1.6
set.seed(13579)
linear.mod = lm(y[train]~X[train,])
linear.pred = coef(linear.mod)[1]+X[test,] %*% coef(linear.mod)[-1]
mean((linear.pred-y[test])^2)

ridge.pred = predict(cv1.out,s=bestlam1,newx=X[test,])
mean((ridge.pred-y[test])^2)

lasso.pred = predict(cv2.out,s=bestlam2,newx=X[test,])
mean((lasso.pred-y[test])^2)


#1.7
plot(y[test],linear.pred,ylim=c(-300,2100),xlab="y_test",ylab="predicted")
points(y[test],ridge.pred,col="blue")
points(y[test],lasso.pred,col="orange")
abline(0,1)

par(mfrow=c(1,3))
plot(y[test],linear.pred,ylim=c(-300,2100),xlab="y_test",ylab="predicted")
abline(0,1)

plot (y[test],ridge.pred,col="blue")
abline(0,1)

plot(y[test],lasso.pred,col="orange")
abline(0,1)

# 2.1
set.seed(10000)
library(gam)
train = sample(1:nrow(Credit),nrow(Credit)/2)
test = -train

gam.mod1 = gam(Balance~ ns(Income,df=4)+ ns(Age,df=1)+Student,data=Credit[train,])
gam.mod2 = gam(Balance~ns(Income,df=4)+ns(Age,df=2)+Student,data=Credit[train,])
gam.mod3 = gam(Balance~ns(Income,df=4)+ns(Age,df=3)+Student,data=Credit[train,])
gam.mod4 = gam(Balance~ ns(Income,df=4)+ ns(Age,df=4)+Student,data=Credit[train,])
gam.mod5 = gam(Balance~ns(Income,df=4)+ns(Age,df=5)+Student,data=Credit[train,])
gam.mod6 = gam(Balance~ns(Income,df=4)+ns(Age,df=6)+Student,data=Credit[train,])
gam.mod7 = gam(Balance~ns(Income,df=4)+ns(Age,df=7)+Student,data=Credit[train,])
gam.mod8 = gam(Balance~ns(Income,df=4)+ns(Age,df=8)+Student,data=Credit[train,])
gam.mod9 = gam(Balance~ns(Income,df=4)+ns(Age,df=9)+Student,data=Credit[train,])
gam.mod10 = gam(Balance~ns(Income,df=4)+ns(Age,df=10)+Student,data=Credit[train,])

pred.mod1 = predict(gam.mod1,newdata=Credit[test,])
pred.mod2 = predict(gam.mod2,newdata=Credit[test,])
pred.mod3 = predict(gam.mod3,newdata=Credit[test,])
pred.mod4 = predict(gam.mod4,newdata=Credit[test,])
pred.mod5 = predict(gam.mod5,newdata=Credit[test,])
pred.mod6 = predict(gam.mod6,newdata=Credit[test,])
pred.mod7 = predict(gam.mod7,newdata=Credit[test,])
pred.mod8 = predict(gam.mod8,newdata=Credit[test,])
pred.mod9 = predict(gam.mod9,newdata=Credit[test,])
pred.mod10 = predict(gam.mod10,newdata=Credit[test,])

mse1 = mean((pred.mod1-Credit $Balance[test])^2)
mse2 = mean((pred.mod2-Credit $Balance[test])^2)
mse3 = mean((pred.mod3-Credit $Balance[test])^2)
mse4 = mean((pred.mod4-Credit $Balance[test])^2)
mse5 = mean((pred.mod5-Credit $Balance[test])^2)
mse6 = mean((pred.mod6-Credit $Balance[test])^2)
mse7 = mean((pred.mod7-Credit $Balance[test])^2)
mse8 = mean((pred.mod8-Credit $Balance[test])^2)
mse9 = mean((pred.mod9-Credit $Balance[test])^2)
mse10 = mean((pred.mod10-Credit $Balance[test])^2)

y = c(mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10)
x=seq(1,10,length=10)
plot(x, y,col="red",pch=8,xlab="degrees of freedom for age",ylab="test mean squared error",cex.lab=1.5)
lines(x, y,col="red",lwd=2)


#2.2
par(mfrow=c(1,3))
plot(gam.mod4,col="blue",lwd=2)


#2.3
sqrt(mse4)
mean(Credit $balance)


#3.1
set.seed(987654321)
Parkinsons= read.csv("parkinsons.csv",header=TRUE)
Parkinsons<-Parkinsons[-c(1)]
X = model.matrix(UPDRS~.,Parkinsons)[,-1]
y = Parkinsons$UPDRS
X = scale(X)
train = sample(1:nrow(X), 30)
test = -train
linear.mod = lm(y[train]~X[train,])
summary(linear.mod)


#3.2
library(glmnet)
grid = 10^seq(3,-1,length = 100)
cv.out = cv.glmnet(X[train,],y[train],alpha=1,lambda=grid,nfolds=30,thresh=1e-10)
cv.out$lambda.min
bestlam = cv.out$lambda.min
lasso.pred = predict(cv.out,s=bestlam,newx=X[test,])
mean((lasso.pred-y[test])^2)
plot(cv.out)


#3.3
out = glmnet(X,y,alpha=1,lambda=grid,thresh=1e-10)
result = predict(out,type="coefficients",s=bestlam)[1:98,]
selected_result = result[which(predict(out,type="coefficients",s=bestlam)[1:98,]!=0)]
selected_result
predict(out,type="coefficients",s=bestlam)[1:98,]



#3.4
train = sample(1:nrow(X), 33)
test = -train
cv1.out = cv.glmnet(X[train,],y[train],alpha=1,lambda=grid,nfolds=30,thresh=1e-10)
cv1.out$lambda.min
bestlam1 = cv1.out$lambda.min
lasso.pred = predict(cv1.out,s=bestlam1,newx=X[test,])
mean((lasso.pred-y[test])^2)
out = glmnet(X,y,alpha=1,lambda=grid,thresh=1e-10)
result = predict(out,type="coefficients",s=bestlam1)[1:98,]
selected_result = result[which(predict(out,type="coefficients",s=bestlam1)[1:98,]!=0)]
selected_result



#4.1
library(ISLR)
nci.data = NCI60$data
X = scale(t(nci.data))
P = X %*% prcomp(X)$rotation
hc = hclust(dist(X))
plot(hc,main="",xlab="",ylab="",sub="",cex=0.5)
table(cutree(hc,3))
table(cutree(hc,4))
table(cutree(hc,5))
table(cutree(hc,6))
Cols=function(vec){
cols=rainbow (length (unique(vec)))
return (cols[as.numeric(as.factor(vec))])
}
plot(P[,1:2], col =Cols(cutree(hc,3)),xlab ="P1-3 clusters",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc,4)),xlab ="P1-4 clusters",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc,5)),xlab ="P1-5 clusters",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc,6)),xlab ="P1-6 clusters",ylab="P2")



#4.2
cd=as.dist(cor(t(X)))
hc1 = hclust(cd)
plot(hc1, main=" ", xlab="", sub ="")
table(cutree(hc1,3))
table(cutree(hc1,4))
table(cutree(hc1,5))
table(cutree(hc1,6))
plot(P[,1:2], col =Cols(cutree(hc1,3)),xlab ="P1-3 clusters",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc1,4)),xlab ="P1-4 clusters",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc1,5)),xlab ="P1-5 clusters",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc1,6)),xlab ="P1-6 clusters",ylab="P2")


#4.3
km3 = kmeans(X,3,nstart=50)
table(km3$cluster)
km4 = kmeans(X,4,nstart=50)
table(km4$cluster)
km5 = kmeans(X,5,nstart=50)
table(km5$cluster)
km6 = kmeans(X,6,nstart=50)
table(km6$cluster)
plot(P[,1:2], col =Cols(km3$cluster),xlab ="P1-km3",ylab="P2")
plot(P[,1:2], col =Cols(km4$cluster),xlab ="P1-km4",ylab="P2")
plot(P[,1:2], col =Cols(km5$cluster),xlab ="P1-km5",ylab="P2")
plot(P[,1:2], col =Cols(km6$cluster),xlab ="P1-km6",ylab="P2")

par(mfrow=c(1,3))
plot(P[,1:2], col =Cols(cutree(hc,3)),xlab ="P1-hc",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc1,3)),xlab ="P1-hc1",ylab="P2")
plot(P[,1:2], col =Cols(km3$cluster),xlab ="P1-km3",ylab="P2")

par(mfrow=c(1,3))
plot(P[,1:2], col =Cols(cutree(hc,4)),xlab ="P1-4hc",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc1,4)),xlab ="P1-4hc1",ylab="P2")
plot(P[,1:2], col =Cols(km4$cluster),xlab ="P1-4km4",ylab="P2")

par(mfrow=c(1,3))
plot(P[,1:2], col =Cols(cutree(hc,5)),xlab ="P1-5hc",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc1,5)),xlab ="P1-5hc1",ylab="P2")
plot(P[,1:2], col =Cols(km5$cluster),xlab ="P1-5km5",ylab="P2")

par(mfrow=c(1,3))
plot(P[,1:2], col =Cols(cutree(hc,6)),xlab ="P1-6hc",ylab="P2")
plot(P[,1:2], col =Cols(cutree(hc1,6)),xlab ="P1-6hc1",ylab="P2")
plot(P[,1:2], col =Cols(km6$cluster),xlab ="P1-6km6",ylab="P2")



