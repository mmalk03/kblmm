# loading of the package and data

library(plsdepot)
library(calibrate)
# loading the data

linnerud <- read.table('~/Upc/kmlmm/lectures/9/linnerud.txt', header = T)
print(linnerud)

# forming of the predictor and response matrices

X <- linnerud[, 1:3]
Y <- linnerud[, 4:6]

print(X)
print(Y)

n <- nrow(X)
p <- ncol(X)
q <- ncol(Y)

# standardization of data

Xs <- as.matrix(scale(X))
Ys <- as.matrix(scale(Y))

options(digits=4)

# running AIB

iba <- interbat(X, Y, scaled = TRUE)

attributes(iba)

iba$values                                    # maximized cov2(t,u)
lmb <- iba$values


Vxy=var(Xs,Ys)
aib <- eigen(t(Vxy)%*%Vxy)
aib$values
aib$vectors

iba$y.wgs                                     # = B
B <- iba$y.wgs
t(B) %*% B


iba$x.wgs                                     # = A
A <- iba$x.wgs
t(A) %*% A


Vxy %*% iba$y.wgs %*% diag(lmb^(-0.5))



# IBA components

iba$x.scores                                  # = T
Xs %*% A
T <- iba$x.scores
var(T)


iba$y.scores                                  # = U
Ys %*% B
U <- iba$y.scores
var(U)

cov(T,U)
diag(cov(T,U))^2
iba$values                                    # maximized cov2(t,u)


#   properties

A
cov(Xs,U) %*% diag(lmb^(-0.5))

apply(cov(Xs,U)^2,2,sum)
lmb

B
cov(Ys,T) %*% diag(lmb^(-0.5))

apply(cov(Ys,T)^2,2,sum)
lmb


# correlations

iba$cor.xt
cor(X,T)

iba$cor.yt
cor(Y,T)

iba$cor.xu
cor(X,U)

iba$cor.yu
cor(Y,U)


corXY.t <- rbind(iba$cor.xt,iba$cor.yt)
corXY.u <- rbind(iba$cor.xu,iba$cor.yu)


# plot of correlations on t (not orthogonal)

plot(corXY.t,type="n",asp=1,ylim=c(-1,1),xlim=c(-1,1),main="correlations on t1, t2")
text(corXY.t,labels=rownames(corXY.t),col=c(rep(1,p),rep(2,q)),adj=1.1,cex=0.85)
abline(v=0,h=0,col="gray")
arrows(rep(0,(p+q)),rep(0,(p+q)),corXY.t[,1],corXY.t[,2],col=c(rep(1,p),rep(2,q)),length=0.07)



# plot of correlations on u (not orthogonal)

plot(corXY.u,type="n",asp=1,ylim=c(-1,1),xlim=c(-1,1),main="correlations on u1, u2")
text(corXY.u,labels=rownames(corXY.u),col=c(rep(1,p),rep(2,q)),adj=1.1,cex=0.85)
abline(v=0,h=0,col="gray")
arrows(rep(0,(p+q)),rep(0,(p+q)),corXY.u[,1],corXY.u[,2],col=c(rep(1,p),rep(2,q)),length=0.07)



# correlation of T and U
print(iba$cor.tu,digits=4)
print(cor(T,U),digits=4)


# looking the relation with the first two components

plot(T[,1],U[,1],type="n",asp=1,main="Relation t1 with u1")
text(T[,1],U[,1],label=rownames(X))
abline(lsfit(T[,1],U[,1]),col="blue")


# communalities

iba$cor.xt^2
t(apply(iba$cor.xt^2,1,cumsum))
iba$R2X

iba$cor.yu^2
t(apply(iba$cor.yu^2,1,cumsum))
iba$R2Y

# basis t is not orthogonal, then we can't compute directly cor.xt^2
# the communalities are the R2 coefficients


# computing the rank of Vxy

a <- sum(lmb > 0.0000001)

com.xt <- matrix(NA,p,a)

for (i in 1:p) {for ( j in 1:a) {com.xt[i,j] <- summary(lm(Xs[,i]~T[,1:j]))$r.squared}}
rownames(com.xt) <- names(X)
print(com.xt)

apply(com.xt,2,mean)


com.yu <- matrix(NA,q,a)

for (i in 1:q) {for ( j in 1:a) {com.yu[i,j] <- summary(lm(Ys[,i]~U[,1:j]))$r.squared}}
rownames(com.yu) <- names(Y)
print(com.yu)
apply(com.yu,2,mean)


# Redundancies

iba$com.yt

red.yt <- matrix(NA,q,a)

for (i in 1:q) {for ( j in 1:a) {red.yt[i,j] <- summary(lm(Ys[,i]~T[,1:j]))$r.squared}}
rownames(red.yt) <- names(Y)
print(red.yt)

apply(red.yt,2,mean)


iba$com.xu

red.xu <- matrix(NA,p,a)

for (i in 1:p) {for ( j in 1:a) {red.xu[i,j] <- summary(lm(Xs[,i]~U[,1:j]))$r.squared}}
rownames(red.xu) <- names(X)
print(red.xu)
apply(red.xu,2,mean)

# looking for the significant dimensionality

iba$statistic      # data is standardized

gd <- NULL
for (l in 1:a) {gd[l] <- (p-l+1)*(q-l+1)}
stat.val <- n*gd*rev(cumsum(rev(lmb)))/c(p*q,(p-cumsum(apply(T,2,var)))*(q-cumsum(apply(U,2,var))))[1:a]
p.val <- pchisq(stat.val,df=gd,lower.tail=FALSE)
tab.res <- data.frame(lmb,stat.val,gd,p.val)
print(tab.res,digits=4)


# by crossvalidation (approximate)
# confirming that one component is the best option

RMPRESS = matrix(NA,a,q)
R2cv = matrix(NA,a,q)
for (i in 1:a) {lmY <- lm(Ys~T[,1:i]-1)
                PRESS  <- apply((as.data.frame(lmY$residuals)/(1-ls.diag(lmY)$hat))^2,2,sum)
                RMPRESS[i,] <- sqrt(PRESS/n)
                R2cv[i,]   <- 1 - PRESS/(apply(Ys,2,var)*(n-1)) }

colnames(RMPRESS) = colnames(Y)
rownames(RMPRESS) = c(1:p)

print(RMPRESS,digits=4)

colnames(R2cv) = colnames(Y)
rownames(R2cv) = c(1:p)

print(R2cv,digits=4)

plot(apply(R2cv,1,mean),type="l")

nd <- 1

# prediction with 1 IBA component


lmY <- lm(Ys~T[,1:nd]-1)
summary(lmY)
summary(manova(lmY))


# coefficients as functions of the original variables

b.coef <- A[,1:nd] %*% lmY$coefficients
rownames(b.coef) <- names(X)
print(b.coef,digits=4)

cor(X,Y)

# R2 in training

lmY_R2 <- sapply(names(summary(lmY)), function(x) {summary(lmY)[[x]]$r.squared})
lmY_R2
mean(lmY_R2)

# R2 by LOO

PRESS  <- apply((as.data.frame(lmY$residuals)/(1-ls.diag(lmY)$hat))^2,2,sum)
(R2loo   <- 1 - PRESS/(apply(Ys,2,var)*(n-1)))
mean(R2loo)
