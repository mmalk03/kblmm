# PLS2 on Push-ups

library(pls)
library(calibrate)

# loading the data

linnerud <- read.table("D:/docent/DMKM/KBLMM/2019-2020/Session 5 pls2/linnerud.txt",header=T)
print(linnerud)

# forming of the predictorand response matrices

X <- linnerud[,1:3]
Y <- linnerud[,4:6]

print(X)
print(Y)


n <- nrow(X)
p <- ncol(X)
q <- ncol(Y)


# standardization of data 

Xs <- as.matrix(scale(X))
Ys <- as.matrix(scale(Y))


# PLS2

p2 <- plsr(Ys ~ Xs, validation = "LOO")

#summary(p2)

#compare
p2$loadings
lm(Xs~p2$scores-1)

#compare
p2$scores %*% t(p2$loadings)   # X = T * P'
Xs

# compare
p2$Yloadings
lm(Ys~p2$scores-1)

#compare
p2$scores %*% t(p2$Yloadings)
p2$fitted.values[,,3]

# compare
Xs %*% p2$coefficients[,,1]
p2$fitted.values[,,1]
Xs %*% p2$coefficients[,,2]
p2$fitted.values[,,2]
Xs %*% p2$coefficients[,,3]
p2$fitted.values[,,3]


# compare
p2$projection
lm(p2$scores~Xs-1)    # T = X * PROJ

# selecting the number of components

print(R2(p2))
plot(R2(p2), legendpos = "bottomright")

(R2.cv <- R2(p2)$val[1,,])
plot(apply(R2.cv,2,mean),type="l")


# confirming that 1 component is the best option (approximate)

RMPRESS = matrix(NA,3,q)
R2cv = matrix(NA,3,q)
for (i in 1:3) {lmY <- lm(Ys~p2$scores[,1:i]-1)
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

# prediction plot
plot(p2, ncomp = 1, asp = 1, line = TRUE, type="n")
text(p2$scores, labels=rownames(p2$scores))


plot(p2, plottype = "scores", comps = 1:2, type="n", main="X Scores")
text(p2$scores, labels=rownames(p2$scores))
abline(h=0,v=0,col="gray")


explvar(p2)

# loading plot
plot(p2, "loadings", comps = 1:3, legendpos = "topleft", labels = rownames(p2$loadings))
abline(h = 0)

# correlation plot

# plot of correlations

corXp2 <- cor(Xs,p2$scores)
corYp2 <- cor(Ys,p2$scores)
corXYp2 <- rbind(corXp2,corYp2)
plot(corXYp2,ylim=c(-1,1),xlim=c(-1,1),asp=1,type="n",main="Correlations with components")
text(corXYp2,labels=rownames(corXYp2),col=c(rep(1,p),rep(2,q)),adj=1.1,cex=0.85)
arrows(rep(0,(p+1)),rep(0,(p+1)),corXYp2[,1],corXYp2[,2],col=c(rep(1,p),rep(2,q)),length=0.07)
abline(h=0,v=0,col="gray")
circle()


# PLS2 Regression  fit of Y by the first PLS component

lmY <- lm(Ys~p2$scores[,1:nd]-1)
summary(lmY)

summary(manova(lmY))

p2$coefficients[,,nd]
cor(X,Y)

