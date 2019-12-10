# ---------------------------------------------------------------------------------------
# FUNCTION tab.val to print the table with the eigenvalues and percentages of inertia
# ---------------------------------------------------------------------------------------

tab.val <- function(val){
        Tot.in <- sum(val)
        tab.valprop <- data.frame(val,(100*val/Tot.in),cumsum(100*val/Tot.in))
        names(tab.valprop) <- c("eigenvalue", "% explained", "% cumulated")
        cat("Total inertia:",Tot.in,sep="  ","\n")
        print(tab.valprop,digits=4)  }

# ---------------------------------------------------------------------------------------
# FUNCTION plot.ind to plot the individuals in a PCA analysis
# ---------------------------------------------------------------------------------------

plot.ind <- function(pc,nd=0) {
         if (nd == 0) nd=ncol(pc)
         Psi = pc[,1:nd]
         iden = rownames(Psi)
         plot(Psi[,1],Psi[,2],type="n",asp=1,main="Plot of individuals")
         text(Psi[,1],Psi[,2],labels=iden)
         abline(h=0,v=0,col="cyan") }



# ---------------------------------------------------------------------------------------
# FUNCTION plot.var to plot the variables in a PCA analysis
# ---------------------------------------------------------------------------------------

plot.cor <- function(Phi,nd=0) {
         if (nd == 0) nd=ncol(Phi)
         a = seq(-1,1,0.01)
         b = sqrt(1-a^2)
         a = c(a,a)
         b = c(b,-b)
         etiq = rownames(Phi)
         ze = rep(0,length(etiq))
         adjx = 0.5-sign(sum(Phi[,1]))*0.5
         plot(Phi[,1],Phi[,2],ylim=c(-1,1),xlim=c(-1,1),asp=1,type="n",main="Plot of correlations of variables")
         abline(h=0,v=0, col="gray")
         arrows(ze, ze, Phi[,1], Phi[,2], length = 0.07, col="blue")
         text(Phi[,1],Phi[,2],labels=etiq, col="blue", adj=adjx)
         lines(a,b)  }


# PLS1 on Octane data

library(pls)
library(calibrate)
library(plsdepot)

# working path
setwd("D:/docent/DMKM/KBLMM/2019-2020/Session 4 nipals+pls1")


# loading the data

# PLS1 Octane data

gasoline <- read.table("gasoline.txt",header=T)

class(gasoline)

dim(gasoline)

names(gasoline)

head(gasoline)

(n  <- nrow(gasoline))
(p1 <- ncol(gasoline))

NIR <- gasoline[,2:p1]
octane <- gasoline[,1]

# looking at the data
library(lattice)

xyplot(as.ts(t(NIR)), screens = 1, col=c(rep(1,50),rep(2,10)),main="Gasoline NIR, 50 train + 10 test (red)")


# separating the test matrix (holdout test) for validation purposes

gasTrain <- gasoline[1:50, ]
gasTest <- gasoline[51:60,]


# forming the response and the predictor matrix

y <- gasTrain[,1]
X <- gasTrain[,2:402]

yt <- gasTest[,1]
Xt <- gasTest[,2:402]

(n_train <- nrow(X))
(n_test  <- nrow(Xt))
(p       <- ncol(X))

########################################################################################
nip <- nipals(X,scaled=F,comps=min(p,(n_train-1)))

attributes(nip)

plot(nip$values[,1],type="l",main="Screeplot")

tab.val(nip$values[,1])   # printing the table of the eigenvalues

# we take 4 dimensions

nd = 4

plot.ind(nip$scores)  # plot of individuals

# Grafting the Test data

Xtc <- scale(Xt, center=apply(X,2,mean), scale=F)

Psit <- Xtc %*% nip$loadings[,1:nd]

plot.ind(nip$scores)
text(Psit,labels=rownames(Xt),col="red")  # plot of test individuals only (as supplementary)



# plot of correlation map of variables

plot.cor(nip$cor.xt)


# plot of octane variable in the space of variables (in train and test data)

cor_y <- cor(y,nip$scores[,1:nd])
cor_yt <- cor(yt,Psit[,1:nd])

ze = rep(0,length(cor_y))
plot.cor(nip$cor.xt)
abline(h=0, v=0, col="gray")
arrows(ze, ze, cor_y[1], cor_y[2], length = 0.07)
         text(cor_y[1], cor_y[2],labels="octane_train")
arrows(ze, ze, cor_yt[1], cor_yt[2], length = 0.07, col="red")
         text(cor_yt[1], cor_yt[2],labels="octane_test", col="red")

# plot of correlation of variables with components as time series data

xyplot(as.ts(nip$cor.xt[,1:nd]), screens=1, col=1:4, main="Correlations of NIR with Nipals components")
abline(0,0)
legend("bottomleft",c("1st comp","2nd comp", "3rd comp", "4th comp"),col=c(1:4),lty=1)


# in NIPALS, varaince is divides by (n-1), but in PCA is divided by (n)
#pc <- PCA(gasoline,scale.unit=FALSE,ind.sup=51:60,quanti.sup=1)

#######################################################################################
# Multivariate regression

r=lm(octane~.,data=gasTrain)    # raw data, non standardized
summary(r)


# why do we obtain such number of NA coefficients?

coef <- r$coefficients[!is.na(r$coefficients)]
coef

nc <- length(coef)
nc

# preditction in train

r$fitted.values
y

summary(r)$r.squared


# prediction of octane in test data

rt = as.matrix(cbind(rep(1,10),Xt[,1:(nc-1)])) %*% coef
rt
yt

# plot of true values and predcited values in test data

plot(yt,ylim=c(min(yt,rt),max(yt,rt)),pch=20,main="Actual and predicted Octane values in test data")
points(rt,pch=20,col="red")
abline(mean(y),0,col="gray")
legend(mean(y),"Mean of Octane in train",bty="n")


# R2 in test sample
1-sum((octane[51:60]-rt)^2)/sum((octane[51:60]-mean(octane[51:60]))^2)


# lets select the most predictive frequencies (in train)

library(FactoMineR)
sig_con <- condes(gasTrain,1,proba=0.0001)

predictors <- rownames(sig_con$quanti)

predictors

length(predictors)   # they should be less than 50


formula <- as.formula(paste("octane~",paste(predictors,collapse="+")))


reg <- lm(formula,data=gasTrain)

summary(reg)

pred_reg <- predict(reg,newdata=gasTest)

# R2 in test sample
1-sum((octane[51:60]-pred_reg)^2)/sum((octane[51:60]-mean(octane[51:60]))^2)



# PCR

pc <- pcr(octane ~ ., ncomp = 10, data = gasoline[1:50,], validation = "LOO")

attributes(pc)
summary(pc)

# selection of PCR components
plot(RMSEP(pc), legendpos = "topright")

R2(pc)
plot(R2(pc), legendpos = "bottomright")

# we take 3 PCR components
nd = 3

#plot of individuals in the PCA components
plot(pc, plottype = "scores", comps = 1:2, type="n", asp=1, main="X Scores")
text(pc$scores, labels=rownames(pc$scores))
abline(h=0,v=0, col="gray")


# prediction plot
plot(pc, ncomp = nd, asp = 1, line = TRUE, type="n")
text(y, pc$fitted.values[,,nd], labels=rownames(X))


# loading plot
plot(pc, "loadings", comps = 1:nd, legendpos = "topleft", labels = rownames(pc$loadings), main="Loading plot")
abline(h = 0)


# plot of correlations

p <- ncol(X)

corXpc <- cor(X,pc$scores)
corypc <- cor(y,pc$scores)
rownames(corypc) = "Octane"
corXypc <- rbind(corXpc,corypc)
plot(corXypc,ylim=c(-1,1),xlim=c(-1,1),asp=1,type="n",main="Correlations with components")
text(corXypc,labels=rownames(corXypc),col=c(rep(1,p),rep(2,1)),adj=1.1,cex=0.45)
arrows(rep(0,(p+1)),rep(0,(p+1)),corXypc[,1],corXypc[,2],col=c(rep(1,p),rep(2,1)),length=0.07)
abline(h=0,v=0, col="gray")
circle()

xyplot(as.ts(corXpc[,1:nd]), screens=1, col=1:nd, main="Correlations of NIR with PCR components")
abline(0,0)
legend("bottomleft",c("1st comp","2nd comp", "3rd comp"),col=c(1:nd),lty=1)



# the PCR model

lmY <- lm(y~pc$scores[,1:nd])
summary(lmY)



# validation on the test sample

pred_test= predict(pc, ncomp = nd, newdata = gasTest)

# prediction plot
plot(pc, ncomp = 3, asp = 1, line = TRUE)
text(octane[51:60],pred_test,labels=c(51:60),col="red")

summary(lmY)$r.squared

# R2 in the test sample
R2val <- 1-sum((octane[51:60]-pred_test)^2)/sum((octane[51:60]-mean(octane[51:60]))^2)
print(R2val, digits=4)



# PLS1


p1 <- plsr(octane ~ ., ncomp = 10, data = gasoline[1:50,], validation = "LOO")

attributes(p1)
summary(p1)


n <- nrow(X)
p <- ncol(X)

# selecting the number of components

plot(RMSEP(p1), legendpos = "topright")

R2(p1)
plot(R2(p1), legendpos = "bottomright")

# we take two PLS components
nd = 2


#compare   Loadings
lm(as.matrix(scale(X,scale=F))~p1$scores-1)$coefficients[1:10,1:5]
t(p1$loadings)[1:10,1:5]


#compare    Yloadings
lm(scale(y,scale=F)~p1$scores-1)$coefficients
p1$Yloadings

#compare    
mean(y)+p1$scores[,1:nd] %*% t(p1$Yloadings)[1:nd]
mean(y)+as.matrix(scale(X,scale=F)) %*% p1$coefficients[,,nd]
p1$fitted.values[,,nd]

# plot scores
plot(p1, plottype = "scores", comps = 1:2, type="n", main="X Scores")
text(p1$scores, labels=rownames(p1$scores))
abline(h=0,v=0, col="gray")


# prediction plot
plot(p1, ncomp = nd, asp = 1, line = TRUE, type="n")
text(y, p1$fitted.values[,,nd], labels=rownames(X))


# loading plot
plot(p1, "loadings", comps = 1:2, legendpos = "topleft", labels = rownames(p1$loadings),main="Loading plot")
abline(h = 0)


# plot of correlations

corXp1 <- cor(X,p1$scores)
coryp1 <- cor(y,p1$scores)
rownames(coryp1) = "Octane"
corXyp1 <- rbind(corXp1,coryp1)
plot(corXyp1,ylim=c(-1,1),xlim=c(-1,1),asp=1,type="n",main="Correlations of variables with components")
#text(corXyp1,labels=rownames(corXyp1),col=c(rep(1,p),rep(2,1)),adj=1.1,cex=0.85)
arrows(rep(0,(p+1)),rep(0,(p+1)),corXyp1[,1],corXyp1[,2],col=c(rep(1,p),rep(2,1)),length=0.07)
abline(h=0,v=0, col="gray")
circle()

xyplot(as.ts(corXp1[,1:2]), screens=1, col=1:2, main="Correlations of NIR with PLS1 components")
abline(0,0)
legend("bottomleft",c("1st comp","2nd comp"),col=c(1:2),lty=1)


# the PLS1 model


lmY <- lm(y~p1$scores[,1:nd])
summary(lmY)



# validation on the test sample

pred_test <- predict(p1, ncomp = nd, newdata = gasTest)

# prediction plot
plot(p1, ncomp = nd, asp = 1, line = TRUE)
text(octane[51:60],pred_test,labels=c(51:60),col="red")

summary(lmY)$r.squared


# R2 in the test sample
R2val <- 1-sum((octane[51:60]-pred_test)^2)/sum((octane[51:60]-mean(octane[51:60]))^2)
print(R2val, digits=4)



