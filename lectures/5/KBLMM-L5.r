################################################
## Kernel-Based Learning & Multivariate Modeling
## MIRI Master - October 2019
################################################

set.seed(6046)
library(kernlab)

#################################################
# Example 1
# SVM for 2D Gaussian data with hand-made kernels
#################################################

# This initial code is heavily based on J.P. Vert's excelllent teaching material

# Original file is available at
#   http://cbio.ensmp.fr/~jvert/svn/tutorials/practical/makekernel/makekernel.R
# The corresponding note is available at
#   http://cbio.ensmp.fr/~jvert/svn/tutorials/practical/makekernel/makekernel_notes.pdf

## First we create a simple two-class data set:

N <- 200 # number of data points
d <- 2   # dimension
sigma <- 2  # variance of the distribution
meanpos <- 0 # centre of the distribution of positive examples
meanneg <- 3 # centre of the distribution of negative examples
npos <- round(N/2) # number of positive examples
nneg <- N-npos # number of negative examples

## Generate the positive and negative examples
xpos <- matrix(rnorm(npos*d,mean=meanpos,sd=sigma),npos,d)
xneg <- matrix(rnorm(nneg*d,mean=meanneg,sd=sigma),npos,d)
x <- rbind(xpos,xneg)

## Generate the class labels
t <- matrix(c(rep(1,npos),rep(-1,nneg)))

## Visualize the data
plot(x,col=ifelse(t>0,1,2))
legend("topleft",c('Pos','Neg'),col=seq(2),pch=1,text.col=seq(2))

## Now let's train a SVM with the standard (built-in) RBF kernel
## see help(kernels) for definition of this and other built-in kernels

## a) Let's start by computing the Gaussian RBF kernel manually
sigma <- 1
kk <- tcrossprod(x)
dd <- diag(kk)

## note that 'crossprod' and 'tcrossprod' are simply matrix multiplications (i.e., dot products)
## see help(crossprod) for details
## it is a function of two arguments x,y; if only one is given, the second is taken to be the same as the first

## make sure you understand why this computes the RBF kernel rather quickly
myRBF.kernel <- exp(-sigma*(matrix(dd,N,N)+t(matrix(dd,N,N))-2*kk))

dim(myRBF.kernel)

## the first 5 entries (note diagonal is always 1)
myRBF.kernel[1:5,1:5]

## Now we would like to train a SVM with our precomputed kernel

## We basically have two options in {kernlab}:

## either we explicitly convert myRBF.kernel to a 'kernelMatrix' object, and then ksvm() understands it
svm1 <- ksvm (as.kernelMatrix(myRBF.kernel), t, type="C-svc")

## or we keep it as a regular matrix and we add the kernel='matrix' argument
svm2 <- ksvm(myRBF.kernel,t, type="C-svc", kernel='matrix')

## b) This is how we would do normally do it (since this is a built-in kernel)

## kpar is the way to pass parameters to a kernel (called kernel parameters)
## WARNING: the ksvm() method scales the data by default; to prevent it, use scale=c()

svm3 <- ksvm(x,t, type="C-svc", kernel='rbf', kpar=list(sigma=1),scale=c())

## Now we compare the 3 formulations, they *should* be exactly the same

## Note also that the built-in version is faster (it is written in C code)

svm1
svm2
svm3

## Now we are going to make predictions with our hand-computed kernel

## First we split the data into training set and test set
ntrain <- round(N*0.8)     # number of training examples
tindex <- sample(N,ntrain) # indices of training samples

## Then we train the svm with our kernel over the training points
svm1.train <- ksvm (myRBF.kernel[tindex,tindex],t[tindex], type="C-svc",kernel='matrix')

## Let's call SV the set of obtained support vectors

## Then it becomes tricky. We must compute the test-vs-SV kernel matrix
## which we do in two phases:

# First the test-vs-train matrix
testK <- myRBF.kernel[-tindex,tindex]
# then we extract the SV from the train
testK <- testK[,SVindex(svm1.train),drop=FALSE]

# Now we can predict the test data
# Warning: here we MUST convert the matrix testK to a 'kernelMatrix'
y1 <- predict(svm1.train,as.kernelMatrix(testK))

# Do the same with the usual built-in kernel formulation
svm2.train <- ksvm(x[tindex,],t[tindex], type='C-svc', kernel='rbf', kpar=list(sigma=1), scale=c())

y2 <- predict(svm2.train,x[-tindex,])

# Check that the predictions are the same
table(y1,y2)

# Check the real performance
table(y1, t[-tindex])
cat('Error rate = ',100*sum(y1!=t[-tindex])/length(y1),'%')


################################################
# Example 2
# SVM for text classification with a string kernel
################################################

## We are going to use a slightly-processed version of the famous
## Reuters news articles dataset.  All articles with no Topic
## annotations are dropped. The text of each article is converted to
## lowercase, whitespace is normalized to single-spaces.  Only the
## first term from the Topic annotation list is retained (some
## articles have several topics assigned).  

## The resulting dataset is a list of pairs (Topic, News). We will use three topics for analysis: Crude Oil, Coffee and Grain-related news

## The resulting data frame contains 994 news items on crude oil,
## coffee and grain. The news text is the column "Content" and its
## category is the column "Topic". The goal is to create a classifier
## for the news articles.

## Note that we can directly read the compressed version (reuters.txt.gz). 
## There is no need to unpack the gz file; for local files R handles unpacking automagically

reuters <- read.table("/home/malkinskim/Upc/kmlmm/lectures/5/reuters.txt.gz", header=T)

# We leave only three topics for analysis: Crude Oil, Coffee and Grain-related news
reuters <- reuters[reuters$Topic == "crude" | reuters$Topic == "grain" | reuters$Topic == "coffee",]

reuters$Content <- as.character(reuters$Content)    # R originally loads this as factor, so needs fixing
reuters$Topic <- factor(reuters$Topic)              # re-level the factor to have only three levels

levels(reuters$Topic)

length(reuters$Topic)

table(reuters$Topic)

## an example of a text about coffee
reuters[2,]

## an example of a text about grain
reuters[7,]

## an example of a text about crude oil
reuters[12,]

(N <- dim(reuters)[1])  # number of rows

# we shuffle the data first
set.seed(12)
reuters <- reuters[sample(1:N, N),]

# To deal with textual data we need to use a string kernel. Several such kernels are implemented in the "stringdot" method of the kernlab package. We shall use the simplest one: the n-spectrum kernel. The feature map represents the string as a multiset of its substrings of length n

# Example, for n=2 we have

# phi("ababc") = ("ab" -> 2, "ba" -> 1, "bc" --> 1, ... other -> 0)

# we can define a normalized 3-spectrum kernel (n is length)
k <- stringdot("spectrum", length=3, normalized=TRUE)

# Let's see some examples:

Frank.Sinatra <- "I did it my way"

k(Frank.Sinatra, Frank.Sinatra)

k(Frank.Sinatra, "He did it his way")

k(Frank.Sinatra, "She did it her way")

k(Frank.Sinatra, "Let's find our way out")

k(Frank.Sinatra, "Brexit means Brexit")

## We start by doing a kPCA (we'll see this in the next lecture)

## first we define a modified plotting function 

plotting <-function (kernelfu, kerneln)
{
  xpercent <- eig(kernelfu)[1]/sum(eig(kernelfu))*100
  ypercent <- eig(kernelfu)[2]/sum(eig(kernelfu))*100
  
  plot(rotated(kernelfu), col=as.integer(reuters$Topic),
       main=paste(paste("Kernel PCA (", kerneln, ")", format(xpercent+ypercent,digits=3)), "%"),
       xlab=paste("1st PC -", format(xpercent,digits=3), "%"),
       ylab=paste("2nd PC -", format(ypercent,digits=3), "%"))
}

## Create a kernel matrix using 'k' as kernel (it takes a couple of mins)

k <- stringdot("spectrum", length=5, normalized=T)
K <- kernelMatrix(k, reuters$Content)
dim(K)

K[2,2]

K[2,3:10]

## Plot the result using the first 2 PCs (we can add colors for the two classes)

kpc.reuters <- kpca (K, features=2, kernel="matrix")
plotting (kpc.reuters,"5 - spectrum kernel")

## finally add a legend
legend("topleft", legend=c("crude oil", "coffee","grain"),    
       pch=c(1,1),                    # gives appropriate symbols
       col=c("red","black", "green")) # gives the correct color

## We can also train a SVM using this kernel matrix in the training set

## First we should split the data into learning (2/3) and test (1/3) parts
ntrain <- round(N*2/3)     # number of training examples
tindex <- sample(N,ntrain) # indices of training examples
  
## The fit a SVM in the train part
svm1.train <- ksvm (K[tindex,tindex],reuters$Topic[tindex], type="C-svc", kernel='matrix')

## and make it predict the test part

## Let's call SV the set of obtained support vectors

## Then it becomes tricky. We must compute the test-vs-SV kernel matrix
## which we do in two phases:

# First the test-vs-train matrix
testK <- K[-tindex,tindex]
# then we extract the SV from the train
testK <- testK[,SVindex(svm1.train),drop=FALSE]

# Now we can predict the test data
# Warning: here we MUST convert the matrix testK to a 'kernelMatrix'
y1 <- predict(svm1.train,as.kernelMatrix(testK))

table (pred=y1, truth=reuters$Topic[-tindex])

cat('Error rate = ',100*sum(y1!=reuters$Topic[-tindex])/length(y1),'%')

## now we define a 3D plotting function

library("rgl")
open3d()

plotting3D <-function (kernelfu, kerneln)
{
  xpercent <- eig(kernelfu)[1]/sum(eig(kernelfu))*100
  ypercent <- eig(kernelfu)[2]/sum(eig(kernelfu))*100
  zpercent <- eig(kernelfu)[3]/sum(eig(kernelfu))*100
  
  # resize window
  par3d(windowRect = c(100, 100, 612, 612))
  
  plot3d(rotated(kernelfu), 
         col  = as.integer(reuters$Topic),
         xlab = paste("1st PC -", format(xpercent,digits=3), "%"),
         ylab = paste("2nd PC -", format(ypercent,digits=3), "%"),
         zlab = paste("3rd PC -", format(zpercent,digits=3), "%"),
         main = paste("Kernel PCA"), 
         sub = "red - crude oil | black - coffee | green - grain",
         top = TRUE, aspect = FALSE, expand = 1.03)
 }

kpc.reuters <- kpca (K, features=3, kernel="matrix")
plotting3D (kpc.reuters,"5 - spectrum kernel")

#########################################
# Example 3
### Basic diffusion kernel on a toy graph
#########################################

library(igraph)
library(diffuStats)
library(expm)

options (digits=3)

g <- graph_from_literal (Fred-Bobby-Georgina-Fred, Fred-Charlie, Bobby-David-Anne-Georgina)
g
plot(g)

S <- diag(6)
names <- c("Fred","Bobby","Georgina","Charlie","David","Anne")
dimnames (S) <- list(names,names)

# Strength of relationships
S["Fred","Bobby"] <- 0.1
S["Fred","Georgina"] <- 0.3
S["Fred","Charlie"] <- 0.2

S["Bobby","Georgina"] <- 0.9
S["Bobby","David"] <- 0.4

S["Georgina","Anne"] <- 0.7

S["David","Anne"] <- 0.5

# make it symmetric

S[lower.tri(S)] = t(S)[lower.tri(S)]
S

lambda <- 0.5
K <- expm(lambda*S)

K

eigen(S, only.values = TRUE)$values # S is not even psd
eigen(K, only.values = TRUE)$values # K is pd

# but better normalize it

normalize.kernel = function(K)
{
  k = 1/sqrt(diag(K))
  K * (k %*% t(k))
}

K.n <- normalize.kernel(K)

rowSums(S)
rowSums(K.n)

#########################################
# Example 4
### Community detection in graphs
#########################################

g <- generate_graph(fun_gen = igraph::barabasi.game,
                    param_gen = list(n = 100, m = 3, directed = FALSE),
                    seed = 1)

com <- cluster_spinglass(g, spins=3)
V(g)$color <- com$membership
g <- set_graph_attr(g, "layout", layout_with_kk(g))
plot(g, vertex.label.dist=1)
colorets <- c("skyblue2","orange3","green4")
legend("bottomleft",c('Statistics','CompSci','Math'),col=colorets,pch=1,text.col=colorets)

K.diff <- diffusionKernel(g, 1)
is_kernel(K.diff)
hist(eigen(K.diff, only.values = TRUE)$values, breaks = 30)

K.diff[1:7,1:7]

K.norm <- normalize.kernel(K.diff)

is_kernel(K.norm)

K.norm[1:7,1:7]

K.svm <- as.kernelMatrix(K.norm)
t <- factor(V(g)$color)

save(K.norm,t, file="graph-kernels")
load(file="graph-kernels")

K.svm <- as.kernelMatrix(K.norm)

ksvm (K.svm, y=t, cross=length(t))
