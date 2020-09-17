

####################################################################################
#### Code loads data, and uses the function 'BNSPfunc' to yield results from the BNSP model.
####################################################################################

rm(list=ls())
####################################################################################
#### Load Simulated Data     ############################################################
load("Simu1Case1Data.Rdata") 
#### Xmat (Predictor Matrix) and y (continuous response vector) are loaded.
Xmat <- simdata$Xmat
y <- simdata$y
## For later comparisons, load 'true.b'
true.b <- simdata$true.b
bin.gen <- simdata$bin.gen

####  Example values below, and what they mean...
####  V <- 20 ## No. of nodes for simulations
####  R <- 5 ## Maximum Dimensionality (Varies with simulation settings)
####  niter <- 50000  ## No. of iterations
####################################################################################
#### Run the model 
####################################################################################

source("BNSP-Function.R") #### loads function 'BNSPfunc'
outlist <- BNSPfunc(Xmat = Xmat, y = y, V = 20, R = 5, niter = 50000)
#### Compute model performance (for inference) metrics.
####################################################################################
####  ***Post-Analysis Code for Simulated Scenario***      
####################################################################################

burnin <- 30000
postburn <- 20000
niter <- 50000

#### (1) MSE - Mean Squared Error of edge coefficients
q <- outlist$q
betamat <- outlist$betamat

betasample <- matrix(NA,postburn,q)
for(i in 1:postburn){
    betasample[i,] <- upperTriangle(betamat[i+burnin,,],byrow=T)
}
MSE <- mean((colMeans(betasample) - true.b)^2)
MSE

#### (2) Which Nodes Identified
bin.gen #### Truth
rn.gen <- outlist$rn.gen
colMeans(rn.gen[(burnin+1):niter,]) #### Detected Nodes if greater than 0.5
length(which(colMeans(rn.gen[(burnin+1):niter,])>0.5))
