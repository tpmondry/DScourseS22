library(tidyverse)
library(magrittr)
library(nloptr)
library(knitr)
library(modelsummary)

# setting seed
set.seed(100)

# generating data
X <- matrix(c(rep(1, 100000),
              rnorm(900000)),
            nrow = 100000)
eps <- rnorm(100000, sd = 0.5)

# setting beta and generating Y
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
y <- X %*% beta + eps

#####################################################################
########### generating estimates for beta:
#####################################################################

### 1. OLS estimate using matrices
bhat.matrix <- solve(t(X) %*% X) %*% t(X) %*% y

#####################################################################

### 2. OLS estimate using manual gradient descent

alpha <- 0.0000003 # step size
iter <- 500 #number of iterations
# to minimize squared residuals, the gradient:
gradient <- function(beta.try, X = X, y = y) return(as.vector(-2 * t(X) %*% (y - X %*% beta.try)))
# randomly initialize a vector of betas
set.seed(100)
beta.init <- floor(runif(n = 10, min = -5, max = 5)) # will be used later
beta.try <- beta.init

# create a vector to contain all betas for all steps
beta.try.All <- matrix(nrow = iter,
                       ncol = 10)
# gradient descent method to find the minimum
for(i in 1:iter){
  beta.try <- beta.try - alpha*gradient(beta.try, X = X, y = y)
  beta.try.All[i,] <- beta.try
}

# saving out final beta
bhat.gradient <- beta.try

#####################################################################

### 3. OLS estimate using nloptr's L-BFGS algorithm

# objective function
ssr <- function(bhat, X = X, y = y) return(sum((y - X %*% bhat)^2))
# gradient
ssr.grad <- function(bhat, X = X, y = y) return(as.vector(-2 * t(X) %*% (y - X %*% bhat)))
# parameters
nloptr.opts <- list("algorithm"="NLOPT_LD_LBFGS",
                    "xtol_rel"=1.0e-8,
                    "maxeval"=1e3)
# optimize
res.LBFGS <- nloptr(x0=beta.init, # value generated randomly in manual gradient descent setup 
                    eval_f=ssr, 
                    eval_grad_f=ssr.grad,
                    opts=nloptr.opts,
                    X = X, y = y)
# saving out final beta
bhat.OLS.LBFGS <- res.LBFGS$solution
iter.OLS.LBFGS <- res.LBFGS$iterations

#####################################################################

### 4. OLS estimate using nloptr's Nelder-Mead algorithm

# parameters
nloptr.opts <- list("algorithm"="NLOPT_LN_NELDERMEAD",
                    "xtol_rel"=1.0e-8,
                    "maxeval"=1e3)
# optimize
res.NelderMead <- nloptr(x0=beta.init, 
                         eval_f=ssr,
                         opts=nloptr.opts,
                         X = X, y = y)
# saving out final beta
bhat.OLS.NelderMead <- res.NelderMead$solution
iter.OLS.NelderMead <- res.NelderMead$iterations

#####################################################################

### 5. MLE using nloptr's L-BFGS algorithm

# objective
MLE.obj <- function(theta, X = X[,-1], y = y) {
  # need to slice our parameter vector into beta and sigma components
  beta <- theta[1:length(theta)-1]
  sig  <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum(-.5*(log(2*pi*(sig^2)) + ((y-X %*% beta)/sig)^2)) 
  return(loglike)
}

# gradient
MLE.grad <- function (theta, X = X[,-1], y = y) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta [1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X) %*% (Y - X %*% beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y - X %*% beta)/(sig^3)
  return (grad)
}

# initial values
theta0 <- runif(10) #start at uniform random numbers equal to number of coefficients

# params
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)


res.MLE.LBFGS <- nloptr(x0=theta0,
                        eval_f=MLE.obj,
                        eval_grad_f=MLE.grad,
                        opts=options,
                        X=X, y=y)
bhat.MLE.LBFGS <- res.MLE.LBFGS$solution[1:(length(res.MLE.LBFGS$solution)-1)]
iter.MLE.LBFGS <- res.MLE.LBFGS$iterations
sigmahat <- res.MLE.LBFGS$solution[length(res.MLE.LBFGS$solution)]

#####################################################################

### 6. OLS estimate the easy way
OLS.model <- lm(y ~ X-1)

#####################################################################
### output
#####################################################################

modelsummary(OLS.model)
