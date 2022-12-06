# Create wrapper around Prospect Theory estimation functions for parallelization
# Hayley Brooks
# December 2022

library('config')
config = config::get()

# load Prospect Theory Functions
setup_source = file.path(config$pt_modeling$ptFunction)
source(setup_source) #, local = knitr::knit_global())



eps = .Machine$double.eps;
#estimation_lowerbound = c(eps,eps,eps,eps);
estimation_lowerbound = c(.5,.3,.1); # lower bound of parameter values is machine precision above zero
estimation_upperbound = c(5,1.3,,20); # sensible upper bounds on parameter values: lambda, rhogain, mu

# Wrap the our PT likelihood and probability functions into one that can be individual sent to each core via mclapply
parallel_ptLL <- function(n,subjdata){
  alloutput <- list() # Prepare the object into which we're going to put all the outputs of each iteraction
  for(i in 1:n){ # n is iteration for each core
    lb = estimation_lowerbound; # lower bound
    ub = estimation_upperbound;  # upper bound
    init_parval = c(runif(1,.1,5), runif(1,.1,1.5), runif(1,.2,10)); #set initial values on each iteration of the loop
    alloutput[[i]] <- NA
    try({output = optim(init_parval, ptLL, data=subjdata, method= "L-BFGS-B", lower=estimation_lowerbound, upper=estimation_upperbound, hessian=TRUE);
    alloutput[[i]] <- output}); # Save the output into a bigass list
  };
  return(alloutput) # return that bigass list
};

