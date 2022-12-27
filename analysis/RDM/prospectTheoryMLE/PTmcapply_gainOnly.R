# Create wrapper around Prospect Theory estimation function for parallelization 
# gain only model (rho and mu, lamda constrained to 1)
# Hayley Brooks
# December 2022

library('config')
config = config::get()

# load Prospect Theory Function
#need to fix configuration file
nllFullModel_source = file.path("PTnll_fullModel.R")
nllGainOnly_source = file.path("PTnll_gainOnly.R")
source(nllFullModel_source) #, local = knitr::knit_global())
source(nllGainOnly_source) #, local = knitr::knit_global())




eps = .Machine$double.eps;
#estimation_lowerbound = c(eps,eps);# lower bound of parameter values is machine precision above zero
estimation_lowerbound = c(.01,.01); # this lower bound seems to work best (lower or higher breaks for some participants)
estimation_upperbound = c(1.3,20); #upper bounds on parameter values: rhogain, mu

# Wrap the our PT likelihood and probability functions into one that can be individual sent to each core via mclapply
parallel_ptLL_gainOnly <- function(n,subjdata){
  alloutput <- list() # Prepare the object into which we're going to put all the outputs of each interaction
  for(i in 1:n){ # n is iteration for each core
    lb = estimation_lowerbound; # lower bound
    ub = estimation_upperbound;  # upper bound
    init_parval = c(runif(1,.1,1.5), runif(1,.2,10)); #set initial values on each iteration of the loop
    alloutput[[i]] <- NA
    try({output = optim(init_parval, ptLL_gainOnly, data=subjdata, method= "L-BFGS-B", lower=estimation_lowerbound, upper=estimation_upperbound, hessian=TRUE);
    alloutput[[i]] <- output}); # Save the output into a bigass list
  };
  return(alloutput) # return the big list
};
