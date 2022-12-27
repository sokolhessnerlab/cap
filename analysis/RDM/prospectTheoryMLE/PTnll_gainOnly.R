# Prospect Theory Function for CAP data
# Hayley Brooks
# December 2022


# Create a wrapper around ptLL  fucntion
# nll Fx constraining lambda to 1

ptLL_gainOnly<-function(parvals, data){
  ll <- ptLL(c(1,parvals), data) #need to give ptLL 2 inputs: parvals and data
  return(ll)
};
