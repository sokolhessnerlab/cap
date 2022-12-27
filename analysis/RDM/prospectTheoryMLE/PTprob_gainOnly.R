# Function for just probability

ptProb_gainOnly <-function(parvals, data){ #parvals and data are the two inputs ptLL needs to work
  eps = .Machine$double.eps;
  
  lambda = 1; #paramter 1 is loss aversion
  rho = parvals [2]; #parameter 2 is risk aversion for gains
  mu = parvals [3]; #parameter 3 is choice consistency
  
  x = data$rdmRiskyGain; #find x in the riskyGain column of given data
  y = data$rdmRiskyLoss; #find y in riskyLoss column of given data
  z = data$rdmAlternative; #find z in alternative column of given data
  
  gamble = ((x^rho)*.5)  + ((-lambda * (-y)^rho)*.5); #utility for gamble
  
  guaranteed = vector(); # create an empty vector for guaranteed since we do subsetting below
  guaranteed[z>=0] = z[z>=0]^rho; # if safe >= 0
  
  
  prob = (1+exp(-mu*(gamble-guaranteed)))^-1; #probabilty of choosing gamble over alternative
  
  prob[prob==1]=1-eps;#indexing, any number in prob that equals 1, set it to 1-eps
  prob[prob==0]=eps; #indexing, any number in prob that equals 0, set it to eps
  
  return(prob)
};







