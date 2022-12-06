# Prospect Theory Function for CAP data
# Hayley Brooks
# December 2022


# Create a nll fucntion

          ptLL <-function(parvals, data){ #parvals and data are the two inputs ptLL needs to work
            eps = .Machine$double.eps;
            
            lambda = parvals [1]; #paramter 1 is loss aversion
            rhoGain = parvals [2]; #parameter 2 is risk aversion for gains
            #rhoLoss = parvals[3]; # parameter 3 is risk aversion for losses
            mu = parvals [3]; #parameter 3 is choice consistency 
            
            x = data$riskyGain; #find x in the riskyGain column of given data
            y = data$riskyLoss; #find y in riskyLoss column of given data
            z = data$safe; #find z in alternative column of given data
            
            gamble = ((x^rhoGain)*.5)  + ((-lambda * (-y)^rhoLoss)*.5); #utility for gamble 
            
            guaranteed = vector(); # create an empty vector for guaranteed since we do subsetting below
            guaranteed[z>=0] = z[z>=0]^rhoGain; # if certain >= 0
            #guaranteed[z<0] = -lambda*(-z[z<0])^rhoLoss; # if certain < 0
            

            prob = (1+exp(-mu*(gamble-guaranteed)))^-1; #probabilty of choosing gamble over alternative
            
            prob[prob==1]=1-eps;#indexing, any number in prob that equals 1, set it to 1-eps
            prob[prob==0]=eps; #indexing, any number in prob that equals 0, set it to eps
            
            ll <- sum(log(data$choice*prob + (1-data$choice)*(1-prob))); #find the log likelihood 
            nll = -ll; # because the log of a number between 0 and 1 is negative (making the log likelihood
            # a negative number that we'd have to maximize (i.e. make the least negative possible), 
            # and most estimation procedures ask for MINIMIZATION problems, we take the negative to flip this, 
            # allowing us to MINIMIZE the NEGATIVE log likelihood
            return(nll);
          };

          
        

# Function for just probability

          ptProb <-function(parvals, data){ #parvals and data are the two inputs ptLL needs to work
            eps = .Machine$double.eps;
            
            lambda = parvals [1]; #paramter 1 is loss aversion
            rhoGain = parvals [2]; #parameter 2 is risk aversion for gains
            #rhoLoss = parvals[3]; # parameter 3 is risk aversion for losses
            mu = parvals [3]; #parameter 3 is choice consistency 
            
            x = data$riskyGain; #find x in the riskyGain column of given data
            y = data$riskyLoss; #find y in riskyLoss column of given data
            z = data$safe; #find z in alternative column of given data
            
            gamble = ((x^rhoGain)*.5)  + ((-lambda * (-y)^rhoLoss)*.5); #utility for gamble 
            
            guaranteed = vector(); # create an empty vector for guaranteed since we do subsetting below
            guaranteed[z>=0] = z[z>=0]^rhoGain; # if safe >= 0
            #guaranteed[z<0] = -lambda*(-z[z<0])^rhoLoss; # if safe < 0
            
            
            prob = (1+exp(-mu*(gamble-guaranteed)))^-1; #probabilty of choosing gamble over alternative
            
            prob[prob==1]=1-eps;#indexing, any number in prob that equals 1, set it to 1-eps
            prob[prob==0]=eps; #indexing, any number in prob that equals 0, set it to eps
            
            return(prob)
          };



          

    

    