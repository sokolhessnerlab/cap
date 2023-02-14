# Model Fitting Time for Stan models of RDM data from CAP

This file catalogues the time it took to fit various Stan models to the RDM dataset from CAP. These times were collected using Peter's Macbook Air. The purpose is to evaluate the impact of various modeling steps on fitting time so as to understand better how to minimize the time spent fitting with MCMC models.

Fit times are on the basis of 2 parallel chains, 100 samples each. 

Dataset consists of 114,800 observations (trials or choices). 

## Results

### Model: `cap_basic0_rhoMuLambdaDB_allFFX.stan`

Contains FFX parameters for R, L, M, and DB (no hierarchy, single terms only, broad priors). 

**TIMING:** Compilation: 9.2 seconds; Fitting: 22.1 minutes.

_Fitting the **same model** with ~HALF the data (59,988 datapoints; 52.3%) yielded a fitting time of roughly **half: 11.4 minutes**._
 

### Model: `cap_basic0_rhoMuLambdaDB_dbRFX.stan`

Contains FFX parameters for R, L, and M (no hierarchy, single terms only, broad priors). Contains RFX hierarchical parameters for DB (a mean, SD, and then _nsubj_ individuals' parameters).

**TIMING:** Compilation: 10.9 seconds; Fitting: 22.3 minutes. 

### Model: `cap_basic0_rhoMuLambdaDB_allRFX.stan`

Contains RFX hierarchical parameters for R, L, M, and DB (a mean, SD, and then _nsubj_ individuals' parameters).

**TIMING:** Compilation: 11.5 seconds; Fitting: 11.3 minutes. 


**NOTE:** Times are not reliable, for some reason... Unclear why. 