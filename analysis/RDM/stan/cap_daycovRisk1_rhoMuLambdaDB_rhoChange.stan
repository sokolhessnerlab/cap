data {
  int N; // number of trials (across participants)
  int nsubj; // number of participants
  int choices[N]; // choice vector
  real gain[N]; // risky gain vector
  real loss[N]; // risky loss vector (loss MAGNITUDE! values should be strictly positive)
  real safe[N]; // safe vector
  int ind[N]; // subject index
  real day[N]; // day (overall, scaled 0-1)
  real covRisk[N]; // perceived Covid risk (scaled -1:+1, no NAs)
}

parameters {
  real meanRho;
  real<lower=0> sdRho;
  real meanMu;
  real<lower=0> sdMu;
  real meanLambda;
  real<lower=0> sdLambda;
  real meanDB;
  real<lower=0> sdDB;

  real r[nsubj]; // random effects for rho
  real m[nsubj]; // random effects for mu
  real l[nsubj]; // random effects for lambda
  real db[nsubj]; // random effects for decision bias

  real dayRho;
  real covRiskRho;
  real dayxcovRiskRho;
}

transformed parameters {
  real rtmp[N];
  real mtmp[N];
  real ltmp[N];

  for(t in 1:N) { // for each trial
    rtmp[t] = exp(r[ind[t]] + day[t] * dayRho + covRisk[t] * covRiskRho + day[t] * covRisk[t] * dayxcovRiskRho); // take individual-level rho sample (that was sampled in unbounded space) and put it in the exponential to make it >0
    mtmp[t] = exp(m[ind[t]]); // same as above for mu
    ltmp[t] = exp(l[ind[t]]); //same for lambda
  } // IF this loop is super slow, could maybe be re-factored?
}

model {
  real div;
  real p[N];
  real total_sum[N];

  //Priors
  meanRho ~ normal(0,30);
  sdRho ~ cauchy(0,2.5);
  meanMu ~ normal(0,30);
  sdMu ~ cauchy(0,2.5);
  meanLambda ~ normal(0,30);
  sdLambda ~ cauchy(0,2.5);
  meanDB ~ normal(0,30);
  sdDB ~ cauchy(0,2.5);

  dayRho ~ normal(0,5); // psychologically plausible range for changes in Rho given exp() transformation
  covRiskRho ~ normal(0,5);
  dayxcovRiskRho ~ normal(0,5);

  //Hierarchy
  r ~ normal(meanRho, sdRho);
  m ~ normal(meanMu, sdMu);
  l ~ normal(meanLambda, sdLambda);
  db ~ normal(meanDB,sdDB);

  for (t in 1:N) {
    div = 61^rtmp[t];
    // Model with M, L, R, DB

    total_sum[t] = mtmp[t] / div * (0.5 * gain[t]^rtmp[t] +
                                   -0.5 * ltmp[t] * loss[t]^rtmp[t] -
                                   safe[t]^rtmp[t] -
                                   db[ind[t]]);
  }

  p = inv_logit(total_sum);

  choices ~ bernoulli(p);
}
