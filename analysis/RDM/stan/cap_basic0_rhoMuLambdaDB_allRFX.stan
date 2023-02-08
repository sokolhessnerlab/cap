data {
  int N; // number of trials (across participants)
  int nsubj; // number of participants
  int choices[N]; // choice vector
  real gain[N]; // risky gain vector
  real loss[N]; // risky loss vector
  real safe[N]; // safe vector
  int ind[N]; // subject index
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
}

transformed parameters {
  // real rtmp[N];
  // real mtmp[N];
  // real ltmp[N];
  //
  // for(t in 1:N) { // for each trial
  //   rtmp[t] = exp(r[ind[t]]); // take individual-level rho sample (that was sampled in unbounded space) and put it in the exponential to make it >0
  //   mtmp[t] = exp(m[ind[t]]); // same as above for mu
  //   ltmp[t] = exp(l[ind[t]]); //same for lambda
  // }
  real rtmp[nsubj];
  real ltmp[nsubj];
  real mtmp[nsubj];

  rtmp = exp(r);
  ltmp = exp(l);
  mtmp = exp(m);
}

model {
  real div;
  real p[N];
  //real gambleUtil; will try vector based stuff below but just in case there is an issue, not defining with N might work better.
  //real safeUtil;
  // real gainUtil[N]; // utility for gain
  // real lossUtil[N]; // utility for loss
  // real safeUtil[N]; // utility for safe option
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

  // //Hierarchy
  r ~ normal(meanRho, sdRho);
  m ~ normal(meanMu, sdMu);
  l ~ normal(meanLambda, sdLambda);
  db ~ normal(meanDB,sdDB);


  for (t in 1:N) {
    div = 61^rtmp[ind[t]];
    // Model with M, L, R, DB

    // gainUtil[t] = 0.5 * gain[t]^rtmp;
    // lossUtil[t] = -0.5 * ltmp * loss[t]^rtmp;
    //
    // safeUtil[t] = safe[t]^rtmp;
    //
    // total_sum[t] = mtmp / div[t] * (gainUtil[t] + lossUtil[t] - safeUtil[t] - dbtmp);

    total_sum[t] = mtmp[ind[t]] / div * (0.5 * gain[t]^rtmp[ind[t]] +
                                  -0.5 * ltmp[ind[t]] * loss[t]^rtmp[ind[t]] -
                                  safe[t]^rtmp[ind[t]] -
                                  db[ind[t]]);
  }

  p = inv_logit(total_sum);

  choices ~ bernoulli(p);
}
