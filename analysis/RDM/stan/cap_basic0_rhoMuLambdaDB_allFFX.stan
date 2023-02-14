data {
  int N; // number of trials (across participants)
  int nsubj; // number of participants
  int choices[N]; // choice vector
  real gain[N]; // risky gain vector
  real loss[N]; // risky loss vector (this should have loss MAGNITUDE, i.e. all values should be >=0)
  real safe[N]; // safe vector
  int ind[N]; // subject index
}

parameters {
  real meanRho;
  real meanMu;
  real meanLambda;
  real meanDB;
}

transformed parameters {
  real rtmp;
  real ltmp;
  real mtmp;

  rtmp = exp(meanRho);
  ltmp = exp(meanLambda);
  mtmp = exp(meanMu);
}

model {
  real div;
  real p[N];
  real total_sum[N];

  //Priors
  meanRho ~ normal(0,30);
  meanMu ~ normal(0,30);
  meanLambda ~ normal(0,30);
  meanDB ~ normal(0,30);

  for (t in 1:N) {
    div = 61^rtmp;
    // Model with M, L, R, DB

    total_sum[t] = mtmp / div * (0.5 * gain[t]^rtmp +
                                -0.5 * ltmp * loss[t]^rtmp -
                                safe[t]^rtmp -
                                meanDB);
  }

  p = inv_logit(total_sum);

  choices ~ bernoulli(p);
}
