# Stan models for CAP (rdm data)

## model 0 (cap_model0basic.stan)
  - accounts for risk aversion, loss aversion and choice consistency (RFX for all)

## model 0 v2
  - PT plus, includes decision-bias (look at file from CBM), no modulation of other things (RFX for all)


## Possible paths forward:
  1) add past outcome (4 updating parameters - start these with FFX - from CBM)
  2) add positive shift amount and relative earnings. We've always struggled with combining earnings and expectations into a single term in the linear framework. Can we do something a little different here because we are looking in a nonlinear framework. Perhaps estimating an alpha and beta where alpha(earnings$ - beta*expectation) and potentially how alpha and beta impact the parameters (this might change where we do 1 alpha or 1 beta or some variation).
 