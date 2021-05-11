# Exclusion based on QualtricsExclusion.R script (in the code directory).

## PHASE 1:
	-  sub 461 - missing qualtrics responses
	-  sub 353 - duplicate responses, will remove the second one
	-  sub 347 - missed an attention check (keep)
	-  sub 348 - missed an attention check (keep)
	-  sub 030 - age response = .32 (drop from all data - participated in phase2)
	-  sub 373 - age response = 14 (drop from all data - only participated in phase 1)

## PHASE 2:
	-  sub 543 - missed an attention check (keep)


### This script:
1) removes the duplicate response from QualtricsCombined_subID_scored.csv and saves a new .csv file: QualtricsCombined_subID_scored_noDuplicates.csv
2) creates an exclusion matrix: qualtricsExclusion.csv (sub ID, phase1exclude, phase2exclude)
