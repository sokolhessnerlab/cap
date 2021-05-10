# A brief summary of the process and results of RDMexclusion.R script


## Four criteria for excluding participants from the risky decision-making data
	1)	Missed trials (in both gain-only and mixed-valence tasks)
	2)	Missed attention checks (gain-only)
		⁃	did participants play it safe when they should have gambled (safe =$0)?
		⁃	did participants gamble when they should have played it safe (safe >=risky gain)?
	3)	Probability of gambling (we looked at this for both gain and gain-loss, but used gain-only in determining final exclusion criteria)
	4)	Choices influenced by gain and safe (gain-only)


### Step 1: looking at the details (behavior by each participant, in each phase, in each task.
	
	1)	The number and proportion of missed trials for each participant in each phase in each of the tasks (gain-only, gain-loss).
		⁃	phase 1 gain-only: 921 missed trials, mean = 1.7, range = 0-36, range of proportion missed = 0-.3
		⁃	phase 2 gain-only: 574 missed trials, mean = 1.1, range = 0-49, range of proportion missed = 0-.4
		⁃	phase 1 gain-loss: 271 missed trials, mean = .49, range = 0-7, range of proportion missed = 0-.35
		⁃	phase 2 gain-loss: 110 missed trials, mean = .2, range = 0-5, range of proportion missed = 0-.25
	
	2)	The total number of each type of attention checks (should safe, should gamble) each participant had and the number they missed and the proportion they missed of these trials.
		⁃	Because of the study design, the full VNI choice set was generated (219 trials), then we only used the first 119 trials (because of time). As a result, each participant has a different number of attention checks.
		⁃	phase 1 gain-only: 158/4200 “should safe” trials were missed & 18/755 “should gamble” trials were missed
		⁃	phase 2 gain-only: 98/2792 “should safe” trials were missed & 10/452 “should gamble” trials were missed
	
	3)	Probability of gambling in phase 1 and phase 2 in gain-only and gain-loss tasks
		⁃	gain-only phase 1: mean = .38; range = 0-.91; median = .39
		⁃	3 subs pgamble = 0; 1 sub pgamble < .9 
		⁃	gain-only phase 2: mean = .35; range = 0-.91; median = .36
		⁃	7 subs pgamble = 0; 1 sub pgamble > .9
		⁃	loss-only phase 1: mean = .45; range =0-1; median = .44
		⁃	20 subs pgamble = 0; 36 subs pgmable <.1
		⁃	22 subs pgmable = 1; 33 subs pgamble >.9
		⁃	loss-only phase 2: mean = .43; range =0-1; median = .4
		⁃	15 subs pgamble = 0; 27 subs pgamble <.1
		⁃	19 subs pgamble = 1; 24 subs pgamble >.9
	
	4)	Choices influenced by gains and safe in phase 1 and phase 2 in gain-only task
		⁃	Phase 1: gains did not influence choice: 31 participants
		⁃	Phase 1: safes did not influence choice: 21 participants
		⁃	Phase 1: 15 of these subs were not influenced by both gains and safe values
		⁃	Phase 2: gains did not influence choice: 38 participants
		⁃	Phase 2: gains did not influence choice: 21 participants
		⁃	Phase 2: 9 of these subs were not influenced by both gains and safe values
	All of these details are saved in the output: RDMqa.csv (in data/combinedData/)

### Step 2: Flag the people who are problematic (e.g. their gamble doesn’t meet our threshold)
	1)	missed trial threshold: more than 10% of trials for both gain and gain-loss 
	2)	missed AC: participants had at least 4 attention checks and they missed more than 30% of those attention checks
	3)	p gamble for gain-only task is less than .05 and greater than .95
	4)	glm threshold: if they were not influenced by safe or gain in either phase, they “fail”
	
	If participants pass = 1; if they fail =0. There are a total of 5 criteria (Accounting for missed trials in gain and loss tasks). 

### Step 3: Sum up the pass/fail. 
	- 5 = perfect, 0 = failed all criteria.
	- phase 1: range = 2-5; mean = 4.76
	- phase 2: range = 1-5; mean = 4.7

### Step 4: who is getting excluded?
	1)	Our cut off is missing more than 1 of the criteria (we are keeping participants with totals of 4 and 5)
		⁃	Phase 1: excluding 26 participants
		⁃	Phase 2: excluding 31 participants
		⁃	10 of these participants are excluded from both phase 1 and phase 2
		⁃	This criteria (keeping 4-5s) captures a lot (but not all) of the participants who missed the glm check. So there will be participants with 4s who missed the glm criteria, but if the rest of their data is good, its not that they weren’t paying attention but rather their choices were influenced by something else (eg. context).
	2)	If our cut off is missing more than 2 of the criteria (we are keeping participants with totals 3-5), we’d exclude a lot fewer people (phase 1 = 3 subs; phase 2 = 2 subs), but we’d be including people with a combination of glm=0, pgamble near the bounds, and lots of missed attention checks.


## The output of these results is a .csv files with 3 columns:
1) SubID: 1-544
2) Phase1exclude: 1 = yes, exclude; 0 = no, keep
3) Phase2exclude: 1 = yes, exclude; 0 = no, keep





