# CAP ANALYSIS for participants who completed phase 1 and phase 2
# GOAL for CAP presentation: focus on subs who completed both phase 1 and phase 2 (n=355) but explore complementary analyses with the full set of participants to have in back pocket and to show similarities and differences when necessary
rm(list=ls());
library("lme4");
library("lmerTest")
library(ggplot2);
library(ggExtra);

#Load all the data we will need for this analysis
load("/Volumes/shlab/Projects/CAP/analysis/phs2FullDatasets/bothPhasesRDMqualtrics.Rdata"); # 'bothPhasesRDMqualtrics'- both phases RDM + Qualtrics
load("/Volumes/shlab/Projects/CAP/analysis/phs1FullDatasets/phs1RDMqualtrics.Rdata"); #'phs1RDMqualtrics' - subs phase 1 RDM + Qualtrics
load("/Volumes/shlab/Projects/CAP/analysis/phs2FullDatasets/phs2RDMqualtrics.Rdata"); #'phs2RDMqualtrics' - subs phase 2 RDM + Qualtrics
load("/Volumes/shlab/Projects/CAP/analysis/phs1FullDatasets/subInfoPlusQualtricsScoredPhs1.Rdata"); #'subInfoQualPhs1' - subs phase 1 qualtrics
load("/Volumes/shlab/Projects/CAP/analysis/phs2FullDatasets/subInfoPlusQualtricsScoredPhs2.Rdata"); #'subInfoQualPhs2' - subs phase 2 qualtrics
load("/Volumes/shlab/Projects/CAP/analysis/phs2FullDatasets/subInfoScoreQualBothPhases.Rdata"); # subInfoScoreQualBothPhases - both phases qualtrics


# Analysis of affective data
# 1) How do things evolve across days for phase 1 and phase 2 (averaging across people; n=355)
staisQuartilesxDaysPhs1= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
staitQuartilesxDaysPhs1= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
pssQuartilesxDaysPhs1= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
uclalQuartilesxDaysPhs1= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));

staisQuartilesxDaysPhs2= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
staitQuartilesxDaysPhs2= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
pssQuartilesxDaysPhs2= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
uclalQuartilesxDaysPhs2= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));

staisQuartilesxDaysDiff= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
staitQuartilesxDaysDiff= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
pssQuartilesxDaysDiff= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));
uclalQuartilesxDaysDiff= matrix(data=NA, nrow=20, ncol=4, dimnames=list(c(NULL),c("Q1","Q2","Q3","Q4")));

# calculate the average affective scores on each day for each quartile
for (d in 1:20){
  for (q in 1:4) {
    staisQuartilesxDaysPhs1[d,q] = mean(subInfoScoreQualBothPhases$stais_scorePhs1[subInfoScoreQualBothPhases$dayPhs1==d & subInfoScoreQualBothPhases$quartilePhs1==q]);
    staitQuartilesxDaysPhs1[d,q] = mean(subInfoScoreQualBothPhases$stait_scorePhs1[subInfoScoreQualBothPhases$dayPhs1==d & subInfoScoreQualBothPhases$quartilePhs1==q]);
    pssQuartilesxDaysPhs1[d,q] = mean(subInfoScoreQualBothPhases$pss_scorePhs1[subInfoScoreQualBothPhases$dayPhs1==d & subInfoScoreQualBothPhases$quartilePhs1==q]);
    uclalQuartilesxDaysPhs1[d,q] = mean(subInfoScoreQualBothPhases$uclal_scorePhs1[subInfoScoreQualBothPhases$dayPhs1==d & subInfoScoreQualBothPhases$quartilePhs1==q]);
    
    staisQuartilesxDaysPhs2[d,q] = mean(subInfoScoreQualBothPhases$stais_scorePhs2[subInfoScoreQualBothPhases$dayPhs2==d & subInfoScoreQualBothPhases$quartilePhs2==q]);
    staitQuartilesxDaysPhs2[d,q] = mean(subInfoScoreQualBothPhases$stait_scorePhs2[subInfoScoreQualBothPhases$dayPhs2==d & subInfoScoreQualBothPhases$quartilePhs2==q]);
    pssQuartilesxDaysPhs2[d,q] = mean(subInfoScoreQualBothPhases$pss_scorePhs2[subInfoScoreQualBothPhases$dayPhs2==d & subInfoScoreQualBothPhases$quartilePhs2==q]);
    uclalQuartilesxDaysPhs2[d,q] = mean(subInfoScoreQualBothPhases$uclal_scorePhs2[subInfoScoreQualBothPhases$dayPhs2==d & subInfoScoreQualBothPhases$quartilePhs2==q]);
    
    staisQuartilesxDaysDiff[d,q]=staisQuartilesxDaysPhs1[d,q]-staisQuartilesxDaysPhs2[d,q];
    staitQuartilesxDaysDiff[d,q]=staitQuartilesxDaysPhs1[d,q]-staitQuartilesxDaysPhs2[d,q]
    pssQuartilesxDaysDiff[d,q]= pssQuartilesxDaysPhs1[d,q]-pssQuartilesxDaysPhs2[d,q]
    uclalQuartilesxDaysDiff[d,q]=uclalQuartilesxDaysPhs1[d,q]-uclalQuartilesxDaysPhs2[d,q]
  };
};

#STAIS
pdf("/Volumes/shlab/Projects/CAP/figures/STAISn355_HRB.pdf")
par(mfrow=c(1,1))
plot(staisQuartilesxDaysPhs1[,1], type="l", col="blue", xlab="day", ylab = "Daily average STAIS", lwd=2,ylim=c(20,80), main="Phase 1 State Anxiety (n=355)")
lines(staisQuartilesxDaysPhs1[,2], col="green", lwd=2)
lines(staisQuartilesxDaysPhs1[,3], col="orange", lwd=2)
lines(staisQuartilesxDaysPhs1[,4], col="yellow", lwd=2)


plot(staisQuartilesxDaysPhs2[,1], type="l", col="blue", xlab="day", ylab = "Daily average STAIS", lwd=2,ylim=c(20,80),main="Phase 2 State Anxiety (n=355)")
lines(staisQuartilesxDaysPhs2[,2], col="green", lwd=2)
lines(staisQuartilesxDaysPhs2[,3], col="orange", lwd=2)
lines(staisQuartilesxDaysPhs2[,4], col="yellow", lwd=2)


plot(staisQuartilesxDaysDiff[,1], type="l", col="blue", xlab="day", ylab = "STAIS (phase 1 - phase 2)", lwd=2, ylim =c(min(staisQuartilesxDaysDiff),max(staisQuartilesxDaysDiff)),main="State Anxiety Differences (n=355)")
lines(staisQuartilesxDaysDiff[,2], col="green", lwd=2)
lines(staisQuartilesxDaysDiff[,3], col="orange", lwd=2)
lines(staisQuartilesxDaysDiff[,4], col="yellow", lwd=2)
abline(h=0, lty="dashed")
dev.off();

#STAIT
pdf("/Volumes/shlab/Projects/CAP/figures/STAISTn355_HRB.pdf")
par(mfrow=c(1,1))
plot(staitQuartilesxDaysPhs1[,1], type="l", col="blue", xlab="day", ylab = "Daily average STAIT", lwd=2,ylim=c(20,80), main="Phase 1 Trait Anxiety (n=355)")
lines(staitQuartilesxDaysPhs1[,2], col="green", lwd=2)
lines(staitQuartilesxDaysPhs1[,3], col="orange", lwd=2)
lines(staitQuartilesxDaysPhs1[,4], col="yellow", lwd=2)


plot(staitQuartilesxDaysPhs2[,1], type="l", col="blue", xlab="day", ylab = "Daily average STAIT", lwd=2,ylim=c(20,80),main="Phase 2 Trait Anxiety (n=355)")
lines(staitQuartilesxDaysPhs2[,2], col="green", lwd=2)
lines(staitQuartilesxDaysPhs2[,3], col="orange", lwd=2)
lines(staitQuartilesxDaysPhs2[,4], col="yellow", lwd=2)


plot(staitQuartilesxDaysDiff[,1], type="l", col="blue", xlab="day", ylab = "STAIT (phase 1 - phase 2)", lwd=2, ylim =c(min(staitQuartilesxDaysDiff),max(staitQuartilesxDaysDiff)),main="TRAIT Anxiety Differences (n=355)")
lines(staitQuartilesxDaysDiff[,2], col="green", lwd=2)
lines(staitQuartilesxDaysDiff[,3], col="orange", lwd=2)
lines(staitQuartilesxDaysDiff[,4], col="yellow", lwd=2)
abline(h=0, lty="dashed")
dev.off();

#UCLAL
pdf("/Volumes/shlab/Projects/CAP/figures/UCLALn355_HRB.pdf")
par(mfrow=c(1,1))
plot(uclalQuartilesxDaysPhs1[,1], type="l", col="blue", xlab="day", ylab = "Daily average UCLAL", lwd=2,ylim=c(28,66), main="UCLA Loneliness Phase 1 (n=355)")
lines(uclalQuartilesxDaysPhs1[,2], col="green", lwd=2)
lines(uclalQuartilesxDaysPhs1[,3], col="orange", lwd=2)
lines(uclalQuartilesxDaysPhs1[,4], col="yellow", lwd=2)


plot(uclalQuartilesxDaysPhs2[,1], type="l", col="blue", xlab="day", ylab = "Daily average UCLAL", lwd=2,ylim=c(28,66),main="UCLA Loneliness Phase 2 (n=355)")
lines(uclalQuartilesxDaysPhs2[,2], col="green", lwd=2)
lines(uclalQuartilesxDaysPhs2[,3], col="orange", lwd=2)
lines(uclalQuartilesxDaysPhs2[,4], col="yellow", lwd=2)


plot(uclalQuartilesxDaysDiff[,1], type="l", col="blue", xlab="day", ylab = "UCLAL (phase 1 - phase 2)", lwd=2, ylim =c(min(uclalQuartilesxDaysDiff),max(uclalQuartilesxDaysDiff)),main="UCLAL Differences (n=355)")
lines(uclalQuartilesxDaysDiff[,2], col="green", lwd=2)
lines(uclalQuartilesxDaysDiff[,3], col="orange", lwd=2)
lines(uclalQuartilesxDaysDiff[,4], col="yellow", lwd=2)
abline(h=0, lty="dashed")
dev.off()

#PSS
pdf("/Volumes/shlab/Projects/CAP/figures/PSSn355_HRB.pdf")
par(mar=c(5, 4, 4, 2) + 0.1, mfrow=c(1,1)); # reset to default
plot(pssQuartilesxDaysPhs1[,1], type="l", col="blue", xlab="day", ylab = "Daily average PSS", lwd=2,ylim=c(8,29), main="Perceived Stress Phase 1 (n=355)")
lines(pssQuartilesxDaysPhs1[,2], col="green", lwd=2)
lines(pssQuartilesxDaysPhs1[,3], col="orange", lwd=2)
lines(pssQuartilesxDaysPhs1[,4], col="yellow", lwd=2)


plot(pssQuartilesxDaysPhs2[,1], type="l", col="blue", xlab="day", ylab = "Daily average PSS", lwd=2,ylim=c(8,29),main="Perceived Stress Phase 2 (n=355)")
lines(pssQuartilesxDaysPhs2[,2], col="green", lwd=2)
lines(pssQuartilesxDaysPhs2[,3], col="orange", lwd=2)
lines(pssQuartilesxDaysPhs2[,4], col="yellow", lwd=2)


plot(pssQuartilesxDaysDiff[,1], type="l", col="blue", xlab="day", ylab = "PSS (phase 1 - phase 2)", lwd=2, ylim =c(min(pssQuartilesxDaysDiff),max(pssQuartilesxDaysDiff)),main="Perceived Stress Differences (n=355)")
lines(pssQuartilesxDaysDiff[,2], col="green", lwd=2)
lines(pssQuartilesxDaysDiff[,3], col="orange", lwd=2)
lines(pssQuartilesxDaysDiff[,4], col="yellow", lwd=2)
abline(h=0, lty="dashed")
dev.off();


#2) The figures above collapse across participants - what does the individual data look like for each of the four affective measures across phase 1 and phase 2

# calculate means for stais, stait, pss, and uclal for each quartile for phase 1 and phase 2
afctQuartMeansBothPhs=as.data.frame(matrix(data=NA, nrow = 8, ncol = 4, dimnames = list(c("statePhs1","statePhs2","traitPhs1","traitPhs2","uclalPhs1","uclalPhs2","pssPhs1","pssPhs2"), c("Q1", "Q2", "Q3", "Q4")))); 

for (q in 1:ncol(afctQuartMeansBothPhs)) {
    afctQuartMeansBothPhs[1,q] = mean(subInfoScoreQualBothPhases$stais_scorePhs1[subInfoScoreQualBothPhases$quartilePhs1==q]);
    afctQuartMeansBothPhs[2,q] = mean(subInfoScoreQualBothPhases$stais_scorePhs2[subInfoScoreQualBothPhases$quartilePhs1==q]);
    afctQuartMeansBothPhs[3,q] = mean(subInfoScoreQualBothPhases$stait_scorePhs1[subInfoScoreQualBothPhases$quartilePhs1==q]);
    afctQuartMeansBothPhs[4,q] = mean(subInfoScoreQualBothPhases$stait_scorePhs2[subInfoScoreQualBothPhases$quartilePhs1==q]);
    afctQuartMeansBothPhs[5,q] = mean(subInfoScoreQualBothPhases$uclal_scorePhs1[subInfoScoreQualBothPhases$quartilePhs1==q]);
    afctQuartMeansBothPhs[6,q] = mean(subInfoScoreQualBothPhases$uclal_scorePhs2[subInfoScoreQualBothPhases$quartilePhs1==q]);
    afctQuartMeansBothPhs[7,q] = mean(subInfoScoreQualBothPhases$pss_scorePhs1[subInfoScoreQualBothPhases$quartilePhs1==q]);
    afctQuartMeansBothPhs[8,q] = mean(subInfoScoreQualBothPhases$pss_scorePhs2[subInfoScoreQualBothPhases$quartilePhs1==q]);
};



pdf("/Volumes/shlab/Projects/CAP/figures/STAIS_MargDist_HRB.pdf")
#STAIS
x = as.data.frame(matrix(nrow = 4,ncol=3, dimnames = list(c(NULL),c("phase1","phase2","quartile"))));
x$phase1 = t(afctQuartMeansBothPhs[1,])
x$phase2 = t(afctQuartMeansBothPhs[2,])
x$quartile = 1:4;

p <- ggplot(subInfoScoreQualBothPhases, aes(x=stais_scorePhs1, y=stais_scorePhs2)) +
   scale_fill_brewer(palette="Dark2") +scale_colour_brewer(palette="Dark2")+ theme_classic() + geom_jitter(height = 0, width = 0.1,color="black", alpha=.2) + scale_x_continuous(name ="STAIS Phase 1") +scale_y_continuous(name ="STAIS Phase 2", limits=c(20,80)) + geom_point(data = x, mapping = aes(x = phase1, y = phase2, group=quartile), shape=23, size=4,color=x$quartile, fill=x$quartile,alpha=.5) + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed")+ removeGrid();


ggExtra::ggMarginal(p,fill="grey",size=5, type="histogram");

dev.off();



#STAIT
pdf("/Volumes/shlab/Projects/CAP/figures/STAIT_MargDist_HRB.pdf")
x = as.data.frame(matrix(nrow = 4,ncol=3, dimnames = list(c(NULL),c("phase1","phase2","quartile"))));
x$phase1 = t(afctQuartMeansBothPhs[3,])
x$phase2 = t(afctQuartMeansBothPhs[4,])
x$quartile = 1:4;

p <- ggplot(subInfoScoreQualBothPhases, aes(x=stait_scorePhs1, y=stait_scorePhs2)) +
  scale_fill_brewer(palette="Dark2") +scale_colour_brewer(palette="Dark2")+ theme_classic() + geom_jitter(height = 0, width = 0.1,color="black", alpha=.2) + scale_x_continuous(name ="STAIT Phase 1",limits = c(20,80)) +scale_y_continuous(name ="STAIT Phase 2",limits = c(20,80)) + geom_point(data = x, mapping = aes(x = phase1, y = phase2, group=quartile), shape=23, size=4,color=x$quartile, fill=x$quartile, alpha=.5) + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed")+ removeGrid();

ggExtra::ggMarginal(p,fill="grey",size=5, type="histogram")
dev.off();



#UCLAL
pdf("/Volumes/shlab/Projects/CAP/figures/UCLAL_MargDist_HRB.pdf")
x = as.data.frame(matrix(nrow = 4,ncol=3, dimnames = list(c(NULL),c("phase1","phase2","quartile"))));
x$phase1 = t(afctQuartMeansBothPhs[5,])
x$phase2 = t(afctQuartMeansBothPhs[6,])
x$quartile = 1:4;

p <- ggplot(subInfoScoreQualBothPhases, aes(x=uclal_scorePhs1, y=uclal_scorePhs2)) +
  scale_fill_brewer(palette="Dark2") +scale_colour_brewer(palette="Dark2")+ theme_classic() + geom_jitter(height = 0, width = 0.1,color="black", alpha=.2) + scale_x_continuous(name ="UCLAL Phase 1",limits = c(20,80)) +scale_y_continuous(name ="UCLAL Phase 2",limits = c(20,80)) + geom_point(data = x, mapping = aes(x = phase1, y = phase2, group=quartile), shape=23, size=4,color=x$quartile, fill=x$quartile, alpha=.5) + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed") +removeGrid();

ggExtra::ggMarginal(p,fill="grey",size=5, type="histogram")
dev.off();



#PSS
pdf("/Volumes/shlab/Projects/CAP/figures/PSS_MargDist_HRB.pdf")
x = as.data.frame(matrix(nrow = 4,ncol=3, dimnames = list(c(NULL),c("phase1","phase2","quartile"))));
x$phase1 = t(afctQuartMeansBothPhs[7,])
x$phase2 = t(afctQuartMeansBothPhs[8,])
x$quartile = 1:4;

p <- ggplot(subInfoScoreQualBothPhases, aes(x=pss_scorePhs1, y=pss_scorePhs2)) +
  scale_fill_brewer(palette="Dark2") +scale_colour_brewer(palette="Dark2")+ theme_classic() + geom_jitter(height = 0, width = 0.1,color="black", alpha=.2) + scale_x_continuous(name ="PSS Phase 1") +scale_y_continuous(name ="PSS Phase 2") + geom_point(data = x, mapping = aes(x = phase1, y = phase2, group=quartile), shape=23, size=4,color=x$quartile, fill=x$quartile,alpha=.5) + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed") + removeGrid();

ggExtra::ggMarginal(p,fill="grey",size=5, type="histogram")

dev.off();

#simple paired sample t-test to see differences across phases
t.test(subInfoScoreQualBothPhases$stais_scorePhs1, subInfoScoreQualBothPhases$stais_scorePhs2, paired =TRUE); #sig
t.test(subInfoScoreQualBothPhases$stait_scorePhs1, subInfoScoreQualBothPhases$stait_scorePhs2, paired =TRUE); #ns
t.test(subInfoScoreQualBothPhases$uclal_scorePhs1, subInfoScoreQualBothPhases$uclal_scorePhs2, paired =TRUE); #ns
t.test(subInfoScoreQualBothPhases$pss_scorePhs1, subInfoScoreQualBothPhases$pss_scorePhs2, paired =TRUE); #sig  (0.027)
t.test(subInfoScoreQualBothPhases$CovidRiskPhs1, subInfoScoreQualBothPhases$CovidRiskPhs2, paired =TRUE); #sig  (0.0070) (phase 1 = 5.3, phase 2 = 5.1)

# Difference scores for STAIS, STAIT, PSS, UCLAL, COVID RISK
subInfoScoreQualBothPhases$staisDiff = subInfoScoreQualBothPhases$stais_scorePhs2 - subInfoScoreQualBothPhases$stais_scorePhs1;
subInfoScoreQualBothPhases$staitDiff = subInfoScoreQualBothPhases$stait_scorePhs2 - subInfoScoreQualBothPhases$stais_scorePhs1;
subInfoScoreQualBothPhases$uclalDiff = subInfoScoreQualBothPhases$uclal_scorePhs2 - subInfoScoreQualBothPhases$uclal_scorePhs1;
subInfoScoreQualBothPhases$pssDiff = subInfoScoreQualBothPhases$pss_scorePhs2 - subInfoScoreQualBothPhases$pss_scorePhs1;
subInfoScoreQualBothPhases$CovidRiskDiff = subInfoScoreQualBothPhases$CovidRiskPhs2 - subInfoScoreQualBothPhases$CovidRiskPhs1; # SIG DECLINE p = 0.007

subInfoScoreQualBothPhasesStacked = as.data.frame(matrix(nrow = 540+355,ncol=10, dimnames = list(c(NULL),c("stais","stait","uclal","pss","CovidRisk","prolificID","Age",'Quartile','Phase','day'))));
subInfoScoreQualBothPhasesStacked$stais = c(subInfoQualPhs1$stais_score,subInfoQualPhs2$stais_score)
subInfoScoreQualBothPhasesStacked$stait = c(subInfoQualPhs1$stait_score,subInfoQualPhs2$stait_score)
subInfoScoreQualBothPhasesStacked$uclal = c(subInfoQualPhs1$uclal_score,subInfoQualPhs2$uclal_score)
subInfoScoreQualBothPhasesStacked$pss = c(subInfoQualPhs1$pss_score,subInfoQualPhs2$pss_score)
subInfoScoreQualBothPhasesStacked$prolificID = c(subInfoQualPhs1$prolificID,subInfoQualPhs2$prolificID)
subInfoScoreQualBothPhasesStacked$Age = c(subInfoQualPhs1$Age,subInfoQualPhs2$Age)
subInfoScoreQualBothPhasesStacked$Quartile = c(subInfoQualPhs1$quartile,subInfoQualPhs2$quartile)
subInfoScoreQualBothPhasesStacked$Phase = c(seq(from=1,to=1,length.out=540),seq(from=2,to=2,length.out=355))
subInfoScoreQualBothPhasesStacked$phaseRecode = subInfoScoreQualBothPhasesStacked$Phase-1;
subInfoScoreQualBothPhasesStacked$day = c(subInfoQualPhs1$day,subInfoQualPhs2$day+20)
subInfoScoreQualBothPhasesStacked$dayoverall01 = subInfoScoreQualBothPhasesStacked$day/max(subInfoScoreQualBothPhasesStacked$day);
subInfoScoreQualBothPhasesStacked$CovidRisk = c(subInfoQualPhs1$CovidRisk,subInfoQualPhs2$CovidRisk)

# for cap presentation, want to show an example choice set over time
onesub = phs1RDMqualtrics[phs1RDMqualtrics$subID==1,];

pdf("/Volumes/shlab/Projects/CAP/figures/choiceSetGEVonly.pdf")
plot(onesub$groundEV, col=onesub$groundEV, pch=16,xlab="Trial", ylab = "$", main="Creating and shifting context \nwith common expected values", ylim=c(0,30));
dev.off();

pdf("/Volumes/shlab/Projects/CAP/figures/choiceSetGEValts.pdf")
plot(onesub$groundEV, col=onesub$groundEV, pch=16,xlab="Trial", ylab = "$", main="Creating and shifting context \nwith common expected values", ylim=c(0,30));
points(onesub$alternative, col=onesub$groundEV)
dev.off();

pdf("/Volumes/shlab/Projects/CAP/figures/choiceSetalts.pdf")
plot(onesub$alternative, col=onesub$groundEV,xlab="Trial", ylab = "$", main="Creating and shifting context \nwith common expected values", ylim=c(0,30));
dev.off();

# Look at some basics decision-making stuff for all participants who completed phase 1 (n=540) and all participants who completed phase 2 (n=355)
subNumsPhs1 = unique(phs1RDMqualtrics$subID);
pIDphs1 = unique(phs1RDMqualtrics$participant);
nSubPhs1 = length(subNumsPhs1); #540
subNumsPhs2 = unique(phs2RDMqualtrics$subID);
pIDphs2 = unique(phs2RDMqualtrics$participant);
nSubPhs2 = length(subNumsPhs2); #355

mean(phs1RDMqualtrics$choice); #0.3757795
mean(phs2RDMqualtrics$choice); #0.3507808
mean(bothPhasesRDMqualtrics$choice[bothPhasesRDMqualtrics$phase==1]); # 0.3725118 p(gamble) for phase 1 for those who completed phase 2

# 95% CI
# 540 participants pgamble in phase 1
me1 = qnorm(.975)*(mean(phs1RDMqualtrics$choice)/sqrt(nSubPhs1)); #0.03169453
mean(phs1RDMqualtrics$choice) + me1; #0.407474
mean(phs1RDMqualtrics$choice) - me1; #0.344085

# 355 participants pgamble in phase 2
me2 = qnorm(.975)*(mean(phs2RDMqualtrics$choice)/sqrt(nSubPhs2)); #0.03648965
mean(phs2RDMqualtrics$choice) + me2; #0.3872705
mean(phs2RDMqualtrics$choice) - me2; #0.3142912

# 355 participants pgamble from phase 1 who also completed phase 2
me3 = qnorm(.975)*(mean(bothPhasesRDMqualtrics$choice[bothPhasesRDMqualtrics$phase==1])/sqrt(nSubPhs2)); #0.0387502
mean(bothPhasesRDMqualtrics$choice[bothPhasesRDMqualtrics$phase==1]) + me3; #0.411262
mean(bothPhasesRDMqualtrics$choice[bothPhasesRDMqualtrics$phase==1]) - me3; #0.3337616


# calculate the probability of gambling for each participant (n=355) for phase 1 and phase 2
pgamBothPhs = matrix(data=NA, nrow = nSubPhs2, ncol=4);
for (s in 1:nSubPhs2) {
  pgamBothPhs[s,1] = mean(bothPhasesRDMqualtrics$choice[bothPhasesRDMqualtrics$prolificID == pIDphs2[s] & bothPhasesRDMqualtrics$phase==1])
  pgamBothPhs[s,2] = mean(bothPhasesRDMqualtrics$choice[bothPhasesRDMqualtrics$prolificID == pIDphs2[s] & bothPhasesRDMqualtrics$phase==2])
  pgamBothPhs[s,3] = pIDphs2[s];
  pgamBothPhs[s,4] = pgamBothPhs[s,1]-pgamBothPhs[s,2]
};

# Correlations btwn change in p(gamble) & changes in STAIS/STAIT/UCLAL/PSS/COVIDRisk
cor.test(pgamBothPhs[,4],subInfoScoreQualBothPhases$staisDiff,method = 'spearman')
cor.test(pgamBothPhs[,4],subInfoScoreQualBothPhases$staitDiff,method = 'spearman')
cor.test(pgamBothPhs[,4],subInfoScoreQualBothPhases$uclalDiff,method = 'spearman')
cor.test(pgamBothPhs[,4],subInfoScoreQualBothPhases$pssDiff,method = 'spearman')
cor.test(pgamBothPhs[,4],subInfoScoreQualBothPhases$CovidRiskDiff,method = 'spearman')


x = as.data.frame(matrix(data=NA, nrow = nSubPhs2*2, ncol=3, dimnames=list(c(NULL), c("pgam", "subID", "phase"))));
x$pgam = c(pgamBothPhs[,1], pgamBothPhs[,2]); # combine into one vector
x$subID = c(pgamBothPhs[,3],pgamBothPhs[,3]);
x$phase = rep(1:2,each=355);

pdf("/Volumes/shlab/Projects/CAP/figures/ViolinPGAMn355_HRB.pdf")
g = ggplot(x, aes(x=as.character(phase), y=pgam));

g + geom_violin(alpha =.3, scale="area", trim=F, adjust = 2, aes(fill=as.character(phase), color=pgam), show.legend = F) +  labs(title="Risk-taking in Phase 1 and Phase 2 (n=355)", subtitle = "Each dot represents the probability of gambling for a single participant") + scale_fill_brewer(palette="Dark2") +scale_colour_brewer(palette="Dark2")+geom_jitter(height = 0, width = 0.1, color="black", alpha=.3) + scale_x_discrete(name ="Phase") +scale_y_continuous(name ="p(gamble)", limits=c(0,1)) + theme_classic();
dev.off();


pdf("/Volumes/shlab/Projects/CAP/figures/histDiffpGAMn355_HRB.pdf")
my_hist=hist(pgamBothPhs[,4] , breaks=50  , plot=F)
#my_color= ifelse(my_hist$breaks <0, rgb(0.9,0.8,0.2,0.8) , ifelse (my_hist$breaks >0, "purple", rgb(0.2,0.2,0.2,0.2) ))
my_color= ifelse(my_hist$breaks <0, "salmon" , ifelse (my_hist$breaks >0, "seagreen", rgb(0.2,0.2,0.2,0.2) ))

plot(my_hist, col=my_color , border=F , main="Distribution of change in risk-taking\n across Phases 1 and 2 (n=355)" ,xlab="Change in risk-taking\np(gamble Phase 1) - p(gamble Phase 1)", ylab="Number of people", xlim=c(-.65,.65), axes=F)

axis(1,at = c(-.64,.01,.64),labels = c(-.64,0,.64),pos = 0);
axis(2, at=c(0,38), labels = T)

legend(.1, 35, legend=c("Riskier in Phase 1", "Riskier in Phase 2", "No change in risk-taking"),
       col=c("seagreen", "salmon",rgb(0.2,0.2,0.2,0.2)), lty=c(1,1,1,3), lwd=3, box.lty = 0, cex=.8, pt.cex=1)

dev.off();


pdf("/Volumes/shlab/Projects/CAP/figures/pgam_MargDist_HRB.pdf")
x = as.data.frame(matrix(nrow = 1,ncol=2, dimnames = list(c(NULL),c("phase1","phase2"))));
x$phase1 = mean(pgamBothPhs[,1])
x$phase2 = mean(pgamBothPhs[,2])

colnames(pgamBothPhs) <- c("phase1","phase2","id","diff");
pgamBothPhsdf = as.data.frame(pgamBothPhs)

p <- ggplot(pgamBothPhsdf, aes(x=phase1, y=phase2)) +
  scale_fill_brewer(palette="Dark2") +scale_colour_brewer(palette="Dark2")+ 
  theme_classic() + #geom_jitter(height = 0, width = 0.1,color="black", alpha=.2) + 
  geom_point(color="black",size=4,alpha=.2) + 
  scale_x_continuous(name ="p(gamble) Phase 1") +scale_y_continuous(name ="p(gamble) Phase 2") + 
  geom_point(data = x, mapping = aes(x = phase1, y = phase2), shape=23, color="red",fill="red",size=8,alpha=.5) + 
  geom_abline(intercept = 0, slope = 1, color="black", linetype="dashed") + 
  removeGrid() + coord_fixed(xlim=c(0,1),ylim=c(0,1));

ggExtra::ggMarginal(p,fill="grey",size=5, type="histogram")

dev.off();



# add a new variable for phase to be coded 0 and 1 in the dataset that combines phase 1 and phase 2
bothPhasesRDMqualtrics$phaseRecode = bothPhasesRDMqualtrics$phase;
bothPhasesRDMqualtrics$phaseRecode[bothPhasesRDMqualtrics$phaseRecode==1] =0;
bothPhasesRDMqualtrics$phaseRecode[bothPhasesRDMqualtrics$phaseRecode==2] =1;

# scale affective measures
bothPhasesRDMqualtrics$stais_score01 = bothPhasesRDMqualtrics$stais_score/max(bothPhasesRDMqualtrics$stais_score);
bothPhasesRDMqualtrics$stait_score01 = bothPhasesRDMqualtrics$stait_score/max(bothPhasesRDMqualtrics$stait_score);
bothPhasesRDMqualtrics$uclal_score01 = bothPhasesRDMqualtrics$uclal_score/max(bothPhasesRDMqualtrics$uclal_score);
bothPhasesRDMqualtrics$pss_score01 = bothPhasesRDMqualtrics$pss_score/max(bothPhasesRDMqualtrics$pss_score);
bothPhasesRDMqualtrics$CovidRisk01 = bothPhasesRDMqualtrics$CovidRisk/max(bothPhasesRDMqualtrics$CovidRisk, na.rm=T);

bothPhasesRDMqualtrics$CovidRisk01noNA = bothPhasesRDMqualtrics$CovidRisk/max(bothPhasesRDMqualtrics$CovidRisk, na.rm=T);
bothPhasesRDMqualtrics$CovidRisk01noNA[is.na(bothPhasesRDMqualtrics$CovidRisk01noNA)] = 0;

# scale past outcomes
phs1RDMqualtrics$poc1sc = phs1RDMqualtrics$poc1/max(phs1RDMqualtrics$riskyGain);
phs2RDMqualtrics$poc1sc = phs2RDMqualtrics$poc1/max(phs2RDMqualtrics$riskyGain);
bothPhasesRDMqualtrics$poc1sc = bothPhasesRDMqualtrics$poc1/max(bothPhasesRDMqualtrics$riskyGain);

# look at positive and negative shifts
bothPhasesRDMqualtrics$absShift = c(0,diff(bothPhasesRDMqualtrics$groundEV));
bothPhasesRDMqualtrics$absShift[bothPhasesRDMqualtrics$triNum==1] =0; #first trial is always 0
bothPhasesRDMqualtrics$posShift = bothPhasesRDMqualtrics$absShift*as.numeric(bothPhasesRDMqualtrics$absShift>0);
bothPhasesRDMqualtrics$negShift = bothPhasesRDMqualtrics$absShift*as.numeric(bothPhasesRDMqualtrics$absShift<0);

# need to calculate cumulative earnings for each day for each participant (above, the cumulative earnings and trial were across both days)

eachPhsEarnings = vector();
trinum = vector();

first = 1
for(s in 1:nSubPhs2){
  sub = bothPhasesRDMqualtrics[bothPhasesRDMqualtrics$prolificID==pIDphs2[s],]
  earningsDay = vector();
  indEndSesh = which(diff(sub$phase)!=0)
  indStartSesh = indEndSesh+1
  endSub = nrow(sub)
  
  # # Simpler code uses function cumsum()
  # for (t in 1:indEndSesh){
  #   earningsDay[t] = sum(sub$realOutcome[1:t]); #calculate total outcomes 
  # }
  # 
  # for (t in indStartSesh:endSub){
  #   earningsDay[t] = sum(sub$realOutcome[indStartSesh:t])
  # }
  
  earningsDay = c(cumsum(sub$realOutcome[1:indEndSesh]),
                  cumsum(sub$realOutcome[indStartSesh:endSub]));
  
  tri1 = 1:indEndSesh/indEndSesh
  tri2 = 1:(endSub-indEndSesh)/(endSub-indEndSesh)
  trial = c(tri1,tri2)
  
  allSeshEarn = c(0,earningsDay[1:(indEndSesh-1)],0,earningsDay[indStartSesh:(endSub-1)]);
  
  last=first+(endSub-1); #last row to store in eachDayEarnings
  eachPhsEarnings[first:last] = allSeshEarn; # store subjects cumulative earnings
  trinum[first:last] = trial
  first = last +1; #set first to be the row after the previous subjects last trial
}

bothPhasesRDMqualtrics$trialSesh = trinum; #store the scaled trial
bothPhasesRDMqualtrics$earningSesh = eachPhsEarnings; #store cumulative earnings for each day for each participant

bothPhasesRDMqualtrics$earningSeshSC = bothPhasesRDMqualtrics$earningSesh/max(bothPhasesRDMqualtrics$riskyGain);

bothPhasesRDMqualtrics$earningSeshSC01 = bothPhasesRDMqualtrics$earningSesh/max(bothPhasesRDMqualtrics$earningSesh);

# Make new variable "dayoverall" from 1:40 (instead of 1:20 and 1:20)
bothPhasesRDMqualtrics$dayoverall = bothPhasesRDMqualtrics$day + bothPhasesRDMqualtrics$phaseRecode*20;
# B/c even tho 1:20 is true within-phase, it's actually 1:40 across phases

# Rescale "Day" to be 0-1
phs1RDMqualtrics$day01 = phs1RDMqualtrics$day/max(phs1RDMqualtrics$day);
phs2RDMqualtrics$day01 = phs2RDMqualtrics$day/max(phs2RDMqualtrics$day);
bothPhasesRDMqualtrics$day01 = bothPhasesRDMqualtrics$day/max(bothPhasesRDMqualtrics$day);
bothPhasesRDMqualtrics$dayoverall01 = bothPhasesRDMqualtrics$dayoverall/max(bothPhasesRDMqualtrics$dayoverall);


# Plot p(gamble) over time, and over time by quartile
pgamOverTime = as.data.frame(matrix(data=NA, nrow = 40, ncol=5,dimnames=list(c(NULL),c('pgamQ1','pgamQ2','pgamQ3','pgamQ4','pgamOverall'))));
for (d in 1:20) {
  
  tmpind = phs1RDMqualtrics$quartile==1 & phs1RDMqualtrics$day==d;
  pgamOverTime$pgamQ1[d] = mean(tapply(phs1RDMqualtrics$choice[tmpind], phs1RDMqualtrics$prolificID[tmpind], mean))
  tmpind = phs2RDMqualtrics$quartile==1 & phs2RDMqualtrics$day==d;
  pgamOverTime$pgamQ1[d+20] = mean(tapply(phs2RDMqualtrics$choice[tmpind], phs2RDMqualtrics$prolificID[tmpind], mean))
  
  tmpind = phs1RDMqualtrics$quartile==2 & phs1RDMqualtrics$day==d;
  pgamOverTime$pgamQ2[d] = mean(tapply(phs1RDMqualtrics$choice[tmpind], phs1RDMqualtrics$prolificID[tmpind], mean))
  tmpind = phs2RDMqualtrics$quartile==2 & phs2RDMqualtrics$day==d;
  pgamOverTime$pgamQ2[d+20] = mean(tapply(phs2RDMqualtrics$choice[tmpind], phs2RDMqualtrics$prolificID[tmpind], mean))

  tmpind = phs1RDMqualtrics$quartile==3 & phs1RDMqualtrics$day==d;
  pgamOverTime$pgamQ3[d] = mean(tapply(phs1RDMqualtrics$choice[tmpind], phs1RDMqualtrics$prolificID[tmpind], mean))
  tmpind = phs2RDMqualtrics$quartile==3 & phs2RDMqualtrics$day==d;
  pgamOverTime$pgamQ3[d+20] = mean(tapply(phs2RDMqualtrics$choice[tmpind], phs2RDMqualtrics$prolificID[tmpind], mean))

  tmpind = phs1RDMqualtrics$quartile==4 & phs1RDMqualtrics$day==d;
  pgamOverTime$pgamQ4[d] = mean(tapply(phs1RDMqualtrics$choice[tmpind], phs1RDMqualtrics$prolificID[tmpind], mean))
  tmpind = phs2RDMqualtrics$quartile==4 & phs2RDMqualtrics$day==d;
  pgamOverTime$pgamQ4[d+20] = mean(tapply(phs2RDMqualtrics$choice[tmpind], phs2RDMqualtrics$prolificID[tmpind], mean))

  tmpind = phs1RDMqualtrics$day==d;
  pgamOverTime$pgamOverall[d] = mean(tapply(phs1RDMqualtrics$choice[tmpind], phs1RDMqualtrics$prolificID[tmpind], mean))
  tmpind = phs2RDMqualtrics$day==d;
  pgamOverTime$pgamOverall[d+20] = mean(tapply(phs2RDMqualtrics$choice[tmpind], phs2RDMqualtrics$prolificID[tmpind], mean))
};

plot(pgamOverTime$pgamQ1, type="l", col="blue", xlab="day", ylab = "p(gamble)", lwd=2,xlim=c(1,40),ylim=c(0,1), main="Gambling Over Time (all data)")
lines(pgamOverTime$pgamQ2, col="green", lwd=2)
lines(pgamOverTime$pgamQ3, col="orange", lwd=2)
lines(pgamOverTime$pgamQ4, col="yellow", lwd=2)
lines(pgamOverTime$pgamOverall, col="black", lwd=4)
lines(x=c(20,20),y=c(0,1),col="black",lty="dashed",lwd=3)



# Regressions on affective measures
am1_stais = lmer(stais ~ 1 + Quartile + phaseRecode + dayoverall01 + Age + (1 | prolificID), data=subInfoScoreQualBothPhasesStacked)
# Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept)   52.23786    2.29890 546.82182  22.723  < 2e-16 ***
#   Quartile      -0.83983    0.43419 575.84622  -1.934   0.0536 .  
#   phaseRecode   -3.71034    1.78482 608.75904  -2.079   0.0381 *        BETTER IN PHASE 2
#   dayoverall01   3.09761    3.43871 531.58870   0.901   0.3681    
#   Age           -0.20047    0.04726 518.49110  -4.242 2.63e-05 ***      BETTER WITH OLDER AGE

am1_stait = lmer(stait ~ 1 + Quartile + phaseRecode + dayoverall01 + Age + (1 | prolificID), data=subInfoScoreQualBothPhasesStacked)
# Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept)   55.28678    2.43044 584.76380  22.748  < 2e-16 ***
#   Quartile      -1.00057    0.44270 694.45872  -2.260  0.02412 *        BETTER IN WORSE QUARTILES??
#   phaseRecode   -5.26576    1.86868 559.87819  -2.818  0.00500 **       BETTER IN PHASE 2?
#   dayoverall01   9.71687    3.69890 537.91851   2.627  0.00886 **       WORSE WITH INCREASING DAYS
#   Age           -0.31661    0.05106 536.20820  -6.201 1.12e-09 ***      BETTER WITH INCREASING AGE

am1_uclal = lmer(uclal ~ 1 + Quartile + phaseRecode + dayoverall01 + Age + (1 | prolificID), data=subInfoScoreQualBothPhasesStacked)
# Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept)   53.32250    2.41713 576.40853  22.060  < 2e-16 ***
#   Quartile      -0.62018    0.44428 666.62329  -1.396 0.163203    
#   phaseRecode   -2.15343    1.85515 563.83565  -1.161 0.246221    
#   dayoverall01   3.18724    3.66346 536.90391   0.870 0.384685    
#   Age           -0.19247    0.05055 533.90109  -3.807 0.000157 ***      BETTER WITH OLDER AGE

# Would be good to add in measures of social distancing

am1_pss = lmer(pss ~ 1 + Quartile + phaseRecode + dayoverall01 + Age + (1 | prolificID), data=subInfoScoreQualBothPhasesStacked)
# Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept)   25.15661    1.49973 559.01395  16.774  < 2e-16 ***
#   Quartile      -0.50198    0.28057 609.86224  -1.789  0.07409 .  
#   phaseRecode   -3.53882    1.15258 582.26618  -3.070  0.00224 **       BETTER IN PHASE 2?
#   dayoverall01   5.70945    2.25406 535.22286   2.533  0.01160 *        WORSE WITH INCREASING DAYS
#   Age           -0.17640    0.03106 527.73785  -5.679 2.24e-08 ***      BETTER WITH OLDER AGE

am1_covidrisk = lmer(CovidRisk ~ 1 + Quartile + phaseRecode + dayoverall01 + Age + (1 | prolificID), data=subInfoScoreQualBothPhasesStacked)
# Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)    5.934958   0.376747 538.350294  15.753   <2e-16 ***
#   Quartile      -0.027076   0.071181 566.702466  -0.380    0.704    
#   phaseRecode    0.213818   0.292733 603.213364   0.730    0.465    
#   dayoverall01  -0.896761   0.563399 523.221113  -1.592    0.112    
#   Age           -0.012453   0.007743 509.835550  -1.608    0.108    

# SUMMARY
# QUARTILE significantly predicts...
#   - worse state anxiety, loneliness, and COVID risk
#   - weirdly, also predicts better PSS (contrast effect?)
# DAY significantly predicts...
#   - worse *trait* anxiety
#   - lower COVID risk
# AGE significantly predicts...
#   - lower state anxiety, trait anxiety, loneliness, and chronic stress



# Simple regressions for risky decision-making part
# some regressions for n=355
m1phase = glmer(choice~ 1 + gainSC + altSC + groundEV + phaseRecode + dayoverall01 +(0 + gainSC + altSC|prolificID), data=bothPhasesRDMqualtrics, family="binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
m1stais = glmer(choice~ 1 + gainSC + altSC + groundEV + stais_score01 +(0 + gainSC + altSC|prolificID), data=bothPhasesRDMqualtrics, family="binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
m1stait = glmer(choice~ 1 + gainSC + altSC + groundEV + stait_score01 +(0 + gainSC + altSC|prolificID), data=bothPhasesRDMqualtrics, family="binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
m1uclal = glmer(choice~ 1 + gainSC + altSC + groundEV + uclal_score01 +(0 + gainSC + altSC|prolificID), data=bothPhasesRDMqualtrics, family="binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
m1pss   =     glmer(choice~ 1 + gainSC + altSC + groundEV + pss_score01 +(0 + gainSC + altSC|prolificID), data=bothPhasesRDMqualtrics, family="binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
m1covidrisk = glmer(choice~ 1 + gainSC + altSC + groundEV + CovidRisk01noNA +(0 + gainSC + altSC|prolificID), data=bothPhasesRDMqualtrics, family="binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));

# AFfective measures...
# - No observed effects (at this level) for STAIS, STAIT, UCLA-L, or PSS.
# - COVID RISK measure predicts less risk-taking. 

# model w/ COVID RISK outperforms (AIC 61624.19) model with PHASE and DAY (AIC 61660.15)

summary(m1covidrisk)
  #                   Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)      -0.02006    0.05116  -0.392  0.69498    
  # gainSC           13.27285    0.67391  19.695  < 2e-16 ***
  # altSC           -33.94995    1.25567 -27.037  < 2e-16 ***
  # groundEV          0.06181    0.02385   2.592  0.00955 ** 
  # CovidRisk01noNA  -0.83982    0.07770 -10.809  < 2e-16 ***
#
# So should THIS be the base model for the rest? 
bothPhasesRDMqualtrics$predm1cr = predict(m1covidrisk,type='link');

m2_pastotcshiftearningsXcovid = glmer(choice ~ 0 + poc1sc*CovidRisk01noNA + posShift*CovidRisk01noNA + earningSeshSC01*poc1sc*CovidRisk01noNA + (1 | prolificID),
                   data = bothPhasesRDMqualtrics, family = 'binomial', offset = predm1cr)


summary(m1phase); #AIC  = 61660.1
  #               Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)   -0.51687    0.04648 -11.121  < 2e-16 ***
  # gainSC        13.23978    0.67726  19.549  < 2e-16 ***
  # altSC        -34.01101    1.24036 -27.420  < 2e-16 ***
  # groundEV       0.06525    0.02397   2.722  0.00650 ** 
  # phaseRecode   -0.39811    0.08148  -4.886 1.03e-06 ***
  # dayoverall01   0.40832    0.15806   2.583  0.00979 ** 
  # 
# Opposing overall effects of phase & day (less risk-taking in phase 2, but more with each day)

bothPhasesRDMqualtrics$pred = predict(m1phase,type='link');
m2_pastotc = glmer(choice ~ 0 + poc1sc + (1 | prolificID),
                      data = bothPhasesRDMqualtrics, family = 'binomial', offset = pred)
# Estimate Std. Error z value Pr(>|z|)
# poc1sc -0.04573    0.03190  -1.434    0.152

m2_pastotcphase = glmer(choice ~ 0 + poc1sc*phaseRecode + (1 | prolificID),
                   data = bothPhasesRDMqualtrics, family = 'binomial', offset = pred)
#                       Estimate Std. Error z value Pr(>|z|)  
#   poc1sc             -0.088534   0.043980  -2.013   0.0441 *
#   phaseRecode         0.005025   0.022600   0.222   0.8241  
#   poc1sc:phaseRecode  0.076569   0.081560   0.939   0.3478  

m2_pastotcphaseday = glmer(choice ~ 0 + poc1sc*phaseRecode*dayoverall01 + (1 | prolificID),
                        data = bothPhasesRDMqualtrics, family = 'binomial', offset = pred)
#                                   Estimate Std. Error z value Pr(>|z|)  
#   poc1sc                           0.05292    0.08555   0.619   0.5362  
#   phaseRecode                     -0.04500    0.11816  -0.381   0.7033  
#   dayoverall01                     0.12966    0.07776   1.667   0.0954 .
#   poc1sc:phaseRecode              -0.54277    0.37664  -1.441   0.1496  
#   poc1sc:dayoverall01             -0.86692    0.34525  -2.511   0.0120 *
#   phaseRecode:dayoverall01        -0.06016    0.17292  -0.348   0.7279  
#   poc1sc:phaseRecode:dayoverall01  1.49709    0.58879   2.543   0.0110 *

# Previous outcome effect gets more negative with days in phase 1, but not phase 2...
#
# Phase 1, Day 1: previous otc effect 0.031
# Phase 1, Day 20: previous otc effect -0.38
# Phase 2, Day 1: previous otc effect 0.38
# Phase 2, Day 20: previous otc effect 0.68

# (SEPARATE PHASE 1 & 2 REGRESSIONS BELOW PARTIALLY CONFIRM DIFF. PATTERNS)

# Confirm in separate phase 1 & 2 regressions
m1phase1 = glmer(choice~ 1 + gainSC + altSC + groundEV + day01 + (0+gainSC + altSC|prolificID), 
                 data=phs1RDMqualtrics, family="binomial",
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -0.58447    0.05553 -10.525  < 2e-16 ***
#   gainSC       13.62890    0.68185  19.988  < 2e-16 ***
#   altSC       -38.96690    1.31122 -29.718  < 2e-16 ***
#   groundEV      0.13860    0.03129   4.430 9.42e-06 ***
#   day01         0.24072    0.09176   2.623  0.00871 ** 

phs1RDMqualtrics$pred = predict(m1phase1,type='link');
m2p1_pastotc = glmer(choice ~ 0 + poc1sc*day01 + (1 | prolificID),
                   data = phs1RDMqualtrics, family = 'binomial', offset = pred)
#                 Estimate Std. Error z value Pr(>|z|)   
#   poc1sc       -0.11609    0.07496  -1.549  0.12148   
#   day01         0.09014    0.03043   2.962  0.00305 **
#   poc1sc:day01 -0.17603    0.14038  -1.254  0.20984   

# PHASE 1 finds a strong positive effect of day, but no significant effect of potc (tho it goes neg)

m1phase2 = glmer(choice~ 1 + gainSC + altSC + groundEV + day01 + (0+gainSC + altSC|prolificID), 
                 data=phs2RDMqualtrics, family="binomial",
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -0.26779    0.07061  -3.793 0.000149 ***
#   gainSC       12.71671    0.88415  14.383  < 2e-16 ***
#   altSC       -41.97234    1.57460 -26.656  < 2e-16 ***
#   groundEV      0.16406    0.03773   4.348 1.37e-05 ***
#   day01        -0.15462    0.12146  -1.273 0.203037    

phs2RDMqualtrics$pred = predict(m1phase2,type='link');
m2p2_pastotc = glmer(choice ~ 0 + poc1sc*day01 + (1 | prolificID),
                     data = phs2RDMqualtrics, family = 'binomial', offset = pred)
#               Estimate Std. Error z value Pr(>|z|)
# poc1sc       -0.01568    0.09854  -0.159    0.874
# day01        -0.01500    0.04149  -0.362    0.718
# poc1sc:day01  0.10185    0.19378   0.526    0.599

# PHASE 2 FINDS NO NET EFFECTS OF POTC OR DAY (OR INTERACTION)



m3_pastotcshiftphase = glmer(choice ~ 0 + poc1sc*phaseRecode + posShift*phaseRecode + negShift*phaseRecode + (1 | prolificID),
                        data = bothPhasesRDMqualtrics, family = 'binomial', offset = pred,
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
#                         Estimate Std. Error z value Pr(>|z|)    
#   poc1sc               -0.115631   0.045093  -2.564 0.010339 *      PREV. OTC
#   phaseRecode          -0.005309   0.022968  -0.231 0.817196    
#   posShift              0.022048   0.006361   3.466 0.000528 ***    POS SHIFT
#   negShift             -0.008859   0.005885  -1.505 0.132255    
#   poc1sc:phaseRecode    0.106803   0.082628   1.293 0.196160    
#   phaseRecode:posShift -0.007117   0.009084  -0.783 0.433366    
#   phaseRecode:negShift  0.002069   0.008329   0.248 0.803781    
#
# No interactions w/ phase.

m3_pastotcshiftphaseday = glmer(choice ~ 0 + poc1sc*phaseRecode*dayoverall01 + posShift*phaseRecode*dayoverall01 + negShift*phaseRecode*dayoverall01 + (1 | prolificID),
                             data = bothPhasesRDMqualtrics, family = 'binomial', offset = pred,
                             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));

  #                                    Estimate Std. Error z value Pr(>|z|)   
  # poc1sc                             0.030867   0.087354   0.353  0.72382   
  # phaseRecode                       -0.047358   0.118933  -0.398  0.69049   
  # dayoverall01                       0.088510   0.079112   1.119  0.26323   
  # posShift                           0.029868   0.012346   2.419  0.01556 *     POSITIVE SHIFT EFFECT
  # negShift                          -0.002586   0.011674  -0.221  0.82471   
  # poc1sc:phaseRecode                -0.583732   0.372921  -1.565  0.11751   
  # poc1sc:dayoverall01               -0.787527   0.348105  -2.262  0.02368 *     NEG POTC EFFECT INCREASES W/ DAY
  # phaseRecode:dayoverall01          -0.030099   0.174427  -0.173  0.86300   
  # phaseRecode:posShift              -0.035711   0.035730  -0.999  0.31756   
  # dayoverall01:posShift             -0.036195   0.043242  -0.837  0.40257   
  # phaseRecode:negShift              -0.042856   0.033369  -1.284  0.19903   
  # dayoverall01:negShift             -0.023679   0.041600  -0.569  0.56922   
  # poc1sc:phaseRecode:dayoverall01    1.507300   0.584438   2.579  0.00991 **    NEG POTC EFFECT REVERSES IN PHASE 2
  # phaseRecode:dayoverall01:posShift  0.063555   0.061323   1.036  0.30002   
  # phaseRecode:dayoverall01:negShift  0.075544   0.058538   1.290  0.19688   
#
# Replicates M2 findings + positive shift effect.


# So things are happening with previous outcome, positive shift, phase, and day. Neg shift doesn't show anything. 

m4_pastotcshiftearningsphaseday = glmer(choice ~ 0 + poc1sc*phaseRecode*dayoverall01 + 
                                          posShift*phaseRecode*dayoverall01 + 
                                          earningSeshSC01*poc1sc*phaseRecode*dayoverall01 + (1 | prolificID),
                                data = bothPhasesRDMqualtrics, family = 'binomial', offset = pred,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
summary(m4_pastotcshiftearningsphaseday)
# NOTE: DOES NOT CONVERGE & TOSSES UNIDENTIFIABLE ERROR W/ LARGE EIGENVALUES; NEED TO RESCALE POS-SHIFT REGRESSOR?
  # AIC 57572.3
  #                                                  Estimate Std. Error z value Pr(>|z|)    
  # poc1sc                                           0.49158    0.18204   2.700 0.006924 **     POS OTC EFFECT?
  # phaseRecode                                     -0.19078    0.26299  -0.725 0.468206    
  # dayoverall01                                     0.66091    0.15186   4.352 1.35e-05 ***    POS EFFECT OF DAY
  # posShift                                         0.03031    0.01250   2.424 0.015347 *      POS EFFECT OF POSSHIFT
  # earningSeshSC01                                 -0.05643    0.11083  -0.509 0.610620    
  # poc1sc:phaseRecode                              -0.47457    0.93908  -0.505 0.613309    
  # poc1sc:dayoverall01                             -2.50190    0.75290  -3.323 0.000891 ***    NEG POTC X DAY EFFECT
  # phaseRecode:dayoverall01                        -0.43699    0.38513  -1.135 0.256525    
  # phaseRecode:posShift                            -0.03756    0.03581  -1.049 0.294194    
  # dayoverall01:posShift                           -0.03967    0.04373  -0.907 0.364259    
  # poc1sc:earningSeshSC01                          -1.13689    0.51253  -2.218 0.026543 *      NEG EARNINGS X OTC  (MORE NEG. OTC W/ MORE $)
  # phaseRecode:earningSeshSC01                      0.50587    0.67534   0.749 0.453821    
  # dayoverall01:earningSeshSC01                    -1.46321    0.50388  -2.904 0.003686 **     NEG EARNINGS X DAY  (LATER DAYS HAVE MORE OF A NEG. EARNINGS EFFECT)
  # poc1sc:phaseRecode:dayoverall01                  2.37468    1.48567   1.598 0.109956    
  # phaseRecode:dayoverall01:posShift                0.06860    0.06169   1.112 0.266131    
  # poc1sc:phaseRecode:earningSeshSC01              -0.32783    2.36338  -0.139 0.889677    
  # poc1sc:dayoverall01:earningSeshSC01              4.53544    2.03090   2.233 0.025534 *      POS OTC X EARNINGS X DAY (COUNTERACTS NEG. OTC X EARNINGS EFFECT EVENTUALLY)
  # phaseRecode:dayoverall01:earningSeshSC01         0.94944    1.03761   0.915 0.360180    
  # poc1sc:phaseRecode:dayoverall01:earningSeshSC01 -2.33581    3.82785  -0.610 0.541721    


m4_pastotcshiftearnings = glmer(choice ~ 0 + poc1sc + 
                                          posShift + 
                                          earningSeshSC01*poc1sc + (1 | prolificID),
                                        data = bothPhasesRDMqualtrics, family = 'binomial', offset = pred,
                                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)));
summary(m4_pastotcshiftearnings)
  #                         Estimate Std. Error z value Pr(>|z|)    
  # poc1sc                  0.064181   0.062852   1.021    0.307    
  # posShift                0.018695   0.004549   4.109 3.97e-05 ***      POS SHIFT EFFECT IS STRONG
  # earningSeshSC01        -0.032822   0.039530  -0.830    0.406    
  # poc1sc:earningSeshSC01 -0.261904   0.173306  -1.511    0.131    




m4_pocShiftEarningsphase = glmer(choice ~ 0 + poc1sc*phaseRecode + 
                                       posShift*phaseRecode + negShift*phaseRecode + 
                                       earningSeshSC + trialSesh + (1 | prolificID),
                             data = bothPhasesRDMqualtrics, family = 'binomial', offset = pred)


bothPhasesRDMqualtrics$triLevResPhase = resid(m1phase,type='response'); # save simple residuals (observed-fitted)

# a simple model looking for general effects of past outcome, shift, and earnings
simple1 = lmer(triLevResPhase ~ 0 + poc1sc +(1|prolificID), data=bothPhasesRDMqualtrics, REML = F);
#AIC = 52310.2 
#Estimate Std. Error         df t value Pr(>|t|)  
#poc1sc -6.499e-03  3.609e-03  1.031e+03  -1.801    0.072 .

simple2 = lmer(triLevResPhase ~ 0 + poc1sc + posShift+ negShift + (1|prolificID), data=bothPhasesRDMqualtrics, REML = F);
#AIC = 52284.5
# Estimate Std. Error         df t value Pr(>|t|)    
# poc1sc   -6.641e-03  3.705e-03  1.215e+03  -1.792   0.0733 .  
# posShift  2.507e-03  4.807e-04  8.015e+04   5.215 1.85e-07 ***
# negShift  7.257e-04  4.925e-04  8.249e+04   1.474   0.1406    

simple3 = lmer(triLevResPhase ~ 0 + poc1sc+ posShift + earningSeshSC+ trialSesh + (1|prolificID), data=bothPhasesRDMqualtrics, REML = F); # AIX = 52002.1
# Estimate Std. Error         df t value Pr(>|t|)    
# poc1sc         2.100e-02  4.695e-03  3.867e+04   4.473 7.72e-06 ***
# posShift       3.691e-03  4.856e-04  8.262e+04   7.601 2.96e-14 ***
# earningSeshSC  5.732e-03  4.493e-04  3.325e+03  12.758  < 2e-16 ***
# trialSesh     -2.038e-01  1.384e-02  3.085e+03 -14.732  < 2e-16 ***

#AIC = 52004.1
# Estimate Std. Error         df t value Pr(>|t|)    
# poc1sc                2.014e-02  7.034e-03  1.322e+04   2.862  0.00421 ** 
# earningSeshSC         5.709e-03  4.708e-04  3.548e+03  12.126  < 2e-16 ***
# posShift              3.697e-03  4.868e-04  8.257e+04   7.595 3.12e-14 ***
# trialSesh            -2.034e-01  1.409e-02  3.305e+03 -14.439  < 2e-16 ***
# poc1sc:earningSeshSC  7.439e-05  4.494e-04  3.112e+04   0.166  0.86852  

# 
# # past outcome
# poc = lmer(triLevResPhase ~ 0 + poc1sc*phaseRecode + (1|prolificID), data=bothPhasesRDMqualtrics, REML = F);
# # AIC = 52266.6
# # Estimate Std. Error         df t value Pr(>|t|)    
# # poc1sc             -2.864e-02  4.924e-03  3.321e+03  -5.815 6.64e-09 ***
# # phaseRecode        -7.564e-03  2.658e-03  7.260e+03  -2.846  0.00444 ** 
# # poc1sc:phaseRecode  6.089e-02  9.262e-03  2.723e+04   6.574 4.97e-11 ***
#   
# 
# pocPhs1n355 = lmer(triLevResPhase ~ 0 + poc1sc + (1|prolificID), data=bothPhasesRDMqualtrics[bothPhasesRDMqualtrics$phase==1,], REML = F);
# #Estimate Std. Error         df t value Pr(>|t|)
# #poc1sc -4.773e-04  7.004e-03  1.107e+04  -0.068    0.946
# 
# 
# pocPhs2n355 = lmer(triLevResPhase ~ 0 + poc1sc + (1|prolificID), data=bothPhasesRDMqualtrics[bothPhasesRDMqualtrics$phase==2,], REML = F);
# #Estimate Std. Error        df t value Pr(>|t|)  
# #poc1sc 1.564e-02  6.942e-03 1.576e+04   2.254   0.0242 *
# 
# 
# # shift
# shift = lmer(triLevResPhase ~ 0 + poc1sc*phaseRecode + posShift*phaseRecode + negShift*phaseRecode + (1|prolificID), data=bothPhasesRDMqualtrics, REML = F); #52241.1
# # Estimate Std. Error         df t value Pr(>|t|)    
# # poc1sc               -2.891e-02  5.044e-03  3.943e+03  -5.732 1.07e-08 ***
# # phaseRecode          -9.485e-03  2.708e-03  7.664e+03  -3.502 0.000464 ***
# # posShift              2.509e-03  6.731e-04  8.224e+04   3.728 0.000193 ***
# # negShift              8.571e-04  6.966e-04  8.244e+04   1.230 0.218549    
# # poc1sc:phaseRecode    6.453e-02  9.378e-03  2.964e+04   6.881 6.06e-12 ***
# # phaseRecode:posShift  3.876e-04  9.692e-04  8.262e+04   0.400 0.689224    
# # phaseRecode:negShift -3.486e-04  9.853e-04  8.238e+04  -0.354 0.723476  
# 
# 
# shiftPhs1n355 = lmer(triLevResPhase ~ 0 + poc1sc + posShift + negShift + (1|prolificID), data=bothPhasesRDMqualtrics[bothPhasesRDMqualtrics$phase==1,], REML = F); #26178.4
# #Estimate Std. Error        df t value Pr(>|t|)    
# #poc1sc   2.291e-03  7.058e-03 1.124e+04   0.325    0.746    
# #posShift 3.338e-03  6.772e-04 4.129e+04   4.928 8.34e-07 ***
# #negShift 6.010e-04  6.923e-04 4.101e+04   0.868    0.385   
# 
# 
# shiftPhs2n355 = lmer(triLevResPhase ~ 0 + poc1sc + posShift + negShift +(1|prolificID), data=bothPhasesRDMqualtrics[bothPhasesRDMqualtrics$phase==2,], REML = F); #21682.3
# # Estimate Std. Error        df t value Pr(>|t|)    
# # poc1sc   1.790e-02  6.995e-03 1.583e+04   2.559   0.0105 *  
# # posShift 2.770e-03  6.521e-04 4.131e+04   4.248 2.16e-05 ***
# # negShift 3.994e-04  6.538e-04 4.102e+04   0.611   0.5412    
# 
# 
# #cumulative earnings
# # shift
# earnings = lmer(triLevResPhase ~ 0 + poc1sc*phaseRecode + posShift*phaseRecode + earningSeshSC*phaseRecode+ (1|prolificID), data=bothPhasesRDMqualtrics, REML = F); #52241.1
# # Estimate Std. Error         df t value Pr(>|t|)    
# # poc1sc                     1.629e-02  6.571e-03  6.351e+04   2.479   0.0132 *  
# # phaseRecode               -1.500e-02  3.805e-03  1.890e+04  -3.942 8.11e-05 ***
# # posShift                   3.479e-03  6.784e-04  8.258e+04   5.128 2.93e-07 ***
# # earningSeshSC             -1.347e-03  1.257e-04  2.281e+04 -10.713  < 2e-16 ***
# # poc1sc:phaseRecode         1.849e-02  1.025e-02  7.734e+04   1.804   0.0713 .  
# # phaseRecode:posShift      -5.530e-04  9.724e-04  8.249e+04  -0.569   0.5695    
# # phaseRecode:earningSeshSC  1.665e-03  2.177e-04  5.290e+04   7.651 2.02e-14 ***  
# 
# 
# 
# #THINGS TO DO AND LOOK INTO
# #   1) COMBINE AND ORGANIZE SO THAT THEE SUBJECT IDS LINE UP ACROSS DAYS - ADD DAY COLUMN
# #   2) DOES DAY INFLUENCE RISK TAKING? 
# #   3) LOOK AT RESIDUALS ACROSS DAYS FOR THE 357 PARTICIPANTS
# #   4) RERUN THE TRIAL LEVEL AND POC MODEL ONCE CHELSEY GETS BACK WITH QUESTIONNAIRE DATA
# 
# 
# 
# #individual level:
# #1) run a trial-level model (phase 1 and phase 2 separately) to get residuals
# #2) look at past outcome effect (phase 1 and phase 2 separately)
# #3) plot the point estimate for each participants past outcome effect
# 
# subRegResultsPhs1 = as.data.frame(matrix(data=NA, nrow = nSubPhs1, ncol = 7, dimnames = list(c(NULL), c("mean", "median","min", "max", "pocEst", "pocP", "pID"))))
# triLevPhs1 = list();
# pocPhs1 = list();
# 
# for (s in 1:nSubPhs1) {
#   sub = phs1RDMqualtrics[phs1RDMqualtrics$subID==subNumsPhs1[s],]
#   triLevPhs1[[s]] = glm(choice~1+gainSC+altSC+groundEV, data=sub, family=binomial());
#   sub$triLevRes = resid(triLevPhs1[[s]], type="response");
#   
#   pocPhs1[[s]] = lm(triLevRes ~ poc1sc, data=sub); 
#   
#   savesummary = summary(pocPhs1[[s]])
#   
#   subRegResultsPhs1$mean[s] = mean(sub$triLevRes);
#   subRegResultsPhs1$median[s] = median(sub$triLevRes);
#   subRegResultsPhs1$min[s] = min(sub$triLevRes);
#   subRegResultsPhs1$max[s] = max(sub$triLevRes);
#   subRegResultsPhs1$pocEst[s] = pocPhs1[[s]]$coefficients[2]
#   subRegResultsPhs1$pocP[s]=savesummary$coefficients[8]
#   subRegResultsPhs1$pID[s] = as.character(sub$participant[1])
# };
# 
# plot(subRegResultsPhs1$pocEst,col=(subRegResultsPhs1$pocP>.05)+3, pch=19,ylim=c(-.8, .4));
# # mean point estimate for phase 1 past outcome effect = -0.026104 (20 of those significant and negative; 7 significant and positive)
# 
# 
# subRegResultsPhs2 = as.data.frame(matrix(data=NA, nrow = nSubPhs2, ncol = 7, dimnames = list(c(NULL), c("mean", "median","min", "max", "pocEst", "pocP", "pID"))))
# triLevPhs2 = list();
# pocPhs2 = list();
# 
# for (s in 1:nSubPhs2) {
#   sub = phs2RDMqualtrics[phs2RDMqualtrics$subID==subNumsPhs2[s],]
#   triLevPhs2[[s]] = glm(choice~1+gainSC+altSC+groundEV, data=sub, family=binomial());
#   sub$triLevRes = resid(triLevPhs2[[s]], type="response");
#   
#   pocPhs2[[s]] = lm(triLevRes ~ poc1sc, data=sub); 
#   
#   savesummary = summary(pocPhs2[[s]])
#   
#   subRegResultsPhs2$mean[s] = mean(sub$triLevRes);
#   subRegResultsPhs2$median[s] = median(sub$triLevRes);
#   subRegResultsPhs2$min[s] = min(sub$triLevRes);
#   subRegResultsPhs2$max[s] = max(sub$triLevRes);
#   subRegResultsPhs2$pocEst[s] = pocPhs2[[s]]$coefficients[2]
#   subRegResultsPhs2$pocP[s]=savesummary$coefficients[8]
#   subRegResultsPhs2$pID[s] = as.character(sub$participant[1])
#   
# };
# 
# plot(subRegResultsPhs2$pocEst,col=(subRegResultsPhs2$pocP>.05)+3, pch=19, ylim=c(-.8, .4));
# # mean point estimate for phase 2 past outcome effect =-0.0005549842 (18 of those significant and negative; 14 significant and positive)
# 
# # who are the people with significant individual effects of past outcome? are they the same people?
# indneg1 = which(subRegResultsPhs1$pocEst<0 & subRegResultsPhs1$pocP<.05); # subs with poc est that are neg and sig
# indpos1 = which(subRegResultsPhs1$pocEst>0 & subRegResultsPhs1$pocP<.05); # subs with poc est that are pos and sig
# 
# indneg2 = which(subRegResultsPhs2$pocEst<0 & subRegResultsPhs2$pocP<.05); # subs with poc est that are neg and sig
# indpos2 = which(subRegResultsPhs2$pocEst>0 & subRegResultsPhs2$pocP<.05); # subs with poc est that are pos and sig
# 
# subRegResultsPhs1$pID[indneg1];
# subRegResultsPhs2$pID[indneg2];
# subRegResultsPhs1$pID[indpos1];
# subRegResultsPhs2$pID[indpos2];
# #some of the same participants show up in phase 1 and phase 2 but actually some variability partly because some participants are in both phase 1 and phase 2 -   THIS MIGHT MAKE MORE SENSE TO DO THE COMPARISON IN ONLY THOSE WHO WERE IN BOTH PHASE 1 AND PHASE 2
# 
# 
