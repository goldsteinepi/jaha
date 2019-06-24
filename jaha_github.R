#################
# Reproducibility study of Saint-Maurice PF, Troiano RP, Matthews CE, Kraus WE. Moderate-to-Vigorous Physical Activity and All-Cause Mortality: Do Bouts Matter? J Am Heart Assoc. 2018 Mar 22;7(6).
# Requires: NHIS and NHANES public use data files, 2003-2006
# Citation: Goldstein ND, Hamra GB, Harper S. Are descriptions of methods alone sufficient for study reproducibility? An example from the cardiovascular literature. Manuscript in preparation.

# NOTES:
# This script attemps to reproduce the findings in the JAHA manuscript
# The datasets must be downloaded in advance with pre-processing of files done in SAS (see notes)
# The data dictionary is contained within, as each variable is operationalized its possible values are indicated
# The script is meant to be run sequentially, from top down
# Any mistakes are our own, and not mean to be reflective of the work of Saint-Maurice et al.
#################

#clear the working environment
rm(list=ls())


### FUNCTIONS ### 

library(foreign) #SAS file support
library(sas7bdat) #SAS file support
library(psych) #describe, describeBy
library(gmodels) #CrossTable
library(survival) #Cox regression
library(survey) #complex survey sampling


### READ DATA ###

#set data directory, note: place NHANES data in subdirectory "NHANES" and NCHS data in subdirectory "NCHS"
setwd("~/")


################################################################
## Create analytic dataset by running code from here to 'END' ##
################################################################

#NHANES: exposure and covariates

#demographics: age, sex, race-ethnicity, education
#https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2003
demo03 = read.xport(file="NHANES/2003-4 DEMO_C.XPT")
#https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005
demo05 = read.xport(file="NHANES/2005-6 DEMO_D.XPT")

#questionnaire: alcohol, smoking, diabetes, medical conditions (coronary artery disease, stroke, cancer), physical functioning (mobility limitation)
#https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Questionnaire&CycleBeginYear=2003
alq03 = read.xport(file="NHANES/2003-4 ALQ_C.XPT")
smq03 = read.xport(file="NHANES/2003-4 SMQ_C.XPT")
diq03 = read.xport(file="NHANES/2003-4 DIQ_C.XPT")
mcq03 = read.xport(file="NHANES/2003-4 MCQ_C.XPT")
pfq03 = read.xport(file="NHANES/2003-4 PFQ_C.XPT")
#https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Questionnaire&CycleBeginYear=2005
alq05 = read.xport(file="NHANES/2005-6 ALQ_D.XPT")
smq05 = read.xport(file="NHANES/2005-6 SMQ_D.XPT")
diq05 = read.xport(file="NHANES/2005-6 DIQ_D.XPT")
mcq05 = read.xport(file="NHANES/2005-6 MCQ_D.XPT")
pfq05 = read.xport(file="NHANES/2005-6 PFQ_D.XPT")

#examination: body measures (bmi), physical activity
#reproduction note: SAS programs from this website were used to create perperson datasets for both NHANES survey years: https://epi.grants.cancer.gov/nhanes_pam/
#https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=2003
bmx03 = read.xport(file="NHANES/2003-4 BMX_C.XPT")
pam03 = read.sas7bdat(file="NHANES/pam_perperson_2003.sas7bdat")
#https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=2005
bmx05 = read.xport(file="NHANES/2005-6 BMX_D.XPT")
pam05 = read.sas7bdat(file="NHANES/pam_perperson_2005.sas7bdat")

#NCHS: outcome

#mortality
#https://www.cdc.gov/nchs/data-linkage/mortality-public.htm
#reproduction note: Sample SAS program from the above website used to preprocess the Public-use Linked Mortality Files and export as CSV
nchs03 = read.csv("NCHS/NHANES_2003_2004_MORT_2011_PUBLIC.csv", as.is=T, stringsAsFactors=F, na.strings="")
nchs05 = read.csv("NCHS/NHANES_2005_2006_MORT_2011_PUBLIC.csv", as.is=T, stringsAsFactors=F, na.strings="")


### MERGE and RECODE ###

#merge by survey year
nhanes_nchs03 = merge(merge(merge(merge(merge(merge(merge(merge(demo03, alq03, by="SEQN", all=T), smq03, by="SEQN", all=T), diq03, by="SEQN", all=T), mcq03, by="SEQN", all=T), pfq03, by="SEQN", all=T), bmx03, by="SEQN", all=T), pam03[,c("SEQN","allmean_mv1","allmean_mv5","allmean_mv10","allmean_cnt_wr")], by="SEQN", all=T), nchs03, by.x="SEQN", by.y="NHANES.Respondent.Sequence.Number", all=T)
nhanes_nchs05 = merge(merge(merge(merge(merge(merge(merge(merge(demo05, alq05, by="SEQN", all=T), smq05, by="SEQN", all=T), diq05, by="SEQN", all=T), mcq05, by="SEQN", all=T), pfq05, by="SEQN", all=T), bmx05, by="SEQN", all=T), pam05[,c("SEQN","allmean_mv1","allmean_mv5","allmean_mv10","allmean_cnt_wr")], by="SEQN", all=T), nchs05, by.x="SEQN", by.y="NHANES.Respondent.Sequence.Number", all=T)
rm(demo03,alq03,smq03,diq03,mcq03,pfq03,bmx03,pam03,nchs03,demo05,alq05,smq05,diq05,mcq05,pfq05,bmx05,pam05,nchs05)

#age: years
nhanes_nchs03$age = nhanes_nchs03$RIDAGEYR
nhanes_nchs05$age = nhanes_nchs05$RIDAGEYR

#recalculate survey weights based on recommendations from NHANES
#https://www.cdc.gov/nchs/tutorials/NHANES/SurveyDesign/Weighting/Task2.htm
nhanes_nchs03$svy.wt <- nhanes_nchs03$WTMEC2YR*0.5
nhanes_nchs05$svy.wt <- nhanes_nchs05$WTMEC2YR*0.5

#sex: 0=male, 1=female
#reproduction note: manuscript used term 'sex', but probably meant gender
nhanes_nchs03$sex = nhanes_nchs03$RIAGENDR - 1
nhanes_nchs05$sex = nhanes_nchs05$RIAGENDR - 1
# table(nhanes_nchs03$sex, useNA="always")
# table(nhanes_nchs05$sex, useNA="always")

#race-ethnicity: 0=non-Hispanic white, 1=non-Hispanic black, 2=Hispanic, 3=other
#reproduction note: unclear how Hispanic was defined; here collapsed Mexican American and Other Hispanic as Hispanic
nhanes_nchs03$race_ethnicity = ifelse(nhanes_nchs03$RIDRETH1==3, 0, ifelse(nhanes_nchs03$RIDRETH1==4, 1, ifelse(nhanes_nchs03$RIDRETH1==5, 3, 2)))
nhanes_nchs05$race_ethnicity = ifelse(nhanes_nchs05$RIDRETH1==3, 0, ifelse(nhanes_nchs05$RIDRETH1==4, 1, ifelse(nhanes_nchs05$RIDRETH1==5, 3, 2)))
# table(nhanes_nchs03$race_ethnicity, useNA="always")
# table(nhanes_nchs05$race_ethnicity, useNA="always")

#education: 0=less than high school, 1=high school diploma, 2=high school or more
#reproduction note: unclear how refused/don't know responses handled; here recoded as missing
nhanes_nchs03$education = ifelse(nhanes_nchs03$DMDEDUC2==1 | nhanes_nchs03$DMDEDUC2==2, 0, ifelse(nhanes_nchs03$DMDEDUC2==3, 1, ifelse(nhanes_nchs03$DMDEDUC2==4 | nhanes_nchs03$DMDEDUC2==5, 2, NA)))
nhanes_nchs05$education = ifelse(nhanes_nchs05$DMDEDUC2==1 | nhanes_nchs05$DMDEDUC2==2, 0, ifelse(nhanes_nchs05$DMDEDUC2==3, 1, ifelse(nhanes_nchs05$DMDEDUC2==4 | nhanes_nchs05$DMDEDUC2==5, 2, NA)))
# table(nhanes_nchs03$education, useNA="always")
# table(nhanes_nchs05$education, useNA="always")

#alcohol consumption: 0=never, 1=former, 2=current
#reproduction note: unclear how defined; here current defined as 12 drinks past year, former as no drinks past year but 12 drinks lifetime, never as no drinks to both questions
nhanes_nchs03$alcohol = ifelse(nhanes_nchs03$ALQ101==2 & nhanes_nchs03$ALQ110==2, 0, ifelse(nhanes_nchs03$ALQ101==2 & nhanes_nchs03$ALQ110==1, 1, ifelse(nhanes_nchs03$ALQ101==1, 2, NA)))
nhanes_nchs05$alcohol = ifelse(nhanes_nchs05$ALQ101==2 & nhanes_nchs05$ALQ110==2, 0, ifelse(nhanes_nchs05$ALQ101==2 & nhanes_nchs05$ALQ110==1, 1, ifelse(nhanes_nchs05$ALQ101==1, 2, NA)))
# table(nhanes_nchs03$alcohol, useNA="always")
# table(nhanes_nchs05$alcohol, useNA="always")

#smoking status, 0=never, 1=former, 2=current
#reproduction note: unclear how defined; here current defined as now smoking cigarettes (any frequency), former defined as not now smoking but smoked 100 in life, never as no 100 lifetime cigarettes
nhanes_nchs03$smoke = ifelse(nhanes_nchs03$SMQ020==2, 0, ifelse(nhanes_nchs03$SMQ020==1 & nhanes_nchs03$SMQ040==3, 1, ifelse(nhanes_nchs03$SMQ040==1 | nhanes_nchs03$SMQ040==2, 2, NA)))
nhanes_nchs05$smoke = ifelse(nhanes_nchs05$SMQ020==2, 0, ifelse(nhanes_nchs05$SMQ020==1 & nhanes_nchs05$SMQ040==3, 1, ifelse(nhanes_nchs05$SMQ040==1 | nhanes_nchs05$SMQ040==2, 2, NA)))
# table(nhanes_nchs03$smoke, useNA="always")
# table(nhanes_nchs05$smoke, useNA="always")

#body mass index (bmi): 0=<25, 1=25-29.9, 2=>=30 kg/m2
nhanes_nchs03$bmi = ifelse(nhanes_nchs03$BMXBMI<25, 0, ifelse(nhanes_nchs03$BMXBMI>=30, 2, 1))
nhanes_nchs05$bmi = ifelse(nhanes_nchs05$BMXBMI<25, 0, ifelse(nhanes_nchs05$BMXBMI>=30, 2, 1))
# table(nhanes_nchs03$bmi, useNA="always")
# table(nhanes_nchs05$bmi, useNA="always")

#diabetes: 0=no, 1=yes
#reproduction note: unclear how defined or handled refused/don't know; here only answer yes to question (not bordeline) and recoded as missing
nhanes_nchs03$diabetes = ifelse(nhanes_nchs03$DIQ010==2 | nhanes_nchs03$DIQ010==3, 0, ifelse(nhanes_nchs03$DIQ010==1, 1, NA))
nhanes_nchs05$diabetes = ifelse(nhanes_nchs05$DIQ010==2 | nhanes_nchs05$DIQ010==3, 0, ifelse(nhanes_nchs05$DIQ010==1, 1, NA))
# table(nhanes_nchs03$diabetes, useNA="always")
# table(nhanes_nchs05$diabetes, useNA="always")

#coronary artery disease (cad): 0=no, 1=yes
#reproduction note: unclear how handled refused/don't know; here only answer yes to question (not bordeline) and recoded as missing
nhanes_nchs03$cad = ifelse(nhanes_nchs03$MCQ160C==2, 0, ifelse(nhanes_nchs03$MCQ160C==1, 1, NA))
nhanes_nchs05$cad = ifelse(nhanes_nchs05$MCQ160C==2, 0, ifelse(nhanes_nchs05$MCQ160C==1, 1, NA))
# table(nhanes_nchs03$cad, useNA="always")
# table(nhanes_nchs05$cad, useNA="always")

#stroke: 0=no, 1=yes
#reproduction note: unclear how handled refused/don't know; here only answer yes to question (not bordeline) and recoded as missing
nhanes_nchs03$stroke = ifelse(nhanes_nchs03$MCQ160F==2, 0, ifelse(nhanes_nchs03$MCQ160F==1, 1, NA))
nhanes_nchs05$stroke = ifelse(nhanes_nchs05$MCQ160F==2, 0, ifelse(nhanes_nchs05$MCQ160F==1, 1, NA))
# table(nhanes_nchs03$stroke, useNA="always")
# table(nhanes_nchs05$stroke, useNA="always")

#cancer: 0=no, 1=yes
#reproduction note: unclear how handled refused/don't know; here only answer yes to question (not bordeline) and recoded as missing
nhanes_nchs03$cancer = ifelse(nhanes_nchs03$MCQ220==2, 0, ifelse(nhanes_nchs03$MCQ220==1, 1, NA))
nhanes_nchs05$cancer = ifelse(nhanes_nchs05$MCQ220==2, 0, ifelse(nhanes_nchs05$MCQ220==1, 1, NA))
# table(nhanes_nchs03$cancer, useNA="always")
# table(nhanes_nchs05$cancer, useNA="always")

#mobility limitation: 0=no, 1=yes
#reproduction note: unclear how defined or handled refused/don't know/do not do this activity; here defined according to previous publication by one of the coauthors (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5081718/) as some/much/unable for walking quarter mile or up ten steps and recoded as missing
nhanes_nchs03$mobility = ifelse((is.na(nhanes_nchs03$PFQ061B) | nhanes_nchs03$PFQ061B==1) & (is.na(nhanes_nchs03$PFQ061C) | nhanes_nchs03$PFQ061C==1), 0, ifelse(nhanes_nchs03$PFQ061B==2 | nhanes_nchs03$PFQ061B==3 | nhanes_nchs03$PFQ061B==4 | nhanes_nchs03$PFQ061B==5 | nhanes_nchs03$PFQ061C==2 | nhanes_nchs03$PFQ061C==3 | nhanes_nchs03$PFQ061C==4 | nhanes_nchs03$PFQ061C==5, 1, NA))
nhanes_nchs05$mobility = ifelse((is.na(nhanes_nchs05$PFQ061B) | nhanes_nchs05$PFQ061B==1) & (is.na(nhanes_nchs05$PFQ061C) | nhanes_nchs05$PFQ061C==1), 0, ifelse(nhanes_nchs05$PFQ061B==2 | nhanes_nchs05$PFQ061B==3 | nhanes_nchs05$PFQ061B==4 | nhanes_nchs05$PFQ061B==5 | nhanes_nchs05$PFQ061C==2 | nhanes_nchs05$PFQ061C==3 | nhanes_nchs05$PFQ061C==4 | nhanes_nchs05$PFQ061C==5, 1, NA))

#reproduction note: this operationalization of mobility incorporates skip patterns and stricter definition; included to test differences in risk based on the variable specification
nhanes_nchs03$mobility1 = ifelse((nhanes_nchs03$PFQ049==2 & nhanes_nchs03$PFQ057==2 & nhanes_nchs03$PFQ059==2 & nhanes_nchs03$RIDAGEYR<=59) | (nhanes_nchs03$PFQ061B==1 & nhanes_nchs03$PFQ061C==1), 0, ifelse(nhanes_nchs03$PFQ061B==2 | nhanes_nchs03$PFQ061B==3 | nhanes_nchs03$PFQ061B==4 | nhanes_nchs03$PFQ061B==5 | nhanes_nchs03$PFQ061C==2 | nhanes_nchs03$PFQ061C==3 | nhanes_nchs03$PFQ061C==4 | nhanes_nchs03$PFQ061C==5, 1, NA))
nhanes_nchs05$mobility1 = ifelse((nhanes_nchs05$PFQ049==2 & nhanes_nchs05$PFQ057==2 & nhanes_nchs05$PFQ059==2 & nhanes_nchs05$RIDAGEYR<=59) | (nhanes_nchs05$PFQ061B==1 & nhanes_nchs05$PFQ061C==1), 0, ifelse(nhanes_nchs05$PFQ061B==2 | nhanes_nchs05$PFQ061B==3 | nhanes_nchs05$PFQ061B==4 | nhanes_nchs05$PFQ061B==5 | nhanes_nchs05$PFQ061C==2 | nhanes_nchs05$PFQ061C==3 | nhanes_nchs05$PFQ061C==4 | nhanes_nchs05$PFQ061C==5, 1, NA))
# table(nhanes_nchs03$mobility, useNA="always")
# table(nhanes_nchs05$mobility, useNA="always")

#moderate-vigorous physical activity (mvpa): total, >=5 minute, and >=10 minute bouts
#reproduction note: see SAS scripts for processing of raw data to perperson datasets; these were modified from the original scripts available here: https://epi.grants.cancer.gov/nhanes_pam/
nhanes_nchs03$mvpa_total = ifelse(is.na(nhanes_nchs03$allmean_mv1), NA, nhanes_nchs03$allmean_mv1)
nhanes_nchs03$mvpa_5min = ifelse(is.na(nhanes_nchs03$allmean_mv5), NA, nhanes_nchs03$allmean_mv5)
nhanes_nchs03$mvpa_10min = ifelse(is.na(nhanes_nchs03$allmean_mv10), NA, nhanes_nchs03$allmean_mv10)
nhanes_nchs05$mvpa_total = ifelse(is.na(nhanes_nchs05$allmean_mv1), NA, nhanes_nchs05$allmean_mv1)
nhanes_nchs05$mvpa_5min = ifelse(is.na(nhanes_nchs05$allmean_mv5), NA, nhanes_nchs05$allmean_mv5)
nhanes_nchs05$mvpa_10min = ifelse(is.na(nhanes_nchs05$allmean_mv10), NA, nhanes_nchs05$allmean_mv10)
# summary(nhanes_nchs03$mvpa_total)
# summary(nhanes_nchs03$mvpa_5min)
# summary(nhanes_nchs03$mvpa_10min)
# summary(nhanes_nchs05$mvpa_total)
# summary(nhanes_nchs05$mvpa_5min)
# summary(nhanes_nchs05$mvpa_10min)

#activity count (ac)
#reproduction note: see SAS scripts for processing of raw data to perperson datasets
nhanes_nchs03$ac = ifelse(is.na(nhanes_nchs03$allmean_cnt_wr), NA, nhanes_nchs03$allmean_cnt_wr)
nhanes_nchs05$ac = ifelse(is.na(nhanes_nchs05$allmean_cnt_wr), NA, nhanes_nchs05$allmean_cnt_wr)
# summary(nhanes_nchs03$ac)
# summary(nhanes_nchs05$ac)

#mortality: 0=no, 1=yes
nhanes_nchs03$mortality = ifelse(nhanes_nchs03$Final.Mortality.Status=="Assumed alive", 0, ifelse(nhanes_nchs03$Final.Mortality.Status=="Assumed deceased", 1, NA))
nhanes_nchs05$mortality = ifelse(nhanes_nchs05$Final.Mortality.Status=="Assumed alive", 0, ifelse(nhanes_nchs05$Final.Mortality.Status=="Assumed deceased", 1, NA))
# table(nhanes_nchs03$mortality, useNA="always")
# table(nhanes_nchs05$mortality, useNA="always")

#calculation of follow-up from interview date and MEC Exam date.
nhanes_nchs03$PYFU.interview <- nhanes_nchs03$Person.Months.of.Follow.up.from.Interview.Date/12
nhanes_nchs05$PYFU.interview <- nhanes_nchs05$Person.Months.of.Follow.up.from.Interview.Date/12
nhanes_nchs03$PYFU.exam <- nhanes_nchs03$Person.Months.of.Follow.up.from.MEC.Exam.Date/12
nhanes_nchs05$PYFU.exam <- nhanes_nchs05$Person.Months.of.Follow.up.from.MEC.Exam.Date/12
# summary(nhanes_nchs03$PYFU.exam)
# summary(nhanes_nchs03$PYFU.interview)
# summary(nhanes_nchs05$PYFU.exam)
# summary(nhanes_nchs05$PYFU.interview)

#join survey years into a single dataset
nhanes_nchs03$NHANES = "2003-4"
nhanes_nchs05$NHANES = "2005-6"
nhanes_nchs = rbind(nhanes_nchs03[,c("SEQN","WTMEC2YR","svy.wt","SDMVPSU","SDMVSTRA","NHANES","age","sex","race_ethnicity","education","alcohol","smoke","bmi","diabetes","cad","stroke","cancer","mobility","mvpa_total","mvpa_5min","mvpa_10min","ac","mortality","PYFU.exam","PYFU.interview")], 
                    nhanes_nchs05[,c("SEQN","WTMEC2YR","svy.wt","SDMVPSU","SDMVSTRA","NHANES","age","sex","race_ethnicity","education","alcohol","smoke","bmi","diabetes","cad","stroke","cancer","mobility","mvpa_total","mvpa_5min","mvpa_10min","ac","mortality","PYFU.exam","PYFU.interview")])
rm(nhanes_nchs03,nhanes_nchs05)

#subset to adults >=40 years of age
#reproduction note: matches n=6355 eligible adults as reported here: Matthews CE, Keadle SK, Troiano RP, Kahle L, Koster A, Brychta R, Van Domelen D, Caserotti P, Chen KY, Harris TB, Berrigan D. Accelerometer measured dose-response for physical activity, sedentary time, and mortality in US adults. Am J Clin Nutr. 2016;104:1424-1432.
nhanes_nchs = nhanes_nchs[nhanes_nchs$age>=40, ]

#subset to only complete data on exposure, outcome, and covariates
#reproduction note: manuscript reported n=4840, discrepancy is largely driving by mobility limitation variable
#running code below brings dataset to N=4632
nhanes_nchs = nhanes_nchs[complete.cases(nhanes_nchs), ]


### OPERATIONALIZE EXPOSURE CATEGORIES ###

#quartiles of total mvpa
nhanes_nchs$mvpa_total_quartile = as.integer(cut(nhanes_nchs$mvpa_total, quantile(nhanes_nchs$mvpa_total, probs=seq(0,1,1/4), na.rm=T), include.lowest=TRUE))

#bout ratio based on proportion of 5 min bouts to total bouts
nhanes_nchs$mvpa_bout_ratio = nhanes_nchs$mvpa_5min/nhanes_nchs$mvpa_total*100

#create tertiles for bout ratios within each strata of total mvpa quartiles
nhanes_nchs$mvpa_q1_tertile[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==1] = as.integer(cut(nhanes_nchs$mvpa_bout_ratio[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==1], quantile(nhanes_nchs$mvpa_bout_ratio[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==1], probs=seq(0,1,1/3), na.rm=T), include.lowest=T))
nhanes_nchs$mvpa_q2_tertile[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==2] = as.integer(cut(nhanes_nchs$mvpa_bout_ratio[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==2], quantile(nhanes_nchs$mvpa_bout_ratio[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==2], probs=seq(0,1,1/3), na.rm=T), include.lowest=T))
nhanes_nchs$mvpa_q3_tertile[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==3] = as.integer(cut(nhanes_nchs$mvpa_bout_ratio[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==3], quantile(nhanes_nchs$mvpa_bout_ratio[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==3], probs=seq(0,1,1/3), na.rm=T), include.lowest=T))
nhanes_nchs$mvpa_q4_tertile[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==4] = as.integer(cut(nhanes_nchs$mvpa_bout_ratio[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==4], quantile(nhanes_nchs$mvpa_bout_ratio[!is.na(nhanes_nchs$mvpa_total_quartile) & nhanes_nchs$mvpa_total_quartile==4], probs=seq(0,1,1/3), na.rm=T), include.lowest=T))

#create twelve dichotomous exposure indicators
nhanes_nchs$exposure_q1t1 = ifelse(is.na(nhanes_nchs$mvpa_q1_tertile), 0, ifelse(nhanes_nchs$mvpa_q1_tertile==1, 1, 0))
nhanes_nchs$exposure_q1t2 = ifelse(is.na(nhanes_nchs$mvpa_q1_tertile), 0, ifelse(nhanes_nchs$mvpa_q1_tertile==2, 1, 0))
nhanes_nchs$exposure_q1t3 = ifelse(is.na(nhanes_nchs$mvpa_q1_tertile), 0, ifelse(nhanes_nchs$mvpa_q1_tertile==3, 1, 0))
nhanes_nchs$exposure_q2t1 = ifelse(is.na(nhanes_nchs$mvpa_q2_tertile), 0, ifelse(nhanes_nchs$mvpa_q2_tertile==1, 1, 0))
nhanes_nchs$exposure_q2t2 = ifelse(is.na(nhanes_nchs$mvpa_q2_tertile), 0, ifelse(nhanes_nchs$mvpa_q2_tertile==2, 1, 0))
nhanes_nchs$exposure_q2t3 = ifelse(is.na(nhanes_nchs$mvpa_q2_tertile), 0, ifelse(nhanes_nchs$mvpa_q2_tertile==3, 1, 0))
nhanes_nchs$exposure_q3t1 = ifelse(is.na(nhanes_nchs$mvpa_q3_tertile), 0, ifelse(nhanes_nchs$mvpa_q3_tertile==1, 1, 0))
nhanes_nchs$exposure_q3t2 = ifelse(is.na(nhanes_nchs$mvpa_q3_tertile), 0, ifelse(nhanes_nchs$mvpa_q3_tertile==2, 1, 0))
nhanes_nchs$exposure_q3t3 = ifelse(is.na(nhanes_nchs$mvpa_q3_tertile), 0, ifelse(nhanes_nchs$mvpa_q3_tertile==3, 1, 0))
nhanes_nchs$exposure_q4t1 = ifelse(is.na(nhanes_nchs$mvpa_q4_tertile), 0, ifelse(nhanes_nchs$mvpa_q4_tertile==1, 1, 0))
nhanes_nchs$exposure_q4t2 = ifelse(is.na(nhanes_nchs$mvpa_q4_tertile), 0, ifelse(nhanes_nchs$mvpa_q4_tertile==2, 1, 0))
nhanes_nchs$exposure_q4t3 = ifelse(is.na(nhanes_nchs$mvpa_q4_tertile), 0, ifelse(nhanes_nchs$mvpa_q4_tertile==3, 1, 0))

#create categorical joint exposure
nhanes_nchs$exposure = ifelse(nhanes_nchs$exposure_q1t1==1, 0, ifelse(nhanes_nchs$exposure_q2t1==1, 1, ifelse(nhanes_nchs$exposure_q3t1==1, 2, ifelse(nhanes_nchs$exposure_q4t1==1, 3, ifelse(nhanes_nchs$exposure_q1t2==1, 4, ifelse(nhanes_nchs$exposure_q2t2==1, 5, ifelse(nhanes_nchs$exposure_q3t2==1, 6, ifelse(nhanes_nchs$exposure_q4t2==1, 7, ifelse(nhanes_nchs$exposure_q1t3==1, 8, ifelse(nhanes_nchs$exposure_q2t3==1, 9, ifelse(nhanes_nchs$exposure_q3t3==1, 10, ifelse(nhanes_nchs$exposure_q4t3==1, 11, NA))))))))))))
# summary(nhanes_nchs$exposure)

#checks for min/max
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q1_tertile==1])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q1_tertile==2])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q1_tertile==3])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q2_tertile==1])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q2_tertile==2])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q2_tertile==3])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q3_tertile==1])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q3_tertile==2])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q3_tertile==3])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q4_tertile==1])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q4_tertile==2])
# summary(nhanes_nchs$mvpa_bout_ratio[nhanes_nchs$mvpa_q4_tertile==3])


#########
## END ##
#########


### RESULTS ###

#follow-up
sum(nhanes_nchs$PYFU.interview)/nrow(nhanes_nchs)

#total mortality
sum(nhanes_nchs$mortality)

#characteristics, taking into account complex survey design
nhanes_nchs_complex = svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~svy.wt, nest=T, data=nhanes_nchs)
svymean(~factor(sex), nhanes_nchs_complex)
svymean(~factor(race_ethnicity), nhanes_nchs_complex)
svymean(~factor(education), nhanes_nchs_complex)
svymean(~factor(smoke), nhanes_nchs_complex)
svymean(~factor(alcohol), nhanes_nchs_complex)

# fitting coxph models, taking into account complex survey design
result_table = svycoxph(Surv(PYFU.exam, mortality) ~ as.factor(exposure) + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(education) + as.factor(alcohol) + as.factor(smoke) + as.factor(bmi) + as.factor(diabetes) + as.factor(cad) + as.factor(stroke) + as.factor(cancer) + as.factor(mobility) , design=nhanes_nchs_complex)
summary(result_table)

