*------------------------------------------------------------------------------------------*;
*create.pam_perperson.sas                                                                  *;
*                                                                                          *;
*Purpose: summarize valid PAM data into one record per person, add demographic variables,  *;
*         and output to a SAS transport data set.                                          *;
* A valid day is a day in which the subject wore the monitor for 10+ hours.                *;
* A valid person is a person with 4+ valid days.                                           *;
* Please see comments below on how to change the definitions of valid day and valid person.*;
*                                                                                          *;
*Before running the code below:                                                            *;
*1. Modify the libname statement to refer to the folder where you want to store the input  *;
*   and output datasets.                                                                   *;
*2. Create a SAS dataset named DEMO_C in that folder from the Demographics data from       *;
*   http://www.cdc.gov/nchs/about/major/nhanes/nhanes2003-2004/demo03_04.htm.              *;
*3. Run create.pam_perday.sas to create the dataset pam_perday, and save it in that folder.*;
*4. Save PAM_formats.txt (included with these programs) and list the full path in the      *;
*   %include statement below.  You will need to include these formats in any program that  *;
*   uses the output dataset.                                                               *;
*------------------------------------------------------------------------------------------*;
title 'PAM Processing ';

libname myfolder "W:\goldstein\2003";
%include "W:\goldstein\original-code\PAM_formats.sas";

libname demo xport "W:\goldstein\2003\demo_c.xpt";

*----------------------------------------------------------------------*;
*Define a valid day and a valid person.                                *;
*NOTE: to change the definitions on the number of wear hours(wear_hr)  *;
*      required for a valid day, or the number of valid days(valdays)  *;
*      required for a valid person, please modify the statements below.*;
*----------------------------------------------------------------------*;
data myfolder.pam_day;
  set myfolder.pam_perday_2003;
  valid_day=(wear_hr>=10); /*change valid day hours criterion here*/
  format valid_day yesno.;
  label valid_day='10+ hours of wear (yes/no)';
run;

proc summary data=myfolder.pam_day;
  by seqn;
  var seqn;
  where valid_day=1;
  output out=myfolder.valid
         n=valdays;     /*number of days with 10+ hours of wear*/
run;

data myfolder.pam_day;
  merge myfolder.pam_day(in=inall) myfolder.valid;
  by seqn;
  if inall;

  if valdays=. then valdays=0;
  label valdays='Number of days with 10+ hours of wear';

  valid_person=(valdays>=1);  /*change valid person days criterion here*/
  format valid_person yesno.;
  label valid_person = 'At least 1 day with 10+ hours of wear (yes/no)';

  drop _freq_ _type_;

run;

*-------------------------------------------------------------------------------------------*;
*Summarize for valid people(4+ valid days), using only their valid days(10+ hrs of wear).   *;
*-------------------------------------------------------------------------------------------*;
proc summary data=myfolder.pam_day;
  by seqn;
  where valid_person=1 and valid_day=1;
  var tot_dur_mv1 tot_dur5_mv tot_dur10_mv
      tot_dur_m1 tot_dur5_m tot_dur10_m
      tot_dur_v1 tot_dur5_v tot_dur10_v
      tot_min_wr tot_cnt_wr;
  output out=myfolder.valid_days
  mean(tot_dur_mv1 tot_dur5_mv tot_dur10_mv
      tot_dur_m1 tot_dur5_m tot_dur10_m
      tot_dur_v1 tot_dur5_v tot_dur10_v
      tot_min_wr)=
      allmean_mv1 allmean_mv5 allmean_mv10  /*mean duration of mod./vig. activity bouts*/
      allmean_m1 allmean_m5 allmean_m10    /*mean duration of mod. activity bouts*/
      allmean_v1 allmean_v5 allmean_v10    /*mean duration of vig. activity bouts*/
      allmean_min_wr                       /*mean wear time(minutes)*/
  sum(tot_min_wr tot_cnt_wr)=all_min_wr all_cnt_wr; /*total wear minutes and intensity counts*/
run;

data myfolder.valid_days;
  set myfolder.valid_days;
  allmean_cnt_wr=all_cnt_wr/all_min_wr; /*mean intensity counts per minute*/
  allmean_hr_wr=allmean_min_wr/60;      /*mean wear time(hr)*/
  label
  allmean_cnt_wr='Mean intensity count per minute on wear periods from all valid days'
  allmean_hr_wr='Mean wear time(hr) per day from all valid days'
  allmean_mv1='Mean duration (minutes) of moderate and vigorous activity bouts (minimum 1 minute bouts) per day from all valid days'
  allmean_m1='Mean duration (minutes) of moderate activity bouts (minimum 1 minute bouts) per day from all valid days'
  allmean_v1='Mean duration (minutes) of vigorous activity bouts (minimum 1 minute bouts) per day from all valid days'
  allmean_mv5='Mean duration (minutes) of moderate and vigorous activity bouts (minimum 5 minute bouts) per day from all valid days'
  allmean_m5='Mean duration (minutes) of moderate activity bouts (minimum 5 minute bouts) per day from all valid days'
  allmean_v5='Mean duration (minutes) of vigorous activity bouts (minimum 5 minute bouts) per day from all valid days'
  allmean_mv10='Mean duration (minutes) of moderate and vigorous activity bouts (minimum 10 minute bouts) per day from all valid days'
  allmean_m10='Mean duration (minutes) of moderate activity bouts (minimum 10 minute bouts) per day from all valid days'
  allmean_v10='Mean duration (minutes) of vigorous activity bouts (minimum 10 minute bouts) per day from all valid days'
;

  drop _type_  _freq_ ;
run;

/*everyone in the analysis*/
proc sort nodupkey data=myfolder.pam_day out=myfolder.pam_all;
  by seqn;
run;

/*merge with the valid day data and demographic data for final data set*/
data myfolder.pam_perperson;
  merge myfolder.pam_all(in=in_pam keep=seqn valid_person valdays) demo.demo_c(in=in_demog) myfolder.valid_days;* changed by kwd;
  by seqn;
  if in_pam;
  if not in_demog then put 'error: not in demog' seqn=;

  /*Age groups*/
  if       6<=ridageyr<=11 then agegrp=1;
  else if 12<=ridageyr<=15 then agegrp=2;
  else if 16<=ridageyr<=19 then agegrp=3;
  else if 20<=ridageyr<=29 then agegrp=4;
  else if 30<=ridageyr<=39 then agegrp=5;
  else if 40<=ridageyr<=49 then agegrp=6;
  else if 50<=ridageyr<=59 then agegrp=7;
  else if 60<=ridageyr<=69 then agegrp=8;
  else if ridageyr>=70     then agegrp=9;
  format agegrp agegrp.;
  label agegrp='Age group' ;

  /*Gender*/
  format riagendr gender.;
  label riagendr='Gender';

  keep seqn valid_person valdays riagendr agegrp ridageyr sdmvstra sdmvpsu wtmec2yr
       allmean_mv1 allmean_mv5 allmean_mv10 
       allmean_m1 allmean_m5 allmean_m10 
       allmean_v1 allmean_v5 allmean_v10 
       allmean_cnt_wr allmean_hr_wr;
run;

data myfolder.pam_perperson_2003;
  set myfolder.pam_perperson;
run;

proc contents data=myfolder.pam_perperson_2003;
run;

*-------------------------------------------------------------------------*;
*You have now created the dataset with one record per person. It may be   *;
*used in person level analysis for the PAM data.                          *;
*-------------------------------------------------------------------------*;

data maurice;
set myfolder.pam_perperson_2003;
where RIDAGEYR>=40;
run;

proc means data=maurice
  n mean stderr min p25 p50 p75 max;
var allmean_mv1 allmean_mv5 allmean_mv10;
run;

