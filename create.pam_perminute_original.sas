*----------------------------------------------------------------------------*;
*create.pam_perminute.sas                                                    *;
*                                                                            *;
*This program edits the Physical Activity Monitor (PAM) data. It edits       *;
*invalid and unreliable intensity values. It creates a SAS dataset named     *;
*pam_perminute in the location specified by the "libname myfolder" statement *;
*below.                                                                      *;
*                                                                            *;
*Before running the code below:                                              *;
*1. Modify the libname statement to refer to the folder where you want to    *;
*   store the input and output datasets.                                     *;
*2. Create a SAS dataset named paxraw_c in that folder from the Physical     *;
*   Activity Monitor data from                                               *;
*   http://www.cdc.gov/nchs/about/major/nhanes/nhanes2003-2004/exam03_04.htm.*;
*3. Save PAM_formats.txt (included with these programs) and list the full    *;
*   path in the %include statement below.  You will need to include these    *;
*   formats in any program that uses the output dataset.                     *;                                        
*----------------------------------------------------------------------------*;
libname myfolder "&home/EATS_NHANES/sasdata";
%include "&home/EATS_NHANES/sasprog/AccelerometryPA/PAM_formats.sas";

*----------------------------------------------------------------------------*;
*Create the sequential day variable DAY. (Note that DAY is different from the*;
*day of the week variable PAXDAY.)                                           *;
*----------------------------------------------------------------------------*;
data paxraw;
  set myfolder.paxraw_c;
  day=ceil(paxn/1440);
  label day='Sequential Day';
  format paxday wkday.;
run;

*----------------------------------------------------------------------*;
*1. edit records with intensity count=32767(invalid monitor reading)   *;
*2. assign missing values for other unreliable data points             *;
*----------------------------------------------------------------------*;
* identify people with fewer than 6 data points;
* with intensity count = 32767 (invalid monitor reading);
* These data will be replaced with the average of the adjacent minutes that have counts that are not = 32767;
data invalid valid;
  set paxraw;
  by seqn;
  if first.seqn then invalid_cnt=0;
  retain invalid_cnt;
  if paxinten=32767 then invalid_cnt=invalid_cnt+1;
  if last.seqn then do; 
    if 0<invalid_cnt<6 then output invalid;
    else output valid;
  end;
run;

*get all monitor data for people with invalid intensity counts;
data paxraw_invalid;
  merge invalid(in=in_invalid) paxraw;
  by seqn;
  if in_invalid;
run;

proc sort data=paxraw_invalid;
  by seqn day paxn;
run;

*create new values for records with intensity count = 32767;
data paxraw_new(keep=seqn day paxinten paxn);
  set paxraw_invalid;
  by seqn day paxn;
*paxn_invalid = first minute with an intensity count = 32767;
*paxn_valid = first minute with a valid intensity count (ne 32767);
*last_int = last minute with a valid intensity count;
  retain paxn_invalid 
         paxn_valid   
         last_int;    

  *save the first minute with intensity count = 32767;
  if paxinten = 32767 and paxn_invalid=. and not last.day then
    paxn_invalid=paxn;

  *when one minute or >1 minutes with intensity count(s) = 32767, take the average of;
  *the valid intensity counts immediately before and after the invalid minute(s);
  else if paxinten > . and paxinten ne 32767 then do;
    if paxn_invalid ne . then do;
      sv_int=paxinten;
      sv_paxn=paxn;
     *one or more consecutive minutes with count=32767;
      do i=paxn_valid+1 to paxn-1; 
        paxn=i;
        paxinten=round(sum(sv_int,last_int)/2);
        output;
      end;
      paxn_invalid=.;
      *last minute with a valid intensity;
      last_int=sv_int;     
      paxn_valid=sv_paxn;
    end;
    else do;
      paxn_valid=paxn;
      last_int=paxinten;
    end; 
  end;
  *if the last minute of the day has intensity count = 32767;
  *use the last valid minute;
  else if paxinten=32767 and last.day then do;
    paxinten=last_int;
    if paxn_invalid ne . then do;
      do i=paxn_invalid to paxn; * >=1 consecutive minutes of count=32767 up to the last minute;
        paxn=i;
        output;
      end;
    end;
    else output;
  end;
run;

proc sort data=paxraw_new;
   by seqn day paxn ;
run;

proc sort data=paxraw;
   by seqn day paxn ;
run;

 *Update the raw data with the modified values above and assign;
 *missing values for other unreliable data points. Records noted
 *by SEQN below were identified as having partial or completely;
 *anomalous data. Anomalous data are set to missing to preserve;
 *remaining data. Alternatively, users may exclude all data with;
 *PAXSTAT=2;
data pam_perminute;
  update  paxraw paxraw_new;
  by seqn day paxn ;

  if (seqn = 29585 and paxn>1400 )  or
     (seqn = 23953 and paxn>=1196 ) or
     seqn = 23163 or
     (seqn = 27791 and paxn<6000) or
     (seqn = 26976 and paxinten=32767) or
     (seqn = 24719 and paxinten = 32767) or
     (seqn = 26984 and (8572<=paxn<=8613 or 8634<=paxn<=9556)) or
     (seqn in ( 22577, 28310, 21901, 29814) and paxinten = 32767) or
     (seqn = 22194) or
     (seqn = 25262 and paxn<=939)  or
     seqn in (22732, 23111, 27441, 21347) or
     (seqn = 21310 and paxn> 1728) or
     (seqn = 23443 and paxn < 7200) or
     (seqn = 29229 and paxn > 5472) or
     (seqn = 24515 and paxn > 6048) or
     (seqn = 27900 and paxn > 7920) or
     (seqn = 28107 and paxn < 3312) or
     (seqn = 27434 and paxn < 3024) or
     (seqn = 25319 and paxn > 8496) or
     (seqn = 25318 and paxn < 1872) or
     (seqn = 22213 and 4500<=paxn<=4700) or
     (seqn = 24845 and 8150<=paxn<=9580) or
     (seqn = 30908 and 1<=paxn<=320) or
     (seqn = 23561 and 1800<=paxn<=3800) then paxinten=.;
run;

*copy the work dataset pam_perminute to the folder referenced by the libname statement above;
data myfolder.pam_perminute;
  set pam_perminute;
run;

proc contents data=myfolder.pam_perminute;
run;

*-------------------------------------------------------------------------*;
*You have now created a dataset that has had two kinds of problem data    *;
*removed (invalid and unreliable intensity counts).  It contains the      *;
*variables listed in pam_perminute_contents.doc.  It is used in           *;
*create.pam_perday.sas to create a dataset with one record per person     *;
*per day, and it may also be used for other analyses.                     *;
*-------------------------------------------------------------------------*;
