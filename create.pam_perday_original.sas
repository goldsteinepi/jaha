*--------------------------------------------------------------------------------------------------------*;
*create.pam_perday.sas                                                                                   *;
*                                                                                                        *;
*Purpose: summarize valid PAM data into one record per person per day. The summary record contains       *;
*         derived variables on duration of non-wear periods as well as activity bouts with moderate,     *;
*         vigorous, and moderate or vigorous intensity.                                                  *;
*                                                                                                        *;
*Before running the code below:                                                                          *;
*1. Modify the libname statement to refer to the folder where you want to store the input and output     *;
*   datasets.                                                                                            *;
*2. Save PAM_formats.txt (included with these programs) and list the full path in the                    *;
*   %include statement below.  You will need to include these formats in any program that                *;
*   uses the output dataset.                                                                             *;
*3. Run create.pam_perminute.sas to create the input dataset pam_perminute.                              *;
*4. Create a SAS dataset named demo_c in that folder from the Demographic data from                      *;
*   http://www.cdc.gov/nchs/about/major/nhanes/nhanes2003-2004/demo03_04.htm.                            *;
*--------------------------------------------------------------------------------------------------------*;
libname myfolder "&home/EATS_NHANES/sasdata";
libname demo xport "&home/EATS_NHANES/sasdata/demo_c.xpt";* Added by kwd;
%include "&home/EATS_NHANES/sasprog/AccelerometryPA/PAM_formats.sas";

*---------------------------------------------------------------------*;
*Read in the edited PAM dataset created by create.pam_perminute.sas.  *;
*Get age variable from the demog file--age is needed later to create  *;
*age-dependent physical activity variables. Only keep people with     *;
*PAXCAL=1 (the monitor was in calibration). This excludes 346 people. *;
*---------------------------------------------------------------------*;
data monitors;
*  merge myfolder.pam_perminute(in=in_pam) myfolder.demo_c(in=in_demog keep=seqn ridageyr);
  merge myfolder.pam_perminute(in=in_pam) demo.demo_c(in=in_demog keep=seqn ridageyr);* Added by kwd;
  by seqn;
  if in_pam;
  if not in_demog then put 'ERROR! not in demog' seqn=;
  if paxcal=1;
run;

*--------------------------------------------------------------------------------------------------------*;
*Macro %nw defines the duration of non-wear periods as well as wear periods within a day based on user   *;
*defined minimum length of non-wear period.                                                              *;
*                                                                                                        *;
*A non-wear period starts at a minute with the intensity count of zero. Minutes with intensity count=0 or*;
*up to 2 consecutive minutes with intensity counts between 1 and 100 are considered to be valid non-wear *;
*minutes. A non-wear period is established when the specified length of consecutive non-wear minutes is  *;
*reached. The non-wear period stops when any of the following conditions is met:                         *;
*  - one minute with intensity count >100                                                                *;
*  - one minute with a missing intensity count                                                           *;
*  - 3 consecutive minutes with intensity counts between 1 and 100                                       *;
*  - the last minute of the day                                                                          *;
*                                                                                                        *;
*Macro call %nw(nwperiod=);                                                                              *;
*nwperiod: minimum length for the non-wear period, must be >1 minute.                                    *;
*--------------------------------------------------------------------------------------------------------*;
%macro nw(nwperiod=);
data nw_all;
  set monitors;
  by seqn day paxn;

  if first.day then nw_num=0;    /*non-wear period number*/

  if first.day or reset or stopped then do;
     strt_nw=0;      /*starting minute for the non-wear period*/
     end_nw=0;       /*ending minute for the non-wear period*/
     start=0;        /*indicator for starting to count the non-wear period*/
     dur_nw=0;       /*duration for the non-wear period*/
     reset=0;        /*indicator for resetting and starting over*/
     stopped=0;      /*indicator for stopping the non-wear period*/
     cnt_non_zero=0; /*counter for the number of minutes with intensity between 1 and 100*/
  end;
  retain nw_num strt_nw end_nw stopped reset start cnt_non_zero dur_nw;

  /*The non-wear period starts with a zero count*/
  if paxinten=0 and start=0 then do;
        strt_nw=paxn;    /*assign the starting minute of non-wear*/
        start=1;
  end;

  /*accumulate the number of the non-wear minutes*/
  if start and paxinten=0 then
     end_nw=paxn;         /*keep track of the ending minute for the non-wear period*/

  /*keep track of the number of minutes with intensity between 1-100*/
  if 0<paxinten<=100 then
     cnt_non_zero=cnt_non_zero+1;

  /*before reaching the 3 consecutive minutes of 1-100 intensity, if encounter one minute with zero intensity, reset the counter*/
  if paxinten=0 then cnt_non_zero=0;

  /*duration of non-wear period*/
  dur_nw=end_nw-strt_nw+1;

  /*A non-wear period ends with 3 consecutive minutes of 1-100 intensity, one missing count, or one minute with >100 intensity*/
  if (cnt_non_zero=3 or paxinten=. or paxinten>100 ) then do;
    if dur_nw<&nwperiod then reset=1;       /*reset if less than &nwperiod minutes of non-wear*/
    else stopped=1;
  end;

  /*last minute of the day*/
  if last.day and dur_nw>=&nwperiod then stopped=1;

  /*output one record for each non-wear period*/
  if stopped=1 then do;
       nw_num=nw_num+1;
       keep seqn day nw_num strt_nw end_nw dur_nw;
       output;
  end;
run;

*---------------------------------------------------------------------*;
*summarize the non-wear periods to one record per day                 *;
*---------------------------------------------------------------------*;
proc summary data=nw_all ;
  by seqn day;
  var dur_nw ;
  output out=sum_nw
       sum=tot_dur_nw
run;

*-------------------------------------------------------------------------*;
*summarize the total number of valid minutes for everyone in the analysis.*;
*-------------------------------------------------------------------------*;
proc summary data=monitors;
  by seqn day ridageyr paxday;
  var paxinten;
  output out=sum_all
         n=tot_min;
run;

*---------------------------------------------------------------------*;
*define hours of wear                                                 *;
*---------------------------------------------------------------------*;
/*create a dataset with one record per minute, for the non-wear periods only*/
data nw_minutes(keep=seqn day paxn);
  set nw_all;
  by seqn day nw_num;
  do i=strt_nw to end_nw by 1;
     paxn=i;
     output;
  end;
run;

/*create a dataset from the monitor data, restricted to the wear periods*/
data wear_minute(keep=seqn day paxn paxinten);
  merge monitors(in=in_all) nw_minutes(in=in_nw);
  by seqn day paxn;
  if in_all and not in_nw;
run;

/*summarize the wear minutes */
proc summary data=wear_minute;
  by seqn day;
  var paxinten;
  output out=sum_wear
         sum=tot_cnt_wr
         n=tot_min_wr;
run;

*---------------------------------------------------------------------*;
*final data for one record per day for everyone in the analysis.      *;
*---------------------------------------------------------------------*;
data nw&nwperiod;
  merge sum_all(in=in_all) sum_nw(in=in_nw) sum_wear;
  by seqn day;
  if in_all;

  if tot_dur_nw=. then tot_dur_nw=0;
  if tot_min_wr=. then tot_min_wr=0;
  if tot_cnt_wr=. then tot_cnt_wr=0;

  wear_hr=tot_min_wr/60;
  tot_dur_nw=tot_dur_nw/60;
  label
  tot_dur_nw='Total duration(hr) of non-wear periods in a day'
  wear_hr='Total number of wear hours for the day'
  tot_min='Total number of valid minutes within a day'
  tot_cnt_wr='Total intensity counts from all wear minutes in a day'
  tot_min_wr='Total number of wear minutes in a day'
  ;
  keep seqn paxday day ridageyr tot_min tot_min_wr wear_hr tot_cnt_wr tot_dur_nw;
run;

%mend nw;

%nw(nwperiod=60);    /* this is where the duration criterion for a non-wear period is set */

*-----------------------------------------------------------------------------------------------*;
*Activity bouts defined by specified number of minutes with intensity count >= the threshold    *;
*                                                                                               *;
*An activity bout starts at a minute with an intensity count greater than or equal to the       *;
*threshold. Minutes with intensity count greater than or equal to the threshold are considered  *;
*to be valid minutes for the activity bout. A bout is established when the specified length of  *;
*consecutive valid minutes are reached. The activity bout stops when any of the following       *;
*conditions is met:                                                                             *;
*  - one minute with intensity < threshold                                                      *;
*  - one minute with a missing intensity count                                                  *;
*  - the last minute of the day                                                                 *;
*-----------------------------------------------------------------------------------------------*;

*--------------------------------------------------------------*;
*Flag the intensity counts that are above the threshold.       *;
*Use different activity bout criteria for different age groups.*;
*NOTE: to change the cutoff points for moderate or vigorous    *;
*      intensity, please modify the statements below for       *;
*      variables modthresh(moderate threshold) and             *;
*      vigthresh(vigorous threshold).                          *;
*--------------------------------------------------------------*;
data monitors;
  set monitors;
  /*moderate threshold*/
  if      ridageyr=6  then modthresh=1400;
  else if ridageyr=7  then modthresh=1515;
  else if ridageyr=8  then modthresh=1638;
  else if ridageyr=9  then modthresh=1770;
  else if ridageyr=10 then modthresh=1910;
  else if ridageyr=11 then modthresh=2059;
  else if ridageyr=12 then modthresh=2220;
  else if ridageyr=13 then modthresh=2393;
  else if ridageyr=14 then modthresh=2580;
  else if ridageyr=15 then modthresh=2781;
  else if ridageyr=16 then modthresh=3000;
  else if ridageyr=17 then modthresh=3239;
  else if ridageyr>=18 then modthresh=2020;

  /*vigorous threshold*/
  if      ridageyr=6  then vigthresh=3758;
  else if ridageyr=7  then vigthresh=3947;
  else if ridageyr=8  then vigthresh=4147;
  else if ridageyr=9  then vigthresh=4360;
  else if ridageyr=10 then vigthresh=4588;
  else if ridageyr=11 then vigthresh=4832;
  else if ridageyr=12 then vigthresh=5094;
  else if ridageyr=13 then vigthresh=5375;
  else if ridageyr=14 then vigthresh=5679;
  else if ridageyr=15 then vigthresh=6007;
  else if ridageyr=16 then vigthresh=6363;
  else if ridageyr=17 then vigthresh=6751;
  else if ridageyr>=18 then vigthresh=5999;

  /*moderate or vigorous activity*/
  if paxinten>=modthresh then _mv=1;
  else if paxinten ne . then _mv=0;

  /*vigorous activity*/
  if paxinten>=vigthresh then _v=1;
  else if paxinten ne . then _v=0;

  /*moderate activity*/
  if modthresh<=paxinten<vigthresh then _m=1;
  else if paxinten ne . then _m=0;

run;

proc sort data=monitors; by seqn day paxn; run;

*---------------------------------------------------------------------------------*;
*Macro %bouts defines duration of activity bouts based on user defined minimum    *;
*bout length.                                                                     *;
*                                                                                 *;
*Macro call %bouts(bout_flg=,boutperiod=);                                        *;
*bout_flg: variable name for activity bout intensity: _m(moderate), _v(vigorous), *;
*          _mv(moderate or vigorous)                                              *;
*boutperiod: minimum bout length (1 minute, 2 minutes, 3 minutes, etc)            *;
*---------------------------------------------------------------------------------*;
%macro bouts(bout_flg=,boutperiod=);
data out&bout_flg&boutperiod;
  set monitors;
  by seqn day paxn;
  if first.day then mv_num=0;       /*number of activity bouts*/

  if first.day or reset or stopped then do;
     strt_mv=0;     /*starting minute for the activity bout*/
     end_mv=0;      /*ending minute for the activity bout*/
     start=0;       /*indicator for starting the activity bout*/
     reset=0;       /*indicator for resetting and starting over*/
     mv_cnt=0;      /*number of minutes for the activity bout*/
     stopped=0;     /*indicator for stopping the activity bout*/
  end;
  retain mv_num strt_mv end_mv mv_cnt stopped reset start;


  /*start the bout when a count with intensity >= the threshold is encountered*/
  if &bout_flg=1 and start=0 then do;
        strt_mv=paxn;     /*assign the starting minute of the bout*/
        start=1;
  end;

  /*accumulate minutes with intensity counts >= the threshold*/
  if start=1 and &bout_flg=1 then do;
     mv_cnt=mv_cnt+1;
     end_mv=paxn;         /*keep track of the ending minute for the bout*/
  end;

  /*stop when encounter a minute with intensity < threshold or missing*/
  if &bout_flg in (0,.)  then  do;
     if mv_cnt<&boutperiod then reset=1;     /*reset if less than the bout length*/
     else stopped=1;
  end;

  /*last minute of the day*/
  if last.day and mv_cnt>=&boutperiod then stopped=1;

  /*output one record for each activity bout*/
  if stopped=1 then do;
      dur_mv=end_mv-strt_mv+1;
      mv_num=mv_num+1;
      output;
  label
  strt_mv='Starting minute for the activity bout'
  end_mv='Ending minute for the activity bout'
  dur_mv='Duration(minutes) of activity bout'
  mv_num='Number of activity bout'
  ;
  end;
  keep seqn  day mv_num strt_mv end_mv dur_mv ;
run;

proc sort data=out&bout_flg&boutperiod;
  by seqn day mv_num;
run;

*-----------------------------------------------*;
*calculate total duration of activity bouts for *;
*each day.                                      *;
*-----------------------------------------------*;
proc summary data=out&bout_flg&boutperiod;
  by seqn day;
  var dur_mv;
  output out=sum_mv
         sum=tot_dur_mv;
run;

*-----------------------------------------------*;
*output one record per day for each person in   *;
*the analysis.                                  *;
*-----------------------------------------------*;
data out&bout_flg&boutperiod._sum;
  merge sum_all(in=in_all) sum_mv;
  by seqn day;
  if in_all;
  if tot_dur_mv=. then tot_dur_mv=0;
  label
  %if &bout_flg=_mv %then %do;
    tot_dur_mv="Total duration(minutes) of moderate or vigorous activity bouts (minimum &boutperiod minute bouts) in a day"
  %end;
  %if &bout_flg=_m %then %do;
    tot_dur_mv="Total duration(minutes) of moderate activity bouts (minimum &boutperiod minute bouts) in a day"
  %end;
  %if &bout_flg=_v %then %do;
    tot_dur_mv="Total duration(minutes) of vigorous activity bouts (minimum &boutperiod minute bouts) in a day"
  %end;
  ;
  keep seqn day tot_dur_mv;
  rename
  tot_dur_mv=tot_dur&bout_flg&boutperiod;
run;
%mend bouts;

*-----------------------------------------------*;
*create activity bouts with moderate, vigorous, *;
*and moderate or vigorous intensity.            *;
*-----------------------------------------------*;
%macro boutsgrp(boutperiod);
  %bouts(bout_flg=_mv,boutperiod=&boutperiod);
  %bouts(bout_flg=_v,boutperiod=&boutperiod);
  %bouts(bout_flg=_m,boutperiod=&boutperiod);
%mend boutsgrp;

*-----------------------------------------------*;
*set bout length here (now set to 1 min)        *;
*-----------------------------------------------*;
%boutsgrp(1);     /*value here is bout length criterion*/

*---------------------------------------------------------------------------------------------*;
*Macro %bouts_8of10 defines activity bouts for 8 out of 10 minutes with intensity count >= the*;
*threshold.                                                                                   *;
*                                                                                             *;
*An activity bout starts with a count that is greater than or equal to the threshold.         *;
*A bout is established when 8 minutes out of a 10 minute window have intensity counts greater *;
*than or equal to the threshold. The bout stops when any of the following conditions is met:  *;
*  - 3 consecutive minutes with intensity < threshold                                         *;
*  - one minute with a missing intensity count                                                *;
*  - last minute of the day                                                                   *;
*                                                                                             *;
*Macro call %bouts_8of10(bout_flg=);                                                          *;
*bout_flg: variable name for activity bout intensity, _m(moderate), _v(vigorous),             *;
*          _mv(moderate or vigorous)                                                          *;
*---------------------------------------------------------------------------------------------*;
%macro bouts_8of10(bout_flg=);
data out&bout_flg;
  set monitors;
  by seqn day paxn;
  /*set up a 10 minute window*/
  array win_paxn(*) win_paxn1-win_paxn10;   /*minute*/
  array win_int(*) win_int1-win_int10;      /*intensity*/
  array win_flg(*) win_flg1-win_flg10;      /*bout flag*/

  if first.day then
     mv_num=0;             /*number of activity bouts*/

  if first.day or stopped or reset then do;
     strt_mv=0;     /*starting minute for the bout*/
     end_mv=0;      /*ending minute for the bout*/
     found=0;       /*set to 1 if a bout has been established*/
     reset=0;       /*reset the counts and start over*/
     stopped=0;     /*indicator for stopping the bout*/
     start=0;       /*start set to 1 if one above the threshold count is encountered*/
     mv_cnt=0;      /*number of minutes with counts >= the threshold*/
     sum10=.;       /*the total intensity counts from the 10 minute window*/
     cnt_below=0;   /*counter for number of minutes with intensity below the threshold*/ 
     do i=1 to 10;   /*initialize the 10 minute window*/
        win_paxn(i)=0;
        win_int(i)=0;
        win_flg(i)=0;
     end;
  end;
  retain mv_num reset strt_mv end_mv start mv_cnt  found stopped sum10 cnt_below;
  retain win_paxn1-win_paxn10;
  retain win_int1-win_int10;
  retain win_flg1-win_flg10;

  /*if the intensity count is >= the threshold, start the bout*/
  if &bout_flg=1 and start=0 then
     start=1;

  /*accumulate the counts*/
  if start=1 then mv_cnt=mv_cnt+1;

  /*set up a moving window of 10 minutes*/
  if 1<=mv_cnt<=10 and not found then do;
       win_paxn(mv_cnt)=paxn;
       win_int(mv_cnt)=paxinten;
       win_flg(mv_cnt)=&bout_flg;
       if paxinten = . then reset=1; /*if encounter a missing count before reaching the 10 minute count, reset and start again*/
   end;

   /*when reach 10 minutes, count the total number of intensity counts that are >= threshold*/
   if mv_cnt=10 and not reset then sum10=sum(of win_flg1-win_flg10);

   /*if 8 out of 10 minutes with intensity counts >= the threshold, a bout is established*/
   if sum10>=8 then found=1;

   /*if less than 8 minutes with intensity counts>= the threshold, continue to search*/
   /*move the 10-minute window down, one minute at a time*/ 
   else if 0<sum10<8 and mv_cnt>10 then do;
     if paxinten=. then reset=1;      /*if the 10th minute has a missing count, reset and start again*/
     else do;
          do i=1 to 9;
             win_paxn(i)=win_paxn(i+1);
             win_int(i)=win_int(i+1);
             win_flg(i)=win_flg(i+1);
          end;
          /*read in minute 10*/
          win_paxn(10)=paxn;
          win_int(10)=paxinten;
          win_flg(10)=&bout_flg;
          sum10=sum(of win_flg1-win_flg10);
     end;
   end;
   if sum10 in (0) then reset=1;               /*skip the windows with no valid minutes*/

  /*after the bout is established*/
  if found then do;
      /*assign the starting minute for the activity bout*/
      if strt_mv= 0 then do;
         do i=1 to 10;
            if win_flg(i)=1 then  do;  /*find the first minute with intensity count>=the threshold*/
               strt_mv=win_paxn(i);
               i=11;
            end;
         end;
      end;
      /*assign the ending minute for the activity bout*/
      if end_mv=0 then do;
         /*the last 2 minutes in the 10 minute window are below the threshold*/
         if win_flg(9)=0 and win_flg(10)=0 then do;
            end_mv= win_paxn(8);
            cnt_below=2;
         end;
         /*the last minute in the 10 minute window is below the threshold*/
         else if win_flg(10)=0 then do;
            end_mv=win_paxn(9);
            cnt_below=1;
         end;
         else
            end_mv=win_paxn(10);
      end;
      if paxn>win_paxn(10) then do;
         if &bout_flg=1 then do;
            cnt_below=0;
            end_mv=paxn;
         end;
         if &bout_flg=0  then
            cnt_below=cnt_below+1;  /*keep track of the number of minutes with intensity counts below the threshold*/
      end;
      /*bout terminates if 3 consecutive minutes below the threshold are encountered, or a missing count, or the last minute of the day*/
      if cnt_below=3 or last.day or &bout_flg=. then stopped=1;
  end;
  /*output one record for each activity bout*/
  if stopped=1 then do;
      dur_mv=end_mv-strt_mv+1;
      mv_num=mv_num+1;
      keep seqn day mv_num strt_mv end_mv dur_mv;
      output;
  end;
run;
proc sort data=out&bout_flg;
  by seqn day mv_num;
run;

*-----------------------------------------------*;
*calculate total duration of activity bouts for *;
*each day.                                      *;
*-----------------------------------------------*;
proc summary data=out&bout_flg;
  by seqn day;
  var dur_mv;
  output out=sum_mv
         sum=tot_dur_mv;
run;

*-----------------------------------------------*;
*output one record per day for each person in   *;
*the analysis.                                  *;
*-----------------------------------------------*;
data out&bout_flg._sum;
  merge sum_all(in=in_all) sum_mv;
  by seqn day;
  if in_all;
  if tot_dur_mv=. then tot_dur_mv=0;

  label
  %if &bout_flg=_mv %then %do;
    tot_dur_mv="Total duration(min) of moderate or vigorous activity bouts (8 out of 10 minutes) in a day"
  %end;
  %if &bout_flg=_m %then %do;
    tot_dur_mv="Total duration(min) of moderate activity bouts (8 out of 10 minutes) in a day"
  %end;
  %if &bout_flg=_v %then %do;
    tot_dur_mv="Total duration(min) of vigorous activity bouts (8 out of 10 minutes) in a day"
  %end;
  ;

  keep seqn day tot_dur_mv;

  rename
  tot_dur_mv=tot_dur&bout_flg;
run;
%mend bouts_8of10;

%bouts_8of10(bout_flg=_mv);
%bouts_8of10(bout_flg=_v);
%bouts_8of10(bout_flg=_m);

*-----------------------------------------------------------*;
*summarize to one record per person per day with duration   *;
*of non-wear and activity bouts                             *;
*-----------------------------------------------------------*;
data pam_perday;
  merge nw60
        out_mv_sum out_v_sum out_m_sum
        out_mv1_sum out_v1_sum out_m1_sum ;
  by seqn day;
run;

*------------------------------------------------------------------------------------------*;
*Copy the work dataset pam_perday to the folder referenced by the libname statement above. *;
*------------------------------------------------------------------------------------------*;
data myfolder.pam_perday;
  set pam_perday;
run;

proc contents data=myfolder.pam_perday;
run;

*------------------------------------------------------------------------------------------*;
*You have now created a dataset that has one record per person per day. It contains the    *;
*variables listed in pam_perday_contents.doc. It is used in create.pam_perperson.sas to    *;
*create a dataset with one record per person, and it may also be used for other analyses.  *;      
*------------------------------------------------------------------------------------------*;

