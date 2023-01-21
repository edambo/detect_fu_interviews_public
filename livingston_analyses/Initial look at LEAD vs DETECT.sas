/*read in LEAD Panel*/
proc import datafile="H:\DETECT\Study Data\110421\Lead Panel Assessment.csv"
        out=LEAD
        dbms=csv
        replace;
     getnames=yes;
run;

/*check # of lead assessments I have per ID*/
proc freq data=lead;
table MedstarID;
ods output OneWayFreqs=check;
run;

proc means data=check;
run;

data lead;
set lead;
if xRecordStatus ="Incomplete" and xcAssessmentScreened = "" then delete;
run;


data lead_1;
set lead;
if AssessmentType='Initial assessment';
run;

data lead_2;
set lead;
if AssessmentType='Secondary assessment';
run;

data lead_3;
set lead;
if AssessmentType='Post-detect assessment';
run;


/*remove duplicates by ID and rater*/
proc sql;
create table lead_unique1 as
	select *
	from lead_1
	group by MedstarID, xModifiedBy
	having xCreatedTimestamp=max(xCreatedTimestamp);
quit;

proc sql;
create table lead_unique2 as
	select *
	from lead_2
	group by MedstarID, xModifiedBy
	having xCreatedTimestamp=max(xCreatedTimestamp);
quit;

proc sql;
create table lead_unique3 as
	select *
	from lead_3
	group by MedstarID, xModifiedBy
	having xCreatedTimestamp=max(xCreatedTimestamp);
quit;


/*recheck, looks better*/
proc freq data=lead_unique1;
table MedstarID;
ods output OneWayFreqs=check;
run;

proc means data=check;
run;

proc freq data=lead_unique1;
table physicalabuse sexualabuse EmotionalPsychoAbuse neglect Abandonment FinancialExploitation selfneglect;
run;

data lead_toscore1;
set lead_unique1;
array char_var[7] physicalabuse sexualabuse EmotionalPsychoAbuse neglect 
                  Abandonment FinancialExploitation selfneglect;
array num_var[7] physicalabuse_ sexualabuse_ EmotionalPsychoAbuse_ neglect_ Abandonment_ 
                 FinancialExploitation_ selfneglect_;
do i=1 to 7;
	if char_var[i]="Yes" then num_var[i]=1; 
	if char_var[i]="No" then num_var[i]=0; 
end;
drop physicalabuse sexualabuse EmotionalPsychoAbuse neglect 
                  Abandonment FinancialExploitation selfneglect;
run;

data lead_toscore2;
set lead_unique2;
array char_var[7] physicalabuse sexualabuse EmotionalPsychoAbuse neglect 
                  Abandonment FinancialExploitation selfneglect;
array num_var[7] physicalabuse_ sexualabuse_ EmotionalPsychoAbuse_ neglect_ Abandonment_ 
                 FinancialExploitation_ selfneglect_;
do i=1 to 7;
	if char_var[i]="Yes" then num_var[i]=1; 
	if char_var[i]="No" then num_var[i]=0; 
end;
drop physicalabuse sexualabuse EmotionalPsychoAbuse neglect 
                  Abandonment FinancialExploitation selfneglect;
run;

data lead_toscore3;
set lead_unique3;
array char_var[7] physicalabuse sexualabuse EmotionalPsychoAbuse neglect 
                  Abandonment FinancialExploitation selfneglect;
array num_var[7] physicalabuse_ sexualabuse_ EmotionalPsychoAbuse_ neglect_ Abandonment_ 
                 FinancialExploitation_ selfneglect_;
do i=1 to 7;
	if char_var[i]="Yes" then num_var[i]=1; 
	if char_var[i]="No" then num_var[i]=0; 
end;
drop physicalabuse sexualabuse EmotionalPsychoAbuse neglect 
                  Abandonment FinancialExploitation selfneglect;
run;


/*make item scores and aggregate for each item and overall*/
proc sql;
create table lead_scored1 as
	select *,
	avg(physicalabuse_) as physicalabuse_score,
	avg(sexualabuse_) as sexualabuse_score,
	avg(EmotionalPsychoAbuse_) as EmotionalPsychoAbuse_score,
	avg(neglect_) as neglect_score,
	avg(Abandonment_) as Abandonment_score,
	avg(FinancialExploitation_) as FinancialExploitation_score,
	avg(selfneglect_) as selfneglect_score
	from lead_toscore1
	group by MedstarID
	;
quit;

proc sql;
create table lead_scored2 as
	select *,
	avg(physicalabuse_) as physicalabuse_score,
	avg(sexualabuse_) as sexualabuse_score,
	avg(EmotionalPsychoAbuse_) as EmotionalPsychoAbuse_score,
	avg(neglect_) as neglect_score,
	avg(Abandonment_) as Abandonment_score,
	avg(FinancialExploitation_) as FinancialExploitation_score,
	avg(selfneglect_) as selfneglect_score
	from lead_toscore2
	group by MedstarID
	;
quit;


proc sql;
create table lead_scored3 as
	select *,
	avg(physicalabuse_) as physicalabuse_score,
	avg(sexualabuse_) as sexualabuse_score,
	avg(EmotionalPsychoAbuse_) as EmotionalPsychoAbuse_score,
	avg(neglect_) as neglect_score,
	avg(Abandonment_) as Abandonment_score,
	avg(FinancialExploitation_) as FinancialExploitation_score,
	avg(selfneglect_) as selfneglect_score
	from lead_toscore3
	group by MedstarID
	;
quit;



data lead_scored1_;
set lead_scored1;
array num_var[7] physicalabuse_score sexualabuse_score EmotionalPsychoAbuse_score neglect_score Abandonment_score 
                 FinancialExploitation_score selfneglect_score;
array final_var[7] physicalabuse_lead sexualabuse_lead EmotionalPsychoAbuse_lead neglect_lead Abandonment_lead 
                 FinancialExploitation_lead selfneglect_lead;
do i=1 to 7;
	if 0=<num_var[i]<0.5 then final_var[i]=0;
	if 0.5=<num_var[i]=<1 then final_var[i]=1;
end;

if sum(physicalabuse_lead, sexualabuse_lead, EmotionalPsychoAbuse_lead, neglect_lead, Abandonment_lead, 
       FinancialExploitation_lead, selfneglect_lead)>=1 then lead_any=1;
if sum(physicalabuse_lead, sexualabuse_lead, EmotionalPsychoAbuse_lead, neglect_lead, Abandonment_lead, 
       FinancialExploitation_lead, selfneglect_lead)=0 then lead_any=0;

run;

data lead_scored2_;
set lead_scored2;
array num_var[7] physicalabuse_score sexualabuse_score EmotionalPsychoAbuse_score neglect_score Abandonment_score 
                 FinancialExploitation_score selfneglect_score;
array final_var[7] physicalabuse_lead sexualabuse_lead EmotionalPsychoAbuse_lead neglect_lead Abandonment_lead 
                 FinancialExploitation_lead selfneglect_lead;
do i=1 to 7;
	if 0=<num_var[i]<0.5 then final_var[i]=0;
	if 0.5=<num_var[i]=<1 then final_var[i]=1;
end;

if sum(physicalabuse_lead, sexualabuse_lead, EmotionalPsychoAbuse_lead, neglect_lead, Abandonment_lead, 
       FinancialExploitation_lead, selfneglect_lead)>=1 then lead_any=1;
if sum(physicalabuse_lead, sexualabuse_lead, EmotionalPsychoAbuse_lead, neglect_lead, Abandonment_lead, 
       FinancialExploitation_lead, selfneglect_lead)=0 then lead_any=0;

run;

data lead_scored3_;
set lead_scored3;
array num_var[7] physicalabuse_score sexualabuse_score EmotionalPsychoAbuse_score neglect_score Abandonment_score 
                 FinancialExploitation_score selfneglect_score;
array final_var[7] physicalabuse_lead sexualabuse_lead EmotionalPsychoAbuse_lead neglect_lead Abandonment_lead 
                 FinancialExploitation_lead selfneglect_lead;
do i=1 to 7;
	if 0=<num_var[i]<0.5 then final_var[i]=0;
	if 0.5=<num_var[i]=<1 then final_var[i]=1;
end;

if sum(physicalabuse_lead, sexualabuse_lead, EmotionalPsychoAbuse_lead, neglect_lead, Abandonment_lead, 
       FinancialExploitation_lead, selfneglect_lead)>=1 then lead_any=1;
if sum(physicalabuse_lead, sexualabuse_lead, EmotionalPsychoAbuse_lead, neglect_lead, Abandonment_lead, 
       FinancialExploitation_lead, selfneglect_lead)=0 then lead_any=0;

run;


/*collapse into single row, only care about the new variables that are aggregated across raters*/
proc sort data=lead_scored1_ nodupkey out=lead_aggregate1 
(keep=MedstarID xCreatedTimestamp xRecordMonth xRecordYear physicalabuse_lead sexualabuse_lead 
	  EmotionalPsychoAbuse_lead neglect_lead Abandonment_lead 
      FinancialExploitation_lead selfneglect_lead lead_any)
;
by MedstarID;
run;

proc sort data=lead_scored2_ nodupkey out=lead_aggregate2 
(keep=MedstarID xCreatedTimestamp xRecordMonth xRecordYear physicalabuse_lead sexualabuse_lead 
	  EmotionalPsychoAbuse_lead neglect_lead Abandonment_lead 
      FinancialExploitation_lead selfneglect_lead lead_any)
;
by MedstarID;
run;

proc sort data=lead_scored3_ nodupkey out=lead_aggregate3 
(keep=MedstarID xCreatedTimestamp xRecordMonth xRecordYear physicalabuse_lead sexualabuse_lead 
	  EmotionalPsychoAbuse_lead neglect_lead Abandonment_lead 
      FinancialExploitation_lead selfneglect_lead lead_any)
;
by MedstarID;
run;

/*check distributions*/
proc freq data=lead_aggregate;
table physicalabuse_lead sexualabuse_lead EmotionalPsychoAbuse_lead neglect_lead Abandonment_lead 
      FinancialExploitation_lead selfneglect_lead lead_any;
run;

/************************************/
/*DO IT AGAIN FOR DETECT YOU SLACKER*/
/************************************/

proc import datafile="H:\DETECT\Study Data\110421\Participant.csv"
        out=detect
        dbms=csv
        replace;
     getnames=yes;
run;

data detect1;
set detect;
array DETECT_have[14] xUnusualOdor xHealthSafetyConcern xInadequatelyClothed xIsolatedHome
					xDisarrayHoarding xCaregiverUnengaged xCaregiverLackKnowledge xCaregiverAnxious
					xCaregiverFrustrated xUnmetNeeds xPoorPersonalHygiene xDifficultyMedications
					xHoardingMedications xDepressed;
array detect_want[14] xUnusualOdor_ xHealthSafetyConcern_ xInadequatelyClothed_ xIsolatedHome_
					xDisarrayHoarding_ xCaregiverUnengaged_ xCaregiverLackKnowledge_ xCaregiverAnxious_
					xCaregiverFrustrated_ xUnmetNeeds_ xPoorPersonalHygiene_ xDifficultyMedications_
					xHoardingMedications_ xDepressed_;

do i=1 to 14;
	if DETECT_have[i]='Yes' then DETECT_want[i]=1;
	if DETECT_have[i]='No' then DETECT_want[i]=0;
	if DETECT_have[i]='N/A' then DETECT_want[i]=.;
	if DETECT_have[i]="Did Not Enter Patient's Home" then DETECT_want[i]=0;
	if DETECT_have[i]='Unable To Assess' then DETECT_want[i]=0;
end;
if sum(xUnusualOdor_, xHealthSafetyConcern_, xInadequatelyClothed_, xIsolatedHome_,
					xDisarrayHoarding_, xCaregiverUnengaged_, xCaregiverLackKnowledge_, xCaregiverAnxious_,
					xCaregiverFrustrated_, xUnmetNeeds_, xPoorPersonalHygiene_, xDifficultyMedications_,
					xHoardingMedications_, xDepressed_)=0 then DETECT_any=0; 
if sum(xUnusualOdor_, xHealthSafetyConcern_, xInadequatelyClothed_, xIsolatedHome_,
					xDisarrayHoarding_, xCaregiverUnengaged_, xCaregiverLackKnowledge_, xCaregiverAnxious_,
					xCaregiverFrustrated_, xUnmetNeeds_, xPoorPersonalHygiene_, xDifficultyMedications_,
					xHoardingMedications_, xDepressed_)>0 then DETECT_any=1; 


keep MedstarID xRecordMonth xRecordYear age race xUnusualOdor_ xHealthSafetyConcern_ xInadequatelyClothed_ xIsolatedHome_
					xDisarrayHoarding_ xCaregiverUnengaged_ xCaregiverLackKnowledge_ xCaregiverAnxious_
					xCaregiverFrustrated_ xUnmetNeeds_ xPoorPersonalHygiene_ xDifficultyMedications_
					xHoardingMedications_ xDepressed_ DETECT_any xApsReported
;
run;

/*check stuff*/
proc freq data=detect1;
table
DETECT_any*xApsReported
;
run;

/****************/
/*MERGE AND LOOK*/
/****************/

proc sort data=detect1;
by MedstarID;
run;

proc sort data=lead_aggregate1;
by MedstarID;
run;

proc sort data=lead_aggregate2;
by MedstarID;
run;

proc sort data=lead_aggregate3;
by MedstarID;
run;

data merged_lead_DETECT1;
merge lead_aggregate1(in=a) detect1;
by MedstarID;
if a;
run;

data merged_lead_DETECT2;
merge lead_aggregate2(in=a) detect1;
by MedstarID;
if a;
run;

data merged_lead_DETECT3;
merge lead_aggregate3(in=a) detect1;
by MedstarID;
if a;
run;


ods html file="H:\DETECT\intial look at DETECT vs LEAD Nov 2021.html";
proc freq data=merged_lead_DETECT1;
title 'LEAD Sample, Initial Assessment';
tables lead_any DETECT_any lead_any*DETECT_any;
run;

proc freq data=merged_lead_DETECT2;
title 'LEAD Sample, Secondary Assessment';
tables lead_any DETECT_any lead_any*DETECT_any;
run;

proc freq data=merged_lead_DETECT3;
title 'LEAD Sample, Post-Detect Assessment';
tables lead_any DETECT_any lead_any*DETECT_any;
run;

proc freq data=detect1;
title 'Full DETECT Sample';
tables xUnusualOdor_ xHealthSafetyConcern_ xInadequatelyClothed_ xIsolatedHome_
					xDisarrayHoarding_ xCaregiverUnengaged_ xCaregiverLackKnowledge_ xCaregiverAnxious_
					xCaregiverFrustrated_ xUnmetNeeds_ xPoorPersonalHygiene_ xDifficultyMedications_
					xHoardingMedications_ xDepressed_ DETECT_any
;
run;
ods html close;
ods html;


/*
data list_;
set merged_lead_DETECT;
if detect_any=1 or lead_any=1;
run;*/

proc import datafile="H:\DETECT\Study Data\110421\Sociodemographic Information.csv"
        out=demos
        dbms=csv
        replace;
     getnames=yes;
run;

proc sort data=demos;
by MedstarID;
run;

proc sort data=detect1;
by MedstarID;
run;

data vet_status;
merge demos (in=a) detect1;
by medstarid;
if a;
run;

ods html file="H:\DETECT\DETECT by Vet Status Nov 2021.html";
proc freq data=vet_status;
table SodeMilitary*detect_any;
run;
ods html close;
ods html;
