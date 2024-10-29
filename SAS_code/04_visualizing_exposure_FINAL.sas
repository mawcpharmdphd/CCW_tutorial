LIBNAME desynpuf "C:\Users\michael.webster\Documents\CCW_tutorial\Stored_data";

/*COMPARING EXPOSURE ACROSS REGIMENS*/

/*Suppose we want to delve deeper into the actual exposure patterns the population experiences in each regimen.*/

/*One way to do this is to plot the cumulative incidence of exposure over time for each regimen (after weighting/censoring). This is a little tricky, but not impossible.*/

/*Let's start with the 0-30 day regimens. We want to plot the percentage of the population on treatment from day 0 to 30.*/

/*How can we do this? One way is generating daily observations and examining the % of people on treatment on each day.*/

DATA daily_trt_0_30;

	SET desynpuf.wted_trt_0_30;

	DO day = start_interval TO end_interval - 1; /*This will generate one observation per day in the interval*/
		
	IF RX_start = . THEN on_treatment = 0; /*These are the easiest to handle, since they will always not be on treatment*/

	ELSE IF RX_start <= day THEN on_treatment = 1; /*If people have an RX_start date prior to the start of the interval, they're on treatment*/
	
	ELSE on_treatment = 0; /*Otherwise, they shouldn't be on treatment yet*/

	IF interval_fu ^= 0 THEN OUTPUT; /*We need to kick out all the 0-day observations*/

	END;
RUN;


PROC MEANS DATA=daily_trt_0_30 NOPRINT;
	CLASS day;
	VAR on_treatment;
	WEIGHT Cumulative_IPCW;
	OUTPUT OUT=pct_on_treatment_0_30 mean=cume_proportion_0_30;
RUN;
		
/*Let's gather the same information for the 0-90 individuals*/;

DATA daily_trt_0_90;

	SET desynpuf.wted_trt_0_90;

	DO day = start_interval TO end_interval - 1; /*This will generate one observation per day in the interval*/
		
	IF RX_start = . THEN on_treatment = 0; /*These are the easiest to handle, since they will always not be on treatment*/

	ELSE IF RX_start <= day THEN on_treatment = 1; /*If people have an RX_start date prior to the start of the interval, they're on treatment*/
	
	ELSE on_treatment = 0; /*Otherwise, they shouldn't be on treatment yet*/

	IF interval_fu ^= 0 THEN OUTPUT; /*We need to kick out all the 0-day observations*/

	END;
RUN;

PROC MEANS DATA=daily_trt_0_90 NOPRINT;
	CLASS day;
	VAR on_treatment;
	WEIGHT Cumulative_IPCW;
	OUTPUT OUT=pct_on_treatment_0_90 mean=cume_proportion_0_90;
RUN;


DATA pct_on_trt_0_30_0_90;
	MERGE pct_on_treatment_0_90 pct_on_treatment_0_30;
	BY day;
	WHERE _TYPE_ = 1;
RUN;

PROC SGPLOT DATA=pct_on_trt_0_30_0_90;
	STEP X=day Y=cume_proportion_0_30 / transparency = 0.5 lineattrs=(color=blue pattern=solid thickness=3);
	STEP X=day Y=cume_proportion_0_90 / transparency = 0.5 lineattrs=(color=red pattern=dash thickness=3);
	XAXIS MAX = 95 MIN=0;
	YAXIS MAX = 1 MIN=0;
RUN;

/*What if we want to try something similar with the 30-90 day individuals? We need to modify the code, as we already have daily observations*/


DATA daily_trt_30_90;

	SET desynpuf.wted_trt_30_90;

	IF start_interval < 30 THEN DO; /*First, we need to create variables for the daily observations we already have*/
		
		day = start_interval; /*Create a day variable designating the time since starting follow-up*/

		IF RX_start ^= . AND RX_start <= day THEN on_treatment = 1; /*Assign those who have already initiated treatment an "on_treatment" of 1 (note: this will be 0 throughout 0-30 in the 30-90 cohort by design*/

		ELSE on_treatment = 0; /*Otherwise they aren't on treatment yet.*/

		IF interval_fu ^= 0 THEN OUTPUT; /*Make sure to kick out 0-day observations*/

	END;

	ELSE IF start_interval = 30 OR start_interval = 90 THEN DO;

		DO day = start_interval TO end_interval - 1; /*This will generate one observation per day in the interval*/
		
		IF RX_start = . THEN on_treatment = 0; /*These are the easiest to handle, since they will always not be on treatment*/

		ELSE IF cens_startearly = 1 THEN on_treatment = 0; /*We made sure people who started treatment early got 0 days of follow-up on treatment*/

		ELSE IF RX_start <= day THEN on_treatment = 1; /*If people have an RX_start date prior to the start of the interval, they're on treatment*/
	
		ELSE on_treatment = 0; /*Otherwise, they shouldn't be on treatment yet*/

		IF interval_fu ^= 0 THEN OUTPUT; /*We need to kick out all the 0-day observations*/

		END;

	END;

RUN;

PROC MEANS DATA=daily_trt_30_90 NOPRINT;
	CLASS day;
	VAR on_treatment;
	WEIGHT Cumulative_IPCW;
	OUTPUT OUT=pct_on_treatment_30_90 mean=cume_proportion_30_90;
RUN;

/*Now we put them all together*/
DATA pct_on_trt_all;
	MERGE pct_on_treatment_0_90 pct_on_treatment_0_30 pct_on_treatment_30_90 ;
	BY day;
	WHERE _TYPE_ = 1;
RUN;

/*And make a plot*/

PROC SGPLOT DATA=pct_on_trt_all NOAUTOLEGEND;
	STEP X=day Y=cume_proportion_0_30 / transparency = 0.1 lineattrs=(color=blue pattern=solid thickness=3);
	STEP X=day Y=cume_proportion_0_90 / transparency = 0.1 lineattrs=(color=red pattern=dash thickness=3);
	STEP X=day Y=cume_proportion_30_90 / transparency = 0.1 lineattrs=(color=gray pattern=dot thickness=3);
	XAXIS MAX = 95 MIN=0LABEL="Time since myocardial infarction" LABELATTRS=(size=14pt weight=bold) VALUEATTRS=(size=12pt weight=bold);
	YAXIS MAX = 1 MIN=0 LABEL="Proportion having initiated treatment" LABELATTRS=(size=14pt weight=bold) VALUEATTRS=(size=12pt weight=bold);
RUN;
