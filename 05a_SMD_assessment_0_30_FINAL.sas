LIBNAME desynpuf "C:\Users\michael.webster\Documents\CCW_tutorial\Stored_data";

/*COMPARING STANDARDIZED MEAN DIFFERENCES*/

/*Let's identify whether the inverse probability of remaining uncensored weights we built are properly reducing covariate imbalances*/

/*This code shows how to calculate these values for the 0_30 analysis/

/*Load in the weighted data*/

DATA wted_trt_0_30;
	SET desynpuf.wted_trt_0_30;
RUN;

/*Subset to observations with start_interval equal to 30 and recentstart equal to 1, creating a flag for treated named "treat"*/

DATA start_30_recentstart_1_trt;
	SET wted_trt_0_30;
	IF start_interval = 30 AND recentstart = 1;
	treat = 1;
RUN;

/*And the "zeros"*/

DATA zeros_0_30;
	SET desynpuf.zeros_0_30;
RUN;

/*Doing the same subsetting as above to make sure we eliminate any earlier 0-duration intervals, with a 0 for the "treat" flag. We will also give them IPCWs of 1 to make the weighted
comparisons a bit easier. This has to be handled a bit differently in the "30_90" analysis where some of these people will have non-zero IPCW.*/

DATA start_30_recentstart_1_zeros;
	SET zeros_0_30;
	IF start_interval = 30 AND recentstart = 1;
	treat = 0;
	Cumulative_IPCW = 1;
RUN;

/*Now let's combine the two data sets, keeping covariate information, the treatment flag, and cumulative IPCW.*/

DATA interval_30;
	SET start_30_recentstart_1_trt start_30_recentstart_1_zeros;
	KEEP intv_age sex renal cumulative_IPCW treat; 
RUN;

/*Now that the cohort is created, we can start estimating SMDs.*/

/*Let's start in the unweighted cohort. First, let's get the means and standard deviations for Table 1*/

PROC MEANS DATA=interval_30 MEAN STDDEV; /*We want means and standard deviation*/
	CLASS treat; /*This calculates values in each group and overall*/
	VAR intv_age;
	OUTPUT OUT=interval_30_mean_stddev_unwt mean=age_mean stddev=age_stddev;
RUN;

/*To calculate the SMD, we need to divide the difference between the two groups by the standard deviation in the overall cohort*/

/*The easiest way to do this is with some transposing. First, we should replace the "missing" value for treat in the overall with some value*/

DATA prep_tpose_interval_30_unwt;
	SET interval_30_mean_stddev_unwt;
	IF treat=. THEN treat = -999;
RUN;

/*Next, we can use PROC TRANPOSE to put all the mean information into 1 observation*/

PROC TRANSPOSE DATA=prep_tpose_interval_30_unwt OUT=tposed_30_mean_unwt PREFIX=Mean_;
	VAR age_mean;
	ID treat;
RUN;

/*And same for standard deviation*/

PROC TRANSPOSE DATA=prep_tpose_interval_30_unwt OUT=tposed_30_stddev_unwt PREFIX=STDDEV_;
	VAR age_stddev;
	ID treat;
RUN;

/*Now we can add flags to merge these sets together*/

DATA tposed_30_mean_flag_unwt;
	SET tposed_30_mean_unwt;
	analysis="unwt"; /*The flag is set to unweighted, since these are the results before IPCW*/
	DROP _name_; /*We don't need this variable anymore*/
RUN;

DATA tposed_30_stddev_flag_unwt;
	SET tposed_30_stddev_unwt;
	analysis="unwt";
	DROP _name_;
RUN;

/*Now we merge the data and calculate the SMD*/

DATA merge_unwt_analysis;
	MERGE tposed_30_mean_flag_unwt tposed_30_stddev_flag_unwt;
	BY analysis;
	variable = "age";
	SMD = (mean_1 - mean_0) / STDDEV__999;
RUN;

/*What about binary variables? This is actually easier. We just need the proportions in each group, easy to get in two tables.*/

PROC FREQ DATA=interval_30;
	TABLES renal*treat / OUT=renalprop_unwt_30 OUTPCT;
	TABLES sex*treat / OUT=sexprop_unwt_30 OUTPCT;
RUN;

/*Next, we limit ourselves to one level of the variables (1 for female, Y for renal) and create flag for the variable name and create actual proportions*/

DATA only_Y_renal_unwt_30;
	SET renalprop_unwt_30;
	IF renal = "Y";
	variable = "ren";
	PROP=PCT_COL/100;
RUN;

DATA only_1_sex_unwt_30;
	SET sexprop_unwt_30;
	IF sex="1";
	variable = "fem";
	PROP=PCT_COL/100;
RUN;

/*Next, we transpose the proportions in each of these data sets, keeping the variable name*/

PROC TRANSPOSE DATA=only_Y_renal_unwt_30 OUT=tposed_30_renal_unwt PREFIX=TREAT_;
	VAR PROP;
	ID treat;
	BY variable;
RUN;

PROC TRANSPOSE DATA=only_1_sex_unwt_30 OUT=tposed_30_sex_unwt PREFIX=TREAT_;
	VAR PROP;
	ID treat;
	BY variable;
RUN;

/*Next, we can combine these sets together and calculate SMDs*/

DATA cat_var_30_unwt;
	SET tposed_30_renal_unwt tposed_30_sex_unwt;
	SMD = (TREAT_1 - TREAT_0)/ SQRT( ( ( TREAT_1*(1-TREAT_1) + TREAT_0*(1-TREAT_0) ) / 2 ) );
RUN;

/*What if we want to estimate the standardized mean differences after weighting? We repeat the above with a "weight" statement in the PROC MEANS/PROC FREQ.*/

PROC MEANS DATA=interval_30 MEAN STDDEV; /*We want means and standard deviation*/
	CLASS treat; /*This calculates values in each group and overall*/
	VAR intv_age;
	WEIGHT cumulative_IPCW;
	OUTPUT OUT=interval_30_mean_stddev_wt mean=age_mean stddev=age_stddev;
RUN;

/*To calculate the SMD, we need to divide the difference between the two groups by the standard deviation in the overall cohort*/

/*The easiest way to do this is with some transposing. First, we should replace the "missing" value for treat in the overall with some value*/

DATA prep_tpose_interval_30_wt;
	SET interval_30_mean_stddev_wt;
	IF treat=. THEN treat = -999;
RUN;

/*Next, we can use PROC TRANPOSE to put all the mean information into 1 observation*/

PROC TRANSPOSE DATA=prep_tpose_interval_30_wt OUT=tposed_30_mean_wt PREFIX=Mean_;
	VAR age_mean;
	ID treat;
RUN;

/*And same for standard deviation*/

PROC TRANSPOSE DATA=prep_tpose_interval_30_wt OUT=tposed_30_stddev_wt PREFIX=STDDEV_;
	VAR age_stddev;
	ID treat;
RUN;

/*Now we can add flags to merge these sets together*/

DATA tposed_30_mean_flag_wt;
	SET tposed_30_mean_wt;
	analysis="wted"; /*The flag is set to unweighted, since these are the results before IPCW*/
	DROP _name_; /*We don't need this variable anymore*/
RUN;

DATA tposed_30_stddev_flag_wt;
	SET tposed_30_stddev_wt;
	analysis="wted";
	DROP _name_;
RUN;

/*Now we merge the data and calculate the SMD*/

DATA merge_wt_analysis;
	MERGE tposed_30_mean_flag_wt tposed_30_stddev_flag_wt;
	BY analysis;
	variable = "age";
	SMD = (mean_1 - mean_0) / STDDEV__999;
RUN;

/*What about binary variables? This is actually easier. We just need the proportions in each group, easy to get in two tables.*/

PROC FREQ DATA=interval_30;
	TABLES renal*treat / OUT=renalprop_wt_30 OUTPCT;
	WEIGHT cumulative_IPCW;
	TABLES sex*treat / OUT=sexprop_wt_30 OUTPCT;
	WEIGHT cumulative_IPCW;
RUN;

/*Next, we limit ourselves to one level of the variables (1 for female, Y for renal) and create flag for the variable name and create actual proportions*/

DATA only_Y_renal_wt_30;
	SET renalprop_wt_30;
	IF renal = "Y";
	variable = "ren";
	PROP=PCT_COL/100;
RUN;

DATA only_1_sex_wt_30;
	SET sexprop_wt_30;
	IF sex="1";
	variable = "fem";
	PROP=PCT_COL/100;
RUN;

/*Next, we transpose the proportions in each of these data sets, keeping the variable name*/

PROC TRANSPOSE DATA=only_Y_renal_wt_30 OUT=tposed_30_renal_wt PREFIX=TREAT_;
	VAR PROP;
	ID treat;
	BY variable;
RUN;

PROC TRANSPOSE DATA=only_1_sex_wt_30 OUT=tposed_30_sex_wt PREFIX=TREAT_;
	VAR PROP;
	ID treat;
	BY variable;
RUN;

/*Finally, we can combine these sets together and calculate weighted SMDs*/

DATA cat_var_30_wt;
	SET tposed_30_renal_wt tposed_30_sex_wt;
	SMD = (TREAT_1 - TREAT_0)/ SQRT( ( ( TREAT_1*(1-TREAT_1) + TREAT_0*(1-TREAT_0) ) / 2 ) );
RUN;
