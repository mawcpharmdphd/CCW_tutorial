LIBNAME desynpuf "C:\Users\michael.webster\Documents\CCW_tutorial\Stored_data";

/*ESTIMATING EFFECTS*/

/*We now have four weighted data sets representing potential mortality (our outcome) from day 0 to 180 under four distinct hypothetical interventions following myocardial infarction:
1) No treatment initiation 
2) Treatment initiation within day 0 to 90
3) Treatment initiation within day 0 to 30
4) Treatment initiation from day 30 to 90
We can contrast the mortality within each of these data sets using our effect measure of interest, the risk difference.*/

/*Suppose we are interested in comparing the risk difference for 180 day mortality comparing the "0 to 90" intervention with the "0-30" intervention as a referent.*/

/*First, we should load in our referent group, "0-30 day." */

DATA treatment_30;
	SET desynpuf.wted_trt_0_30;
RUN;

PROC PHREG DATA=treatment_30;
	MODEL end_interval*long_outcome (0) = / entrytime=start_interval;
	weight Cumulative_IPCW;
	BASELINE OUT=treatment_30_surv_curv survival=surv_trt_30;
RUN;

DATA treatment_30_180_day;
	SET treatment_30_surv_curv end=last; /*This creates a temporary flag to identify the last observation in the data set*/
	risk_trt_30 = 1-surv_trt_30;
	flag = 1; /*Creating a flag for merging*/
	IF last;
RUN;

/*We can do the same thing to the treatment initiation within day 0 to 90 group*/

DATA treatment_90;
	SET desynpuf.wted_trt_0_90;
RUN;

PROC PHREG DATA=treatment_90;
	MODEL end_interval*long_outcome (0) = / entrytime=start_interval;
	weight Cumulative_IPCW;
	BASELINE OUT=treatment_90_surv_curv survival=surv_trt_90;
RUN;

DATA treatment_90_180_day;
	SET treatment_90_surv_curv end=last; /*This creates a temporary flag to identify the last observation in the data set*/
	risk_trt_90 = 1-surv_trt_90;
	flag = 1; /*Creating a flag for merging*/
	IF last;
RUN;

/*We can repeat this for the other group as well*/

DATA treatment_30_90;
	SET desynpuf.wted_trt_30_90;
RUN;

PROC PHREG DATA=treatment_30_90;
	MODEL end_interval*long_outcome (0) = / entrytime=start_interval;
	weight Cumulative_IPCW;
	BASELINE OUT=treatment_30_90_surv_curv survival=surv_trt_30_90;
RUN;

DATA treatment_30_90_180_day;
	SET treatment_30_90_surv_curv end=last; /*This creates a temporary flag to identify the last observation in the data set*/
	risk_trt_30_90 = 1-surv_trt_30_90;
	flag = 1; /*Creating a flag for merging*/
	IF last;
RUN;

/*And now we can merge everything, still using 0-30 as a referent*/

DATA merge_all_regimens_risks;
	MERGE treatment_90_180_day treatment_30_180_day treatment_30_90_180_day;
	BY flag;
	Risk_difference_90_vs_30 = risk_trt_90 - risk_trt_30;
	Risk_difference_30_90_vs_30 = risk_trt_30_90 - risk_trt_30;
RUN;

/*And print our estimates*/

PROC PRINT DATA=merge_all_regimens_risks;
	VAR risk_difference_90_vs_30 risk_difference_30_90_vs_30;
RUN;

/*We could calculate risk ratios just as easily. Note that these are not accurate due to the nature of the
synthetic data disrupting the treatment/outcome relationship.*/
