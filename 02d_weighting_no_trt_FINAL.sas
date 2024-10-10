LIBNAME desynpuf "C:\Users\michael.webster\Documents\CCW_tutorial\Stored_data";

/*CENSORING WEIGHTS FOR THE "UNTREATED" ARM*/

/*This set has two forms of censoring: general loss to follow-up and censoring because of starting a prescription. While we can assume the former 
is basically random, we should for censoring due to the latter by fitting inverse probability of remaining uncensored weights. We also
need to make sure we account for the "t0" censoring that occurs. Let's take care of the time 0 censoring first in its own step.*/

/*First, we fit a model in the notrt data set predicting the probability of being uncensored at time 0 (i.e., t0censnotrt = 0) based on baseline 
covariates including renal, baseline age, and sex. We'll use GENMOD for multivariable logistic regression. Because we want to predict
the probability of being UNCENSORED, we should not use a DESCENDING statement.*/

PROC GENMOD DATA=desynpuf.notrt_with_cens;
	CLASS sex renal / param=ref;
	MODEL t0censnotrt = baseline_age sex renal / link=logit dist=binomial; 
	OUTPUT OUT=notrt_t0_uncens prob=t0_uncens;
RUN;

/*Now we can generate a new data set with those who were not censored at t0, assigning them weights based on these conditional probabilities to counter
the t0 censoring. We can call those weights "t0IPCW" to represent their action at t0.*/

DATA notrt_with_cens_no_0_fu;
	SET notrt_t0_uncens;
	WHERE t0censnotrt ^= 1;
	t0IPCW=1/t0_uncens;
RUN;

/*This new data set still has potentially informative censoring, however, from those with cens_start = 1. We will need to fix this using IPCW applied
to a "long" data set of observations, basing the IPCW on a pooled multivariable logistic model predicting the probability of censoring.*/

/*First, let's make the data set long. Since we're looking at 180-day risks, we can create 18 intervals of 10 days each. We also need to recalculate any time-varying variables (e.g., age)*/

DATA long_no_trt;
	SET notrt_with_cens_no_0_fu;
	DO start_interval = 0 to 170 by 10; /*This will generate up to 18 observations per individual, each covering 10 days*/

		date = discharge_date + start_interval; /*Find the date for the start of the interval*/
		intv_age=intck('year',birthdate,date,"c"); /*Calculate their age at the start of the interval*/

		IF Cens_followup > (start_interval + 10) THEN DO; /*If someone's censored follow-up is greater than the start of the interval plus ten...*/
			long_outcome = 0; /*They didn't have the outcome during the interval*/
			long_cens_start = 0; /*They didn't get censored because of starting the treatment in that interval*/
			interval_fu = 10; /*They should get credit for all 10 days of person-time in the interval*/
			end_interval = start_interval + 10; /*The interval will end after 10 days*/
		END;

		ELSE IF Cens_followup <= (start_interval + 10) THEN DO; /*On the other hand, if someone's censored follow-up falls within the interval...*/
			long_outcome = cens_outcome; /*They should get the value of the outcome from the "short" data set*/
			long_cens_start = cens_start; /*They should get the value of cens_start from the short data set. This prevents people who experienced the outcome or were lost to follow-up people from being miscategorized*/
			interval_fu = Cens_followup - start_interval; /*They should get the days of follow-up based on their actual censored followup*/
			end_interval = Cens_followup; /*Their interval should end at the actual censored followup*/
		END;

		OUTPUT;
		IF Cens_followup <= (start_interval + 10) THEN LEAVE; /*Now those who met the second "IF" criteria should stop generating observations*/
	END;
RUN;


/*Now, we can fit a pooled multivariable logistic model predicting the probability of long_cens_start being equal to 0 (i.e., remaining uncensored)
across the entire follow-up period. We probably want to include a term for start_interval with both linear and quadratic terms to capture time trends somewhat flexibly.*/

PROC GENMOD DATA=long_no_trt;
	CLASS sex renal / param=ref;
	MODEL long_cens_start = intv_age sex renal start_interval start_interval*start_interval / link=logit dist=binomial; 
	OUTPUT OUT=long_no_trt_for_IPCW prob=FU_uncens;
RUN;
			
/*We can now generate our interval-specific and cumulative IPCWs*/

DATA long_no_trt_weights;
	SET long_no_trt_for_IPCW;
	RETAIN Cumulative_IPCW; /*Allow this variable to cross observations*/
	BY ID;
	Interval_IPCW = 1/FU_uncens;
	IF first.ID THEN Cumulative_IPCW = t0IPCW*Interval_IPCW; /*For the first observation of each patient, initiate their cumulative IPCW as their t0IPCW then multiply by their IPCW for the current interval*/
	ELSE Cumulative_IPCW = Interval_IPCW * Cumulative_IPCW; /*For every subsequent observation, their cumulative IPCW is equal to the product of their last cumulative IPCW and the interval IPCW*/
RUN;

/*And now we can save our final analytic dataset*/

DATA desynpuf.wted_no_trt;
	SET long_no_trt_weights;
RUN;

