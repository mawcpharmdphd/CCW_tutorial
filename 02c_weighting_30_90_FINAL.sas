LIBNAME desynpuf "C:\Users\michael.webster\Documents\CCW_tutorial\Stored_data";

/*CENSORING WEIGHTS FOR THE 30-90 DAY WINDOW*/

/*Now, let's weight the trt_30_90 data set. This incorporates some elements of the "trt_0_30" and "trt_0_90" data sets with additional censoring during the first 30 days.

This set has three forms of censoring: 
1) general loss to follow-up
2) censoring because patients starting taking medications from day 0 to 30 (cens_startearly = 1)
3) censoring because patients did not start taking medications by day 90 (cens_nostart = 1)

We will be assuming that general loss to follow-up is random, meaning we only have potential selection bias from "cens_startearly = 1" before day 30 and "cens_nostart=1" at day 90.*/

/*Before doing anything, we need to deal with the day 0 censoring.*/

PROC GENMOD DATA=desynpuf.trt_30_90_with_cens;
	CLASS sex renal / param=ref;
	MODEL t0cens30_90 = baseline_age sex renal / link=logit dist=binomial; 
	OUTPUT OUT=trt_30_90_t0_uncens prob=t0_uncens;
RUN;

/*Now we can generate a new data set with those who were not censored at t0, assigning them weights based on these conditional probabilities to counter
the t0 censoring. We can call those weights "t0IPCW" to represent their action at t0.*/

DATA trt_30_90_with_cens_no_0_fu;
	SET trt_30_90_t0_uncens;
	WHERE t0cens30_90 ^= 1;
	t0IPCW=1/t0_uncens;
RUN;


/*Next, let's make the data set long. Unlike the trt_0_30 and trt_0_90 analyses, we will need more than two observations. This is because we will need to account
for censoring from cens_startearly = 1 during the 0-30 day window. We can do this with 30 one-day intervals starting on day 0. We will then have an interval from
day 30 to 90 where no one is censored for either starting early or not starting, and then a final interval from day 90 to 180 where we deal with the censoring from cens_nostart = 1.*/

DATA long_trt_30_90;
	SET trt_30_90_with_cens_no_0_fu;
	DO start_interval = 0 to 29 by 1; /*This will generate our observations covering the first 30 days"*/

		date = discharge_date + start_interval; /*Find the date for the start of the interval*/
		intv_age=intck('year',birthdate,date,"c"); /*Calculate age at the start of the interval*/

		long_cens_nostart = 0; /*No one in this time interval can be censored for not starting treatment*/

		IF Cens_followup > (start_interval + 1) THEN DO; /*If someone's censored follow-up is greater than the start of the interval plus the length of the interval...*/
			long_outcome = 0; /*They did not have an outcome during the interval*/
			long_cens_startearly = 0; /*They didn't get censored because of starting the treatment early in that interval*/
			interval_fu = 1; /*They should get credit for the day of person-time in the interval*/
			end_interval = start_interval + 1; /*The interval will end after 1 day*/
		END;

		ELSE IF Cens_followup = (start_interval + 1) THEN DO; /*If someone was followed until the end of an interval*/
			long_outcome = cens_outcome; /*They experienced their outcome*/
			long_cens_startearly = 0; /*They didn't get censored due to starting treatment early in that interval*/
			interval_fu = 1; /*They should get credit for the followup time in the interval*/
			end_interval = start_interval + 1; /*The interval ends at their outcome time;*/
		END;

		ELSE IF Cens_followup = start_interval THEN DO; /*All we have left will be the people who were censored for starting early...*/
			long_outcome = 0; /*They didn't have the outcome*/
			long_cens_startearly = cens_startearly; /*They should get the correct reason for censoring*/
			interval_fu = 0; /*They shouldn't get credit for the person-time in the interval*/
			end_interval = start_interval; /*For them, the interval starts and stops on the same day*/
		END;

		OUTPUT;

		IF (Cens_followup = (start_interval + 1) AND cens_startearly = 0) OR Cens_followup = start_interval THEN GOTO thend; /*Letting people out of the loop while allowing people who were censored for starting early to generate an extra observation with 0 followup*/

	END; /*Ending the DO loop for the first 30 days of observations*/

	start_interval = 30; /*Now we have to create the interval covering day 30 to 90*/

	date = discharge_date + start_interval; /*Find the date for the start of the interval*/
	intv_age=intck('year',birthdate,date,"c"); /*Calculate age at the start of the interval*/

	long_cens_startearly = 0; /*No one is getting censored for starting early at this point*/

	long_cens_nostart = 0; /*No one is getting censored for not starting during the interval*/

	IF Cens_followup > 90 THEN DO; /*If someone's censored follow-up is greater than 90 days...*/
		long_outcome = 0; /*They didn't experience the outcome during the interval*/
		interval_fu = 60; /*They should get credit for all 60 days during the interval*/
		end_interval = 90; /*The interval will end after 90 days*/
	END;

	ELSE IF Cens_followup <= 90 THEN DO; /*Otherwise, if people were followed less than 90 days...*/
		long_outcome = cens_outcome; /*They should get the value of the outcome from the "short" data set*/
		interval_fu = Cens_followup - 30; /*They should get credit for however long we followed them*/
		end_interval = Cens_followup; /*Their interval will end after their followup duration*/
	END;

	OUTPUT;

	IF Cens_followup <= 90 AND cens_nostart = 0 THEN GOTO thend; /*Getting people who have finished follow-up but were not censored due to not starting out of the long data set generation process*/

	start_interval = 90;

	date = discharge_date + start_interval; /*Find the date for the start of the interval*/
	intv_age=intck('year',birthdate,date,"c"); /*Calculate age at the start of the interval*/

	long_cens_startearly = 0; /*No observations are getting censored for starting early at this point*/
	
	IF cens_nostart = 1 THEN DO; /*First, we should create observations for the people censored due to not starting at day 90*/
		long_outcome = 0; /*They shouldn't be counted as experiencing the outcome during this interval*/
		long_cens_nostart = 1; /*They were censored due to not starting treatment*/
		interval_fu = 0; /*They should get 0 follow-up*/
		end_interval = 90; /*They shouldn't be followed past day 90*/
		recentstart = 1; /*This is for convenience and to signify that these people's hypothetical treatment regimen would involve starting at day 90, rather than earlier*/
	END;

	ELSE IF cens_nostart = 0 THEN DO; /*We also need observations who started between day 30 and 90*/
				
		IF RX_start >= 83 THEN recentstart = 1; /*We need to create a "recent start" variable to ensure we only upweight those who started near the end of the interval. Here, we are requiring them to start within the last 7 days*/
		ELSE recentstart = 0;

			IF Cens_followup > 180 THEN DO; /*What if they were followed for more than 180 days?*/
				long_outcome = 0; /*They didn't have the outcome.*/
				long_cens_nostart = 0; /*They were not censored due to not starting treatment*/
				interval_fu = 90; /*This interval covers 90 days*/
				end_interval = 180; /*Their follow-up ends at day 180*/
			END;

			ELSE IF Cens_followup <= 180 THEN DO; /*What if their follow-up ended in this period?*/
				long_outcome = cens_outcome; /*They should receive their true outcome*/;
				long_cens_nostart = 0; /*They were not censored due to not starting treatment*/
				interval_fu = Cens_followup - 90; /*This interval covers as many days as they had past 90*/
				end_interval = Cens_followup; /*Their interval ends at their follow-up time*/
			END;

		END; /*Ending the loop for cens_nostart = 0*/	


	OUTPUT;

	thend: ; /*This is just a label to get people out of all the DO loops when they are finished*/
RUN;

/*First, let's split this long data set into 4: one for the first chunk of time where censoring is the result of starting early,
one in the middle where there is no informative censoring, one at the end where the informative censoring is all due to not starting including only recent starters, and one for
the people initiated before day 83.*/

DATA first_30_days day_30_to_90 day_90_plus day_90_plus_not_recent;
	SET long_trt_30_90;
	IF start_interval <= 29 THEN OUTPUT first_30_days; /*The first data set will have all of the observations covering time 0 to 30.*/
	ELSE IF start_interval = 30 THEN OUTPUT day_30_to_90; /*The second has the day 30-90 observations.*/
	ELSE IF start_interval = 90 AND recentstart = 1 THEN OUTPUT day_90_plus; /*The third has the recent starter observations*/
	ELSE IF start_interval = 90 THEN OUTPUT day_90_plus_not_recent; /*The last has everyone who started before day 83*/
RUN;

/*We need to create cumulative weights for the first 30 days of follow-up. First, we fit a pooled multivariable logistic model including linear and quadratic terms for the start of the interval*/

PROC GENMOD DATA=first_30_days;
	CLASS sex renal / param=ref;
	MODEL long_cens_startearly = intv_age sex renal start_interval start_interval*start_interval/ link=logit dist=binomial; 
	OUTPUT OUT=first_30_for_IPCW prob=FU_uncens;
RUN;

/*Next, we generate our weights for these first intervals*/

DATA first_30_weights;
	SET first_30_for_IPCW;
	Interval_IPCW = 1/FU_uncens;
RUN;


/*In the second data set, everyone's person-time gets an interval weight of 1, because no one is censored by design during this interval.*/

DATA day_30_to_90_weights;
	SET day_30_to_90;
	Interval_IPCW = 1;
RUN;

/*Next, we need to create weights for the final interval in the same way we created weights for the trt_0_30 and trt_0_90 analyses. Again, we should only fit among those who started
near the end of the interval unless we are comfortable ignoring that potential outcomes under treatment may vary over time.*/

PROC GENMOD DATA=day_90_plus;
	WHERE recentstart = 1;
	CLASS sex renal / param=ref;
	MODEL long_cens_nostart = intv_age sex renal / link=logit dist=binomial; 
	OUTPUT OUT=day_90_plus_IPCW prob=FU_uncens;
RUN;

DATA day_90_plus_weights;
	SET day_90_plus_IPCW day_90_plus_not_recent;
	BY ID start_interval;

	IF recentstart = 1 AND long_cens_nostart = 0 THEN Interval_IPCW = 1/FU_uncens; /*Those who recently started treatment should be assigned IPCW based on their predicted probability*/

	ELSE IF recentstart = 0 AND long_cens_nostart = 0 THEN Interval_IPCW = 1; /*Those who did not recently start treatment but started in the interval should receive IPCW of 1*/

	ELSE Interval_ipcw = 0; /*All other observations should get weights of 0*/

RUN;

/*And finally we can calculate our cumulative IPCW*/

DATA weights_trt_30_90;
	SET first_30_weights day_30_to_90_weights day_90_plus_weights ;
	BY ID start_interval;
	RETAIN Cumulative_IPCW; /*Allow this variable to cross observations*/
	IF first.ID THEN Cumulative_IPCW = t0IPCW*Interval_IPCW; /*For the first observation of each patient, initiate their cumulative IPCW as their t0IPCW then multiply by their IPCW for the current interval*/
	ELSE DO;
		Last_cumulative_IPCW = Cumulative_IPCW; /*Saving the last cumulative IPCW for everyone is helpful for calculating SMDs*/
		Cumulative_IPCW = Interval_IPCW * Cumulative_IPCW; /*For every subsequent observation, their cumulative IPCW is equal to the product of their last cumulative IPCW and the interval IPCW*/
	END;
RUN;

/*And now we can save our final analytic dataset using the observations with weights other 0 and save the rest in another data set for later diagnostics of the IPCW performance*/

DATA desynpuf.wted_trt_30_90 desynpuf.zeros_30_90;
	SET weights_trt_30_90;
	BY ID start_interval;
	IF cumulative_IPCW ^= 0 AND interval_fu ^= 0 THEN OUTPUT desynpuf.wted_trt_30_90;
	ELSE OUTPUT desynpuf.zeros_30_90;
RUN;
