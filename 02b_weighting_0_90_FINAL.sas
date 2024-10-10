LIBNAME desynpuf "C:\Users\michael.webster\Documents\CCW_tutorial\Stored_data";


/*CENSORING WEIGHTS FOR THE 0-90 DAY WINDOW*/

/*This is virtually identical to the trt_0_30 data set.

This set has two forms of censoring: general loss to follow-up and censoring because patients did not start taking any medications by day 90. We will
be assuming that general loss to follow-up is random, meaning we only have potential selection bias from "cens_nostart=1" at day 90. Handling differential loss to
follow-up requires a separate set of weights.*/

/*First, let's make the data set long. We only need two observations: one covering day 0-90 (where everyone contributes) and one covering day 90-180 (where
only those who initiated on or prior to day 90 contribute).*/


DATA long_trt_0_90;
	SET desynpuf.trt_0_90_with_cens;
	DO start_interval = 0 to 90 by 90; /*This will generate up to 2 observations per individuals with our desired start timings*/

		date = discharge_date + start_interval; /*Find the date for the start of the interval*/
		intv_age=intck('year',birthdate,date,"c"); /*Calculate age at the start of the interval*/

		IF start_interval = 0 THEN DO; /*The interval starting at time 0...*/

			IF Cens_followup > 90 THEN DO; /*If someone's censored follow-up is greater than 90 days...*/
				long_outcome = 0; /*They didn't experience the outcome during the interval*/
				long_cens_nostart = 0; /*They didn't get censored during the interval*/
				interval_fu = 90; /*They should get credit for all 90 days during the interval*/
				end_interval = 90; /*The interval will end after 90 days*/
			END;

			ELSE IF Cens_followup <= 90 THEN DO; /*Otherwise, if people were followed less than 90 days...*/
				long_outcome = cens_outcome; /*They should get the value of the outcome from the "short" data set*/
				long_cens_nostart = 0; /*They definitely weren't censored due to not starting treatment*/
				interval_fu = Cens_followup; /*They should get credit for however long we followed them*/
				end_interval = Cens_followup; /*Their interval will end after their followup duration*/
			END;

			OUTPUT;
			IF Cens_followup <= 90 AND cens_nostart = 0 THEN GOTO thend; /*Making sure nobody who has less than 90 days of follow-up gets a second observation*/

		END; /*Ending the loop for start_interval = 0*/

		ELSE IF start_interval = 90 THEN DO; /*The second interval involves more logical conditions*/
			
			IF cens_nostart = 1 THEN DO; /*First, we should create observations for the people censored due to not starting at day 90*/
				long_outcome = 0; /*They shouldn't be counted as having the outcome during this interval*/
				long_cens_nostart = 1; /*They were censored due to not starting treatment*/
				interval_fu = 0; /*They should get 0 follow-up*/
				end_interval = 90; /*They shouldn't be followed past day 90*/
				recentstart = 1; /*This is for convenience and to signify that these people's hypothetical treatment regimen would involve starting at day 90, rather than earlier*/
			END;

			ELSE IF cens_nostart = 0 THEN DO; /*We also need observations for those who did start by day 90*/
				
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

		END; /*Ending the loop for start_interval = 90*/

	END;

	thend: ; /*This is just a label to help get the people who should only have 1 observation out of the DO loop*/
RUN;

/*First, let's split this long data set into 2.*/

DATA first_90_days other_days other_days_not_recent;
	SET long_trt_0_90;
	IF start_interval = 0 THEN OUTPUT first_90_days; /*The first data set will have all of the observations covering time 0 to 90.*/
	ELSE IF start_interval = 90 AND recentstart = 1 THEN OUTPUT other_days; /*The second has all the observations we will be using to fit our censoring weights*/
	ELSE OUTPUT other_days_not_recent;
RUN;

/*In the first data set, the weights are 1 for every observation because all censoring is random.*/

DATA first_90_weights;
	SET first_90_days;
	Cumulative_IPCW = 1;
RUN;

/*In the second data set, we need to fit a logistic regression model predicting the probability of being uncensored at the start of the interval among those with recentstart=1
(i.e., the people who were censored at day 90 and the people who started treatment from day 83 to day 90.*/

PROC GENMOD DATA=other_days;
	CLASS sex renal / param=ref;
	MODEL long_cens_nostart = intv_age sex renal / link=logit dist=binomial; 
	OUTPUT OUT=other_days_for_IPCW prob=FU_uncens;
RUN;

/*We can now generate our IPCWs*/

DATA other_days_weights;
	SET other_days_for_IPCW other_days_not_recent;

	IF recentstart = 1 AND long_cens_nostart = 0 THEN Cumulative_IPCW = 1/FU_uncens; /*Those who recently started treatment should be assigned cumulative IPCW based on their predicted probability*/

	ELSE IF recentstart = 0 AND long_cens_nostart = 0 THEN Cumulative_IPCW = 1; /*Thosefrom the "didn't start treatment recently" data set should receive IPCW of 1*/

	ELSE cumulative_ipcw = 0; /*All other observations should get weights of 0*/

RUN;

/*And now we can save our final analytic dataset, as well as a data set that includes observations we can use to examine the performance of IPCW.*/

DATA desynpuf.wted_trt_0_90 desynpuf.zeros_0_90;
	SET other_days_weights first_90_weights;
	BY ID start_interval;
	IF cumulative_IPCW ^= 0 THEN OUTPUT desynpuf.wted_trt_0_90;
	ELSE OUTPUT desynpuf.zeros_0_90;
RUN;
