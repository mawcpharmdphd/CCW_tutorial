LIBNAME desynpuf "C:\Users\michael.webster\Documents\CCW_tutorial\Stored_data";

/*CLONING AND CENSORING*/

/*First, let's load in our data set*/

DATA baseccwcohort;
	SET desynpuf.baseccwcohort;
RUN;

/*CLONING*/

/*Next, let's clone individuals into four different data sets representing four potential treatment regimens:
Treatment 0 to 30 days after MI (trt0_30)
Treatment 0 to 90 days after MI (trt0_90)
Treatment 30 to 90 days after MI (trt30_90)
No treatment after MI (notrt), separate from the main regimens in the manuscript*/

DATA trt0_30 trt0_90 trt30_90 notrt; 
/*Each of these data sets represents a potential clone. We will be cloning everyone into all groups, even those they are incompatible
with at baseline, though we will be setting a "t0-censoring" flag before cloning. An alternative approach is to clone people only into
groups they are compatible with at baseline, though you will need to deal with potential confounding separately during your final comparative
analysis if you choose to do this*/
	SET baseccwcohort;
	IF RX_start = 0 THEN DO;
		t0censnotrt = 1;
		t0cens30_90 = 1;
	END;
	ELSE DO;
		t0censnotrt=0;
		t0cens30_90=0;
	END;
	OUTPUT trt0_30;
	OUTPUT trt0_90;
	OUTPUT trt30_90;
	OUTPUT notrt;
RUN;

/*Now we need to create censoring within the cohorts.*/
		
/*Let's do the trt0_30 cohort first. We need to censor everyone who doesn't start by day 30 at day 30*/

DATA desynpuf.trt_0_30_with_cens;
	SET trt0_30;
	/*First, let's sort out the people with less than 30 days of followup. They should use their original values*/
	IF followup < 31 THEN DO;
		Cens_followup = followup;
		Cens_outcome = outcome;
		Cens_nostart = 0; /*We also want to make it clear this is a separate censoring mechanism if we decided to weight for it*/
	END;
	ELSE IF RX_start = . OR RX_start > 30 THEN DO; /*This will get everyone who doesn't start or starts after day 30 but survives to that day*/
		Cens_followup = 30;
		Cens_outcome = 0;
		Cens_nostart = 1; /*This flag makes it clear they were censored for not starting*/
	END;
	ELSE IF RX_start < 31 THEN DO; /*Now we assign the people who started within the first 30 days their original values*/
		Cens_followup = followup;
		Cens_outcome = outcome;
		Cens_nostart = 0;
	END;
RUN;

/*And next, the most similar cohort, trt0_90. We need to change "91" or "90" to "31" or "30".*/

DATA desynpuf.trt_0_90_with_cens;
	SET trt0_90;
	/*First, let's sort out the people with less than 30 days of followup. They should use their original values*/
	IF followup < 91 THEN DO;
		Cens_followup = followup;
		Cens_outcome = outcome;
		Cens_nostart = 0; /*We also want to make it clear this is a separate censoring mechanism if we decided to weight for it*/
	END;
	ELSE IF RX_start = . OR RX_start > 90 THEN DO; /*This will get everyone who doesn't start or starts after day 30 but survives to that day*/
		Cens_followup = 90;
		Cens_outcome = 0;
		Cens_nostart = 1; /*This flag makes it clear they were censored for not starting*/
	END;
	ELSE IF RX_start < 91 THEN DO; /*Now we assign the people who started within the first 30 days their original values*/
		Cens_followup = followup;
		Cens_outcome = outcome;
		Cens_nostart = 0;
	END;
RUN;

/*Finally, the last cohort covered in the tutorial: trt30_90. Here, we need to do two forms of censoring: one for people who start from day 0 to 30, and another
for people who do not start by day 90.*/

DATA desynpuf.trt_30_90_with_cens;
	SET trt30_90;
	/*First, let's deal with censoring the people who start within the first 30 days.*/
	IF RX_start < 30 AND RX_start ^= . THEN DO;
		IF followup >= RX_start THEN DO; /*If their start of treatment is on or after their end of follow-up (entirely possible for some outcomes), they should be censored*/
			Cens_followup = RX_start;
			Cens_outcome = 0;
			Cens_startearly = 1;
			Cens_nostart = 0;
		END;
		ELSE DO; /*Otherwise, they should have their outcome at the original follow-up time*/
			Cens_followup = followup;
			Cens_outcome = outcome;
			Cens_startearly = 0;
			Cens_nostart = 0;
		END;
	END;
	/*Next, we need to deal with the people who have less than 91 days of followup. Their followup should be unchanged and they should not get a flag for either censoring reason*/
	ELSE IF followup < 91 THEN DO;
		Cens_followup = followup;
		Cens_outcome = outcome;
		Cens_startearly = 0;
		Cens_nostart = 0;
	END;
	/*Next, let's deal with people who never start or start after day 90. Their censoring date should be day 90, and their reason should be not starting*/
	ELSE IF RX_start = . OR RX_start > 90 THEN DO;
		Cens_followup = 90;
		Cens_outcome = 0;
		Cens_startearly = 0;
		Cens_nostart = 1;
	END;
	ELSE DO; /*Now, we need to create the follow-up for people who started an RX within the window*/
		Cens_followup = followup;
		Cens_outcome = outcome;
		Cens_startearly = 0;
		Cens_nostart = 0;
	END;
RUN;

/*Additionally, the notrt cohort for those interested. These patients need to have their follow-up censored
on the date they start an RX.*/

DATA desynpuf.notrt_with_cens;
	SET notrt;
	IF RX_start = . THEN DO; /*Everyone who is missing an RX start date uses their old outcome and follow-up information*/
		Cens_followup = followup;
		Cens_outcome = outcome;
		Cens_start = 0; /*We want to make sure it's clear that anyone who was censored here was not censored for starting an RX*/
	END;
	ELSE IF RX_start ^= . AND followup >= RX_start THEN DO; /*Everyone with an RX_start time on or before their followup date should be censored*/
		Cens_followup = RX_start;
		Cens_outcome = 0; /*We set their death to 0, since they were censored. In some cases, you might want to make a special exception if they die the same die they are censored*/
		Cens_start = 1; /*We want this flag to make sure it's clear they were censored for starting*/
	END;
	ELSE IF RX_start ^= . AND followup < RX_start THEN DO; /*Everyone with an earlier follow-up date than RX start should get their original followup*/
		Cens_followup = followup;
		Cens_outcome = outcome;
		Cens_start = 0;
	END;
RUN;
