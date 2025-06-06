# SMART Tournament Outcome Evalation Code

# program average predicted PAI
# average overall response
# average overall benefit
# half slide on the window 254, slide over 127, doesn't count tails twice. 
# standard error of estimate of LI and HI in each bin.
# harmonic mean in each bin
# decompose HI and LI dots, standard error bars
# binomial not normal distribution
# how does PSM weigh in...
# different models will have different #s of observations in each window
# person on left 10 only participates once, person in middle 30 times

# create dots

# try with seven bins
# try with sliding window



# temporary code for setting path
# insert your path to the data here
setwd("~/research/PRN project/SMART_T1")
setwd("C:/Users/Zach/PRN project/SMART_T1")

# The outcomes will be evaluated exactly the same for both sets of models: standard and enriched variables.

# 1. Data splitting as an example

# in order to ensure that I have accurately instantiated your models, I ask that you run this code and send me the results

# what I have done is split the training sample  (N=~4000) into 50% vs. 50% 

# should I do this, or should I use the 4000 as the training sample and the 2000 as the test sample (as it's just an example)
# we will consider the first ~2000 as the training sample, and the final ~2000 as the held-out test sample

# I will generate predictions for the final 2000 based on your models, and evaluate those predictions in the exact way

# then I will evaluate each of your models in the true 2000 hold-out, and the 1000 hold-out, 30K hold outs.

############################# read in data ############################# 
X1 = read.csv("A_imputed_standard_SMART_training_dataset.csv")
X2 = read.csv("B_imputed_enriched_SMART_training_dataset.csv")
X3 = read.csv("C_non_imputed_standard_SMART_training_dataset.csv")
X4 = read.csv("D_non_imputed_enriched_SMART_training_dataset.csv")
############################# read in data ############################# 


############################# read in test and validation samples ############################# 
# X5 = read.csv("test_sample_leeds_holdout.csv")
# X6 = read.csv("validation_sample_cumbria.csv")
# X7 = read.csv("generalizability_sample1_PRN.csv")
# X8 = read.csv("generalizability_sample2_london.csv") # to be confirmed

# When we evaluate the test samples we will bind them to the training data in order to impute missing data
############################# read in test and validation samples ############################# 

############################# decide which data to use ############################# 
# we will demonstrate using the standard non-imputed dataset
# X = X3
X = X2 # temporary code that will use the already imputed data
############################# decide which data to use ############################# 

############################# set variable names ############################# 
# first we have to show how the outcome metric is calculated for the test dataset
outcome_var_names = c("Treatment_pathway",
                      "PHQ9_first",
                      "PHQ9_last",
                      "GAD7_first",
                      "GAD7_last",
                      "Step2_sessions",
                      "Step2_first_PHQ9",
                      "Step2_last_PHQ9",
                      "Step2_first_GAD7",
                      "Step2_last_GAD7",
                      "Step3_sessions",
                      "Step3_first_PHQ9",
                      "Step3_last_PHQ9",
                      "Step3_first_GAD7",
                      "Step3_last_GAD7",
                      "Phobia_Q1_last",
                      "Phobia_Q2_last",
                      "Phobia_Q3_last")

all_vars_for_standard = c("CaseLink",
                          "Treatment_pathway",
                          "Discharge_code",
                          "Contacts",
                          "Age",
                          "Gender",
                          "Ethnicity_IAPT",
                          "Ethnicity_ONS",
                          "Diagnosis",
                          "LTC_binary",
                          "Long_Term_Condition_Type",
                          "Employment_IAPT_first",
                          "Employment_IAPT_last",
                          "Disability_status",
                          "Disability_type",
                          "Number_of_disabilities",
                          "PHQ9_first",
                          "PHQ9_last",
                          "GAD7_first",                       
                          "GAD7_last",
                          "WSAS_first",
                          "WSAS_last",
                          "Medication_IAPT_first",
                          "Medication_IAPT_last",
                          "Step2_sessions",
                          "Step2_first_PHQ9",
                          "Step2_last_PHQ9",
                          "Step2_first_GAD7",
                          "Step2_last_GAD7",
                          "Step2_first_WSAS",
                          "Step2_last_WSAS",
                          "Step3_sessions",
                          "Step3_first_PHQ9",
                          "Step3_last_PHQ9",
                          "Step3_first_GAD7",
                          "Step3_last_GAD7",
                          "Step3_first_WSAS",
                          "Step3_last_WSAS",
                          "y",
                          "PHQ9_LOCF",
                          "GAD7_LOCF",
                          "Phobia_Q1_first",
                          "Phobia_Q2_first",
                          "Phobia_Q3_first",
                          "Phobia_Q1_last",
                          "Phobia_Q2_last",
                          "Phobia_Q3_last")

all_vars_for_standard_imputation = c("Age",
                                     "Gender",
                                     "Ethnicity_IAPT",
                                     "Ethnicity_ONS",
                                     "Diagnosis",
                                     "LTC_binary",
                                     "Long_Term_Condition_Type",
                                     "Employment_IAPT_first",
                                     "Disability_status",
                                     "Disability_type",
                                     "Number_of_disabilities",
                                     "PHQ9_first",
                                     "GAD7_first",                       
                                     "WSAS_first",
                                     "Medication_IAPT_first",
                                     "Phobia_Q1_first",
                                     "Phobia_Q2_first",
                                     "Phobia_Q3_first")
############################# set variable names ############################# 

############################# data cleaning ############################# 
# need to fix binary and categorical variables prior to imputation
# right now we aren't doing anything special with non categorical/binary variables 
# i.e., we are treating all as continuous and not distinguishing rank/decimal or count variables
X$Gender = as.factor(X$Gender)
X$Ethnicity_IAPT = as.factor(X$Ethnicity_IAPT)
X$Ethnicity_ONS = as.factor(X$Ethnicity_ONS)
X$Diagnosis = as.factor(X$Diagnosis)
X$LTC_binary = as.factor(X$LTC_binary)
X$Long_Term_Condition_Type = as.factor(X$Long_Term_Condition_Type)
X$Employment_IAPT_first = as.factor(X$Employment_IAPT_first)
X$Disability_status = as.factor(X$Disability_status)
X$Disability_type = as.factor(X$Disability_type)
X$Family_history = as.factor(X$Family_history)
X$Previous_treatment_binary = as.factor(X$Previous_treatment_binary)
X$Medication_IAPT_first = as.factor(X$Medication_IAPT_first)

# we will perform other data cleaning steps specific to the test samples that aren't specified here
# such as adjusting variable names, removing extra variables, or collapsing categorical variables
############################# data cleaning ############################# 

############################# calculate SMART outcome variable ############################# 
# now calculate SMART outcome variable for everyone (except those below caseness on both at start)
# note that this is unnecessary as the variable is already on the dataset, but is included here to show you how it was calculated
# create a vector to save the new outcome
SMART_outcome = rep(NA,dim(X)[1])
# create two vectors to save final PHQ9 and GAD7 scores
SMART_final_PHQ9 = rep(NA,dim(X)[1])
SMART_final_GAD7 = rep(NA,dim(X)[1])
# create two additional vectors to record where the PHQ9s and GAD7s came from
SMART_final_PHQ9_origin = SMART_final_PHQ9
SMART_final_GAD7_origin = SMART_final_GAD7

# create four additional vectors to record criteria for the PHQ9s and GAD7s in the smart outcome
PHQ9_a = SMART_outcome
PHQ9_b = SMART_outcome
GAD7_a = SMART_outcome
GAD7_b = SMART_outcome

for (i in 1:nrow(X)){
  # grab this patient
  temp_patient = X[i,]
  # start with those who were in LI only
  if (temp_patient$Treatment_pathway==2){
    # first calculate their end PHQ9 for the step they started in
    # if they have a PHQ9_last then use this
    if (!is.na(temp_patient$PHQ9_last)){
      SMART_final_PHQ9[i]=temp_patient$PHQ9_last
      SMART_final_PHQ9_origin[i] = 4 # this indicates the best type of outcome data
    } 
    # if there's no PHQ9_last but there is a Step2_last_PHQ9 then use this
    else if (!is.na(temp_patient$Step2_last_PHQ9)){
      SMART_final_PHQ9[i]=temp_patient$Step2_last_PHQ9
      SMART_final_PHQ9_origin[i] = 3 # second best outcome data
    }
    # if there's no PHQ9_last or Step2_last_PHQ9 but there is a Step2_first_PHQ9 then use this
    else if (!is.na(temp_patient$Step2_first_PHQ9)){
      SMART_final_PHQ9[i]=temp_patient$Step2_first_PHQ9
      SMART_final_PHQ9_origin[i] = 2 # third best outcome data
    }
    # if there's no PHQ9_last, Step2_last_PHQ9 or Step2_first_PHQ9 but there is a PHQ9_first then use this
    else if (!is.na(temp_patient$PHQ9_first)){
      SMART_final_PHQ9[i]=temp_patient$PHQ9_first
      SMART_final_PHQ9_origin[i] = 1 # fourth best type of outcome data
    }
    # this situation won't occur for the training data because no one is missing baseline PHQ9
    else {
      warning(paste("patient row",i,"missing all relevant PHQ9 data"))
      SMART_final_PHQ9_origin[i] = 0 # no outcome data
    }
    # now calculate GAD7
    # if they have a GAD7_last then use this
    if (!is.na(temp_patient$GAD7_last)){
      SMART_final_GAD7[i]=temp_patient$GAD7_last
      SMART_final_GAD7_origin[i] = 4
    } 
    # if there's no GAD7_last but there is a Step2_last_GAD7 then use this
    else if (!is.na(temp_patient$Step2_last_GAD7)){
      SMART_final_GAD7[i]=temp_patient$Step2_last_GAD7
      SMART_final_GAD7_origin[i] = 3
    }
    # if there's no GAD7_last or Step2_last_GAD7 but there is a Step2_first_GAD7 then use this
    else if (!is.na(temp_patient$Step2_first_GAD7)){
      SMART_final_GAD7[i]=temp_patient$Step2_first_GAD7
      SMART_final_GAD7_origin[i] = 2
    }
    # if there's no GAD7_last, Step2_last_GAD7 or Step2_first_GAD7 but there is a GAD7_first then use this
    else if (!is.na(temp_patient$GAD7_first)){
      SMART_final_GAD7[i]=temp_patient$GAD7_first
      SMART_final_GAD7_origin[i] = 1
    }
    # this situation won't occur for the training data because no one is missing baseline GAD7
    else {
      warning(paste("patient row",i,"missing all relevant GAD7 data"))
      SMART_final_GAD7_origin[i] = 0
    }
  } # here is where code for LI-only (Treatment_pathway==2) ends
  
  # now calculate for those who were in HI only
  else if (temp_patient$Treatment_pathway==3){
    # first calculate their end PHQ9 for the step they started in
    # if they have a PHQ9_last then use this
    if (!is.na(temp_patient$PHQ9_last)){
      SMART_final_PHQ9[i]=temp_patient$PHQ9_last
      SMART_final_PHQ9_origin[i] = 4
    } 
    # if there's no PHQ9_last but there is a Step3_last_PHQ9 then use this
    else if (!is.na(temp_patient$Step3_last_PHQ9)){
      SMART_final_PHQ9[i]=temp_patient$Step3_last_PHQ9
      SMART_final_PHQ9_origin[i] = 3
    }
    # if there's no PHQ9_last or Step3_last_PHQ9 but there is a Step3_first_PHQ9 then use this
    else if (!is.na(temp_patient$Step3_first_PHQ9)){
      SMART_final_PHQ9[i]=temp_patient$Step3_first_PHQ9
      SMART_final_PHQ9_origin[i] = 2
    }
    # if there's no PHQ9_last, Step3_last_PHQ9 or Step3_first_PHQ9 but there is a PHQ9_first then use this
    else if (!is.na(temp_patient$PHQ9_first)){
      SMART_final_PHQ9[i]=temp_patient$PHQ9_first
      SMART_final_PHQ9_origin[i] = 1
    }
    # this situation won't occur for the training data because no one is missing baseline PHQ9
    else {
      warning(paste("patient row",i,"missing all relevant PHQ9 data"))
      SMART_final_PHQ9_origin[i] = 0
    }
    # now calculate GAD7
    # if they have a GAD7_last then use this
    if (!is.na(temp_patient$GAD7_last)){
      SMART_final_GAD7[i]=temp_patient$GAD7_last
      SMART_final_GAD7_origin[i] = 4
    } 
    # if there's no GAD7_last but there is a Step3_last_GAD7 then use this
    else if (!is.na(temp_patient$Step3_last_GAD7)){
      SMART_final_GAD7[i]=temp_patient$Step3_last_GAD7
      SMART_final_GAD7_origin[i] = 3
    }
    # if there's no GAD7_last or Step3_last_GAD7 but there is a Step3_first_GAD7 then use this
    else if (!is.na(temp_patient$Step3_first_GAD7)){
      SMART_final_GAD7[i]=temp_patient$Step3_first_GAD7
      SMART_final_GAD7_origin[i] = 2
    }
    # if there's no GAD7_last, Step3_last_GAD7 or Step3_first_GAD7 but there is a GAD7_first then use this
    else if (!is.na(temp_patient$GAD7_first)){
      SMART_final_GAD7[i]=temp_patient$GAD7_first
      SMART_final_GAD7_origin[i] = 1
    }
    # this situation won't occur for the training data because no one is missing baseline GAD7
    else {
      warning(paste("patient row",i,"missing all relevant GAD7 data"))
      SMART_final_GAD7_origin[i] = 0
    }
  } # here is where code for HI-only (Treatment_pathway==3) ends
  
  # now calculate for those in LI-HI
  else if (temp_patient$Treatment_pathway==4){
    # first calculate their end PHQ9 for the step they started in (LI)
    # we cannot use PHQ9_last because this captures outcome after they've received Step3 (HI)
    # so their best outcome PHQ9 is their Step2_last_PHQ9
    # note that we cannot use Step3_first_PHQ9 because the average wait time between ending Step2 and starting
    # Step3 is 6 months in this sample
    if (!is.na(temp_patient$Step2_last_PHQ9)){
      SMART_final_PHQ9[i]=temp_patient$Step2_last_PHQ9
      SMART_final_PHQ9_origin[i] = 3
    } 
    # if there's no Step2_last_PHQ9 but there is a Step2_first_PHQ9 then use this
    else if (!is.na(temp_patient$Step2_first_PHQ9)){
      SMART_final_PHQ9[i]=temp_patient$Step2_first_PHQ9
      SMART_final_PHQ9_origin[i] = 2
    }
    # if there's no Step2_last_PHQ9 or Step2_first_PHQ9 but there is a PHQ9_first then use this
    else if (!is.na(temp_patient$PHQ9_first)){
      SMART_final_PHQ9[i]=temp_patient$PHQ9_first
      SMART_final_PHQ9_origin[i] = 1
    }
    # this situation won't occur for the training data because no one is missing baseline PHQ9
    else {
      warning(paste("patient row",i,"missing all relevant PHQ9 data"))
      SMART_final_PHQ9_origin[i] = 0
    }
    # now calculate GAD7
    if (!is.na(temp_patient$Step2_last_GAD7)){
      SMART_final_GAD7[i]=temp_patient$Step2_last_GAD7
      SMART_final_GAD7_origin[i] = 3
    } 
    # if there's no Step2_last_GAD7 but there is a Step2_first_GAD7 then use this
    else if (!is.na(temp_patient$Step2_first_GAD7)){
      SMART_final_GAD7[i]=temp_patient$Step2_first_GAD7
      SMART_final_GAD7_origin[i] = 2
    }
    # if there's no Step2_last_GAD7 or Step2_first_GAD7 but there is a GAD7_first then use this
    else if (!is.na(temp_patient$GAD7_first)){
      SMART_final_GAD7[i]=temp_patient$GAD7_first
      SMART_final_GAD7_origin[i] = 1
    }
    # this situation won't occur for the training data because no one is missing baseline GAD7
    else {
      warning(paste("patient row",i,"missing all relevant GAD7 data"))
      SMART_final_GAD7_origin[i] = 0
    }
  } # here is where code for LI-HI (Treatment_pathway==4) ends
  
  # now you have the LOCF PHQ9 and GAD7 scores
  
  # calculate PHQ9 and GAD7 change
  temp_PHQ9_change = temp_patient$PHQ9_first-SMART_final_PHQ9[i]
  temp_GAD7_change = temp_patient$GAD7_first-SMART_final_GAD7[i]
  
  # now calculate the SMART tournament outcome metric
  
  # first, calculate for those at/above caseness at assessment
  if (temp_patient$PHQ9_first>9&temp_patient$GAD7_first>7){
    # then they need one of these for PHQ9
    # below caseness post and reliable change
    temp_PHQa = SMART_final_PHQ9[i]<10 & temp_PHQ9_change>5
    # or above caseness post but at least 50% change
    temp_PHQb = SMART_final_PHQ9[i]>9 & (temp_PHQ9_change/temp_patient$PHQ9_first)>=.5
    # and they need one of these for GAD7
    # below caseness post and reliable change
    temp_GADa = SMART_final_GAD7[i]<8 & temp_GAD7_change>4
    # or above caseness post but at least 50% change
    temp_GADb = SMART_final_GAD7[i]>7 & (temp_GAD7_change/temp_patient$GAD7_first)>=.5
  }
  
  # now calculate for those only at caseness on PHQ9
  else if (temp_patient$PHQ9_first>9&temp_patient$GAD7_first<8){
    # then they need one of these for PHQ9
    # below caseness post and reliable change
    temp_PHQa = SMART_final_PHQ9[i]<10 & temp_PHQ9_change>5
    # or above caseness post but at least 50% change
    temp_PHQb = SMART_final_PHQ9[i]>9 & (temp_PHQ9_change/temp_patient$PHQ9_first)>=.5
    # and they need one of these for GAD7
    # below caseness
    temp_GADa = SMART_final_GAD7[i]<8
    # or above caseness post but less than reliable deterioration
    temp_GADb = SMART_final_GAD7[i]>7 & temp_GAD7_change>-5
  }
  
  # now calculate for those only at caseness on GAD7
  else if (temp_patient$PHQ9_first<10&temp_patient$GAD7_first>7){
    # then they need one of these for PHQ9
    # below caseness post
    temp_PHQa = SMART_final_PHQ9[i]<10
    # or above caseness post but less than reliable deterioration
    temp_PHQb = SMART_final_PHQ9[i]>9 & temp_PHQ9_change>-6
    # and they need one of these for GAD7
    # below caseness post and reliable change
    temp_GADa = SMART_final_GAD7[i]<8 & temp_GAD7_change>4
    # or above caseness post but at least 50% change
    temp_GADb = SMART_final_GAD7[i]>7 & (temp_GAD7_change/temp_patient$GAD7_first)>=.5
  }
  # for those below caseness on both, they get NAs for everything
  else{
    temp_PHQa = NA
    temp_PHQb = NA
    temp_GADa = NA
    temp_GADb = NA
  }
  # save their status on all 4 criteria
  PHQ9_a[i] = as.numeric(temp_PHQa)
  PHQ9_b[i] = as.numeric(temp_PHQb)
  GAD7_a[i] = as.numeric(temp_GADa)
  GAD7_b[i] = as.numeric(temp_GADb)
  
  # now use the criteria to get the final SMART outcome
  SMART_outcome[i]=as.numeric((temp_PHQa|temp_PHQb)&(temp_GADa|temp_GADb))
}


# now add outcome variable to the original, non-imputed dataset (which is now reduced in size)
X$y = SMART_outcome
# the new SMART_outcome metric is now available as the variable named "y"
############################# calculate SMART outcome variable ############################# 

############################# imputing missing baseline data ############################# 
# now impute missing baseline data using only the data that would be available at baseline 
# this section is commented out because we are using the already imputed data in this example
# library(missForest)

# for X_imputed_standard first
# grab only the data that would be available at baseline
# X_for_imputation_standard = subset(X,select=all_vars_for_standard_imputation)

# set.seed(1234) # this makes the imputation reproducible
# my_imputation_standard <- missForest(X_for_imputation_standard, variablewise = TRUE)
# warnings()

# grab the imputed data from the missForest output
# X_from_imputation_standard <- as.data.frame(my_imputation_standard$ximp)

# first we create X_imputed_standard from the original X
# X_imputed_standard = subset(X,select=all_vars_for_standard)

# we only want to replace the missing data for certain variables with the imputed versions
# X_imputed_standard$Age = X_from_imputation_standard$Age
# X_imputed_standard$Gender = X_from_imputation_standard$Gender
# X_imputed_standard$Ethnicity_IAPT = X_from_imputation_standard$Ethnicity_IAPT
# X_imputed_standard$Ethnicity_ONS = X_from_imputation_standard$Ethnicity_ONS
# X_imputed_standard$Diagnosis = X_from_imputation_standard$Diagnosis
# X_imputed_standard$LTC_binary = X_from_imputation_standard$LTC_binary
# X_imputed_standard$Long_Term_Condition_Type = X_from_imputation_standard$Long_Term_Condition_Type
# X_imputed_standard$Employment_IAPT_first = X_from_imputation_standard$Employment_IAPT_first
# X_imputed_standard$Disability_status = X_from_imputation_standard$Disability_status
# X_imputed_standard$Disability_type = X_from_imputation_standard$Disability_type
# X_imputed_standard$Number_of_disabilities = X_from_imputation_standard$Number_of_disabilities
# X_imputed_standard$PHQ9_first = X_from_imputation_standard$PHQ9_first
# X_imputed_standard$GAD7_first = X_from_imputation_standard$GAD7_first
# X_imputed_standard$WSAS_first = X_from_imputation_standard$WSAS_first
# X_imputed_standard$Medication_IAPT_first = X_from_imputation_standard$Medication_IAPT_first
# X_imputed_standard$Phobia_Q1_first = X_from_imputation_standard$Phobia_Q1_first
# X_imputed_standard$Phobia_Q2_first = X_from_imputation_standard$Phobia_Q2_first
# X_imputed_standard$Phobia_Q3_first = X_from_imputation_standard$Phobia_Q3_first

# temporary trick to skip imputation - we pulled the "imputed data" from X (because it's already done there)
X_imputed_standard = X

# add a binary tx variable to both relevant data structures
X_imputed_standard$tx = as.factor(as.numeric(X_imputed_standard$Treatment_pathway==3))
X$tx = as.factor(as.numeric(X$Treatment_pathway==3))
############################# imputing missing baseline data ############################# 

###### start set up a few binary variables for the purpose of this demonstration #######
# each team can choose to analyze categorical variables in whatever way they feel is best

# Medication_first_IAPT	1		Prescribed but not taking
# 2		Prescribed and taking
# 3		Not prescribed
# 9	missing	Not stated

X_imputed_standard$Medication_binary = as.factor(as.numeric(X_imputed_standard$Medication_IAPT_first==2))
# taking vs. not taking

# Employment_IAPT_first
# 1		Employed
# 2		Unemployed job seeker
# 3		Student
# 4		Long-term sick or disabled
# 5		Homemaker / carer
# 6		Unemployed, not seeking work
# 7		Voluntary work
# 8		Retired
# 9	  missing	Not stated
X_imputed_standard$Employment_binary = as.factor(as.numeric(X_imputed_standard$Employment_IAPT_first==1|X_imputed_standard$Employment_IAPT_first==3|
                                                              X_imputed_standard$Employment_IAPT_first==5|X_imputed_standard$Employment_IAPT_first==8))

X_imputed_standard$Ethnicity_binary = as.factor(as.numeric(X_imputed_standard$Ethnicity_ONS==1))

###### finish set up a few binary variables #######


############################# start split the training data for demonstration purposes ############################# 
# first we split the data 50-50 and take the first 50% as our training set and the second 50% of the data as our fake test set.
X_training = X_imputed_standard[1:(dim(X_imputed_standard)[1]/2),]
X_hold_out = X_imputed_standard[((dim(X_imputed_standard)[1]/2)+1):dim(X_imputed_standard)[1],]

# we only want to grab the hold_out and training patients who are at caseness on at least PHQ9 or GAD7 at start
X_training = X_training[X_training$PHQ9_first>9|X_training$GAD7_first>7,]
X_hold_out = X_hold_out[X_hold_out$PHQ9_first>9|X_hold_out$GAD7_first>7,]

# we have to do a temporary hack to remove two people with diagnosis 13 and 15 because those categories were dropped in the model building phase
X_training = X_training[!(X_training$Diagnosis==13|X_training$Diagnosis==15),]
X_hold_out = X_hold_out[!(X_hold_out$Diagnosis==13|X_hold_out$Diagnosis==15),]
############################# end split the training data for demonstration purposes ############################# 

############################# demonstrate evaluation in LI prognosis model #############################
# here we build 3 simple LI prognosis models to demonstrate the evaluation

# temporary list of predictors I will consider for this demonstration
prediction_variables = c("Age",
                         "Gender",
                         "Ethnicity_binary",
                         "Diagnosis",
                         "LTC_binary",
                         "Employment_binary",
                         "Disability_status",
                         "Number_of_disabilities",
                         "PHQ9_first",
                         "GAD7_first",                       
                         "WSAS_first",
                         "Medication_binary",
                         "Phobia_Q1_first",
                         "Phobia_Q2_first",
                         "Phobia_Q3_first")

# here we get all the training data necessary for building LI prognostic model
X_LI_only = X_training[(X_training$Treatment_pathway==2|X_training$Treatment_pathway==4)&(!is.na(X_training$y)),]
# this selects only the prediction variables we are considering.
X_LI_only_forEN = subset(X_LI_only,select=prediction_variables)
y_LI_only = as.factor(X_LI_only$y)

# grab the LI only patients from the hold-out
X_LI_only_hold_out = X_hold_out[X_hold_out$Treatment_pathway==2|X_hold_out$Treatment_pathway==4,]
y_LI_only_hold_out = X_LI_only_hold_out$y
# this selects only the prediction variables we are considering and y.
X_LI_only_hold_out = subset(X_LI_only_hold_out,select=c(prediction_variables,"y"))

# need to grab this without Y for the ENR model
X_LI_only_hold_out_forEN = subset(X_LI_only_hold_out,select=prediction_variables)

# model 1 is just baseline PHQ_9 severity in a simple logistic regression
LI_prognostic_model1 = glm(y ~ PHQ9_first, data = X_LI_only, family = "binomial")
summary(LI_prognostic_model1)

# model 2 in the split-halves training sample uses 6 predictors in a simple logistic regression, but not PHQ-9
LI_prognostic_model2 = glm(y ~ Phobia_Q1_first+Phobia_Q2_first+Phobia_Q3_first+Number_of_disabilities+Ethnicity_binary+WSAS_first, data = X_LI_only, family = "binomial")
summary(LI_prognostic_model2)

# model 3 is a logistic regression built using elastic net regularization 
# use elastic net to build this model
library(glmnet)
set.seed(123456)
fit_LI = glmnet(data.matrix(X_LI_only_forEN), y_LI_only, family="binomial", alpha=.5)
plot(fit_LI,label=TRUE)
LI_prognostic_model3 = cv.glmnet(data.matrix(X_LI_only_forEN), y_LI_only, family="binomial", alpha=.5)
plot(LI_prognostic_model3)
LI_prognostic_model3$lambda.min
print(coef(LI_prognostic_model3, s = "lambda.min"))
print(coef(LI_prognostic_model3, s = "lambda.1se"))
# in the split halves training sample this model, using 1se for lambda, includes age, employment, # disabilities, PHQ9_first, WSAS_first, and the 3 phobia Qs

# now we produce prognostic predictions for the hold-out
LI_prognosis_1 = predict(LI_prognostic_model1, X_LI_only_hold_out, type = "response")
LI_prognosis_2 = predict(LI_prognostic_model2, X_LI_only_hold_out, type = "response")
# to produce prognostic predictions for the hold-out with an elastic net model requires slightly different code
LI_prognosis_3 = predict(LI_prognostic_model3, newx=data.matrix(X_LI_only_hold_out_forEN), type = "response", s = "lambda.1se")
# have to convert to numeric vector form
LI_prognosis_3 = as.numeric(LI_prognosis_3)

# this shows the baserate in the training sample
mean(X_LI_only$y)
# [1] 0.3538353 - in the full training sample
# [1] 0.3642757 - in the split halves training sample

# this shows the baserate in the test sample
mean(y_LI_only_hold_out)
# [1] 0.3432836

# Please add your LI prognosis model here
# for now I'm just repeating LI prognosis model 3
LI_prognosis_4 = LI_prognosis_3

# we add a fifth set of predictions which are simply the baserate in the training sample
# LI_prognosis_5 = rep(0.3538353,dim(X_LI_only_hold_out)[1]) # using baserate in full training sample
LI_prognosis_5 = rep(0.3642757,dim(X_LI_only_hold_out)[1]) # using baserate in split halves training sample

plot(LI_prognosis_1,LI_prognosis_2,'p')
cor(LI_prognosis_1,LI_prognosis_2)

plot(LI_prognosis_1,LI_prognosis_3,'p')
cor(LI_prognosis_1,LI_prognosis_3)

plot(LI_prognosis_2,LI_prognosis_3,'p')
cor(LI_prognosis_2,LI_prognosis_3)

# calculate deviance statistic
# log-likelihood : sum from i=1 to N of [ Yi*ln(P(Yi))+(1-Yi)*ln(1-P(yi))]
# calculate brier score
# brier_score: (1/n)*sum(pi-oi)^2
log_likelihood_calculator_LI_1 = rep(NA,dim(X_LI_only_hold_out)[1])
brier_score_calculator_LI_1 = rep(NA,dim(X_LI_only_hold_out)[1])
log_likelihood_calculator_LI_2 = rep(NA,dim(X_LI_only_hold_out)[1])
brier_score_calculator_LI_2 = rep(NA,dim(X_LI_only_hold_out)[1])
log_likelihood_calculator_LI_3 = rep(NA,dim(X_LI_only_hold_out)[1])
brier_score_calculator_LI_3 = rep(NA,dim(X_LI_only_hold_out)[1])
log_likelihood_calculator_LI_4 = rep(NA,dim(X_LI_only_hold_out)[1])
brier_score_calculator_LI_4 = rep(NA,dim(X_LI_only_hold_out)[1])
log_likelihood_calculator_LI_5 = rep(NA,dim(X_LI_only_hold_out)[1])
brier_score_calculator_LI_5 = rep(NA,dim(X_LI_only_hold_out)[1])

for (i in 1:dim(X_LI_only_hold_out)[1]){
  log_likelihood_calculator_LI_1[i] = y_LI_only_hold_out[i]*log(LI_prognosis_1[i])+(1-y_LI_only_hold_out[i])*log(1-LI_prognosis_1[i])
  brier_score_calculator_LI_1[i] = (LI_prognosis_1[i]-y_LI_only_hold_out[i])^2
  log_likelihood_calculator_LI_2[i] = y_LI_only_hold_out[i]*log(LI_prognosis_2[i])+(1-y_LI_only_hold_out[i])*log(1-LI_prognosis_2[i])
  brier_score_calculator_LI_2[i] = (LI_prognosis_2[i]-y_LI_only_hold_out[i])^2
  log_likelihood_calculator_LI_3[i] = y_LI_only_hold_out[i]*log(LI_prognosis_3[i])+(1-y_LI_only_hold_out[i])*log(1-LI_prognosis_3[i])
  brier_score_calculator_LI_3[i] = (LI_prognosis_3[i]-y_LI_only_hold_out[i])^2
  log_likelihood_calculator_LI_4[i] = y_LI_only_hold_out[i]*log(LI_prognosis_4[i])+(1-y_LI_only_hold_out[i])*log(1-LI_prognosis_4[i])
  brier_score_calculator_LI_4[i] = (LI_prognosis_4[i]-y_LI_only_hold_out[i])^2
  log_likelihood_calculator_LI_5[i] = y_LI_only_hold_out[i]*log(LI_prognosis_5[i])+(1-y_LI_only_hold_out[i])*log(1-LI_prognosis_5[i])
  brier_score_calculator_LI_5[i] = (LI_prognosis_5[i]-y_LI_only_hold_out[i])^2
}

deviance_for_LI_model_1 = sum(log_likelihood_calculator_LI_1,na.rm=TRUE)
brier_score_for_LI_model_1 = (1/dim(X_LI_only_hold_out)[1])*sum(brier_score_calculator_LI_1,na.rm=TRUE)

deviance_for_LI_model_2 = sum(log_likelihood_calculator_LI_2,na.rm=TRUE)
brier_score_for_LI_model_2 = (1/dim(X_LI_only_hold_out)[1])*sum(brier_score_calculator_LI_2,na.rm=TRUE)

deviance_for_LI_model_3 = sum(log_likelihood_calculator_LI_3,na.rm=TRUE)
brier_score_for_LI_model_3 = (1/dim(X_LI_only_hold_out)[1])*sum(brier_score_calculator_LI_3,na.rm=TRUE)

deviance_for_LI_model_4 = sum(log_likelihood_calculator_LI_4,na.rm=TRUE)
brier_score_for_LI_model_4 = (1/dim(X_LI_only_hold_out)[1])*sum(brier_score_calculator_LI_4,na.rm=TRUE)

deviance_for_LI_model_5 = sum(log_likelihood_calculator_LI_5,na.rm=TRUE)
brier_score_for_LI_model_5 = (1/dim(X_LI_only_hold_out)[1])*sum(brier_score_calculator_LI_5,na.rm=TRUE)

print(round(deviance_for_LI_model_1,1))
print(round(brier_score_for_LI_model_1,3))
print(round(deviance_for_LI_model_2,1))
print(round(brier_score_for_LI_model_2,3))
print(round(deviance_for_LI_model_3,1))
print(round(brier_score_for_LI_model_3,3))
print(round(deviance_for_LI_model_4,1))
print(round(brier_score_for_LI_model_4,3))
print(round(deviance_for_LI_model_5,1))
print(round(brier_score_for_LI_model_5,3))

# bind all predictions together with observed y 
LI_predictions = data.frame(y_LI_only_hold_out,LI_prognosis_1,LI_prognosis_2,LI_prognosis_3,LI_prognosis_4,LI_prognosis_5)

# here is a program for calculating brier scores and log likelihood if you want to confirm that my code works
# library(scoring)
# LI_1_brierscore = brierscore(y_LI_only_hold_out~LI_prognosis_1, data=LI_predictions)
# mean(LI_1_brierscore)
# LI_2_brierscore = brierscore(y_LI_only_hold_out~LI_prognosis_2, data=LI_predictions)
# mean(LI_2_brierscore)
# LI_3_brierscore = brierscore(y_LI_only_hold_out~LI_prognosis_3, data=LI_predictions)
# mean(LI_3_brierscore)
# LI_4_brierscore = brierscore(y_LI_only_hold_out~LI_prognosis_4, data=LI_predictions)
# mean(LI_4_brierscore)
# LI_5_brierscore = brierscore(y_LI_only_hold_out~LI_prognosis_5, data=LI_predictions)
# mean(LI_5_brierscore)

# LI_1_logscore = logscore(y_LI_only_hold_out~LI_prognosis_1, data=LI_predictions)
# sum(LI_1_logscore)
# LI_2_logscore = logscore(y_LI_only_hold_out~LI_prognosis_2, data=LI_predictions)
# sum(LI_2_logscore)
# LI_3_logscore = logscore(y_LI_only_hold_out~LI_prognosis_3, data=LI_predictions)
# sum(LI_3_logscore)
# LI_4_logscore = logscore(y_LI_only_hold_out~LI_prognosis_4, data=LI_predictions)
# sum(LI_4_logscore)
# LI_5_logscore = logscore(y_LI_only_hold_out~LI_prognosis_5, data=LI_predictions)
# sum(LI_5_logscore)

# produce ROC curves
library(pROC)
labels = y_LI_only_hold_out
scores = LI_prognosis_1
plot(roc(labels, scores, direction="<"), col="red", lwd=3, main="ROC curve for p(y) in LI test sample model_1")
roc_obj_LI_model_1 <- roc(y_LI_only_hold_out, LI_prognosis_1)
auc(roc_obj_LI_model_1)
# round to 3 decimals 
scores = LI_prognosis_2
plot(roc(labels, scores, direction="<"), col="red", lwd=3, main="ROC curve for p(y) in LI test sample model_2")
roc_obj_LI_model_2 <- roc(y_LI_only_hold_out, LI_prognosis_2)
auc(roc_obj_LI_model_2)

scores = LI_prognosis_3
plot(roc(labels, scores, direction="<"), col="red", lwd=3, main="ROC curve for p(y) in LI test sample model_3")
roc_obj_LI_model_3 <- roc(y_LI_only_hold_out, LI_prognosis_3)
auc(roc_obj_LI_model_3)

scores = LI_prognosis_4
plot(roc(labels, scores, direction="<"), col="red", lwd=3, main="ROC curve for p(y) in LI test sample model_4")
roc_obj_LI_model_4 <- roc(y_LI_only_hold_out, LI_prognosis_4)
auc(roc_obj_LI_model_4)
############################# demonstrate evaluation in LI prognosis model #############################

############################# demonstrate evaluation in HI prognosis model #############################
# here we build 3 simple HI prognosis models to demonstrate the evaluation

# here we get all the training data necessary to build a HI prognostic model
X_HI_only = X_training[X_training$Treatment_pathway==3,]
X_HI_only_forEN = subset(X_HI_only,select=prediction_variables)
y_HI_only = as.factor(X_HI_only$y)

# grab the HI only patients from the hold-out
X_HI_only_hold_out = X_hold_out[X_hold_out$Treatment_pathway==3,]
y_HI_only_hold_out = X_HI_only_hold_out$y
X_HI_only_hold_out = subset(X_HI_only_hold_out,select=c(prediction_variables,"y"))

# need to grab this without Y for the ENR model
X_HI_only_hold_out_forEN = subset(X_HI_only_hold_out,select=prediction_variables)

# model 1 is just baseHIne PHQ_9 severity in a simple logistic regression
HI_prognostic_model1 = glm(y ~ PHQ9_first, data = X_HI_only, family = "binomial")
summary(HI_prognostic_model1)

# model 2 uses 4 other decent predictors and WSAS in a simple logistic regression
HI_prognostic_model2 = glm(y ~ Ethnicity_binary+Phobia_Q1_first+Phobia_Q2_first+Employment_binary, data = X_HI_only, family = "binomial")
summary(HI_prognostic_model2)

# model 3 is a logistic regression built using elastic net regularization 
# use elastic net to build this model
set.seed(12345678)
fit_HI = glmnet(data.matrix(X_HI_only_forEN), y_HI_only, family="binomial", alpha=.5)
plot(fit_HI,label=TRUE)
HI_prognostic_model3 = cv.glmnet(data.matrix(X_HI_only_forEN), y_HI_only, family="binomial", alpha=.5)
plot(HI_prognostic_model3)
HI_prognostic_model3$lambda.min
HI_prognostic_model3$lambda.1se
print(coef(HI_prognostic_model3, s = "lambda.min"))
print(coef(HI_prognostic_model3, s = "lambda.1se"))
# in the split halves training sample this model, using 1se for lambda, includes age, employment, disability status, PHQ9_first, and WSAS_first

# now we produce prognostic predictions for the hold-out
HI_prognosis_1 = predict(HI_prognostic_model1, X_HI_only_hold_out, type = "response")
HI_prognosis_2 = predict(HI_prognostic_model2, X_HI_only_hold_out, type = "response")
# to produce prognostic predictions for the hold-out with an elastic net model requires slightly different code
# HI_prognosis_3 = predict(HI_prognostic_model3, newx=data.matrix(X_HI_only_hold_out), type = "response", s = .07)
HI_prognosis_3 = predict(HI_prognostic_model3, newx=data.matrix(X_HI_only_hold_out_forEN), type = "response", s = "lambda.1se")
# have to convert to numeric vector form
HI_prognosis_3 = as.numeric(HI_prognosis_3)

# this shows the baserate in the training sample
mean(X_HI_only$y)
# [1] 0.4658298 - rate in full training sample
# [1] 0.4971264 - rate in split-halves training sample

# this shows the baserate in the test sample
mean(y_HI_only_hold_out)
# [1] 0.4363144

# Please add your HI prognosis model here
# for now I'm just repeating HI prognosis model 3
HI_prognosis_4 = HI_prognosis_3

# we add a fifth set of predictions which are simply the baserate in the training sample
# HI_prognosis_5 = rep(0.4658298,dim(X_HI_only_hold_out)[1]) # using baserate in full training sample
# we add a fifth set of predictions which are simply the baserate in the training sample
HI_prognosis_5 = rep(0.4971264,dim(X_HI_only_hold_out)[1]) # using baserate in split-halves training sample

plot(HI_prognosis_1,HI_prognosis_2,'p')
cor(HI_prognosis_1,HI_prognosis_2)

plot(HI_prognosis_1,HI_prognosis_3,'p')
cor(HI_prognosis_1,HI_prognosis_3)

plot(HI_prognosis_2,HI_prognosis_3,'p')
cor(HI_prognosis_2,HI_prognosis_3)

# calculate deviance statistic
# log-HIkeHIhood : sum from i=1 to N of [ Yi*ln(P(Yi))+(1-Yi)*ln(1-P(yi))]
# calculate brier score
# brier_score: (1/n)*sum(pi-oi)^2
log_likelihood_calculator_HI_1 = rep(NA,dim(X_HI_only_hold_out)[1])
brier_score_calculator_HI_1 = rep(NA,dim(X_HI_only_hold_out)[1])
log_likelihood_calculator_HI_2 = rep(NA,dim(X_HI_only_hold_out)[1])
brier_score_calculator_HI_2 = rep(NA,dim(X_HI_only_hold_out)[1])
log_likelihood_calculator_HI_3 = rep(NA,dim(X_HI_only_hold_out)[1])
brier_score_calculator_HI_3 = rep(NA,dim(X_HI_only_hold_out)[1])
log_likelihood_calculator_HI_4 = rep(NA,dim(X_HI_only_hold_out)[1])
brier_score_calculator_HI_4 = rep(NA,dim(X_HI_only_hold_out)[1])
log_likelihood_calculator_HI_5 = rep(NA,dim(X_HI_only_hold_out)[1])
brier_score_calculator_HI_5 = rep(NA,dim(X_HI_only_hold_out)[1])

for (i in 1:dim(X_HI_only_hold_out)[1]){
  log_likelihood_calculator_HI_1[i] = y_HI_only_hold_out[i]*log(HI_prognosis_1[i])+(1-y_HI_only_hold_out[i])*log(1-HI_prognosis_1[i])
  brier_score_calculator_HI_1[i] = (HI_prognosis_1[i]-y_HI_only_hold_out[i])^2
  log_likelihood_calculator_HI_2[i] = y_HI_only_hold_out[i]*log(HI_prognosis_2[i])+(1-y_HI_only_hold_out[i])*log(1-HI_prognosis_2[i])
  brier_score_calculator_HI_2[i] = (HI_prognosis_2[i]-y_HI_only_hold_out[i])^2
  log_likelihood_calculator_HI_3[i] = y_HI_only_hold_out[i]*log(HI_prognosis_3[i])+(1-y_HI_only_hold_out[i])*log(1-HI_prognosis_3[i])
  brier_score_calculator_HI_3[i] = (HI_prognosis_3[i]-y_HI_only_hold_out[i])^2
  log_likelihood_calculator_HI_4[i] = y_HI_only_hold_out[i]*log(HI_prognosis_4[i])+(1-y_HI_only_hold_out[i])*log(1-HI_prognosis_4[i])
  brier_score_calculator_HI_4[i] = (HI_prognosis_4[i]-y_HI_only_hold_out[i])^2
  log_likelihood_calculator_HI_5[i] = y_HI_only_hold_out[i]*log(HI_prognosis_5[i])+(1-y_HI_only_hold_out[i])*log(1-HI_prognosis_5[i])
  brier_score_calculator_HI_5[i] = (HI_prognosis_5[i]-y_HI_only_hold_out[i])^2
}

deviance_for_HI_model_1 = sum(log_likelihood_calculator_HI_1,na.rm=TRUE)
brier_score_for_HI_model_1 = (1/dim(X_HI_only_hold_out)[1])*sum(brier_score_calculator_HI_1,na.rm=TRUE)

deviance_for_HI_model_2 = sum(log_likelihood_calculator_HI_2,na.rm=TRUE)
brier_score_for_HI_model_2 = (1/dim(X_HI_only_hold_out)[1])*sum(brier_score_calculator_HI_2,na.rm=TRUE)

deviance_for_HI_model_3 = sum(log_likelihood_calculator_HI_3,na.rm=TRUE)
brier_score_for_HI_model_3 = (1/dim(X_HI_only_hold_out)[1])*sum(brier_score_calculator_HI_3,na.rm=TRUE)

deviance_for_HI_model_4 = sum(log_likelihood_calculator_HI_4,na.rm=TRUE)
brier_score_for_HI_model_4 = (1/dim(X_HI_only_hold_out)[1])*sum(brier_score_calculator_HI_4,na.rm=TRUE)

deviance_for_HI_model_5 = sum(log_likelihood_calculator_HI_5,na.rm=TRUE)
brier_score_for_HI_model_5 = (1/dim(X_HI_only_hold_out)[1])*sum(brier_score_calculator_HI_5,na.rm=TRUE)

print(round(deviance_for_HI_model_1,1))
print(round(brier_score_for_HI_model_1,3))
print(round(deviance_for_HI_model_2,1))
print(round(brier_score_for_HI_model_2,3))
print(round(deviance_for_HI_model_3,1))
print(round(brier_score_for_HI_model_3,3))
print(round(deviance_for_HI_model_4,1))
print(round(brier_score_for_HI_model_4,3))
print(round(deviance_for_HI_model_5,1))
print(round(brier_score_for_HI_model_5,3))

# bind all predictions together with observed y 
HI_predictions = data.frame(y_HI_only_hold_out,HI_prognosis_1,HI_prognosis_2,HI_prognosis_3,HI_prognosis_4,HI_prognosis_5)

# here is a program for calculating brier scores if you want to confirm that my code works
# library(scoring)
# HI_1_brierscore = brierscore(y_HI_only_hold_out~HI_prognosis_1, data=HI_predictions)
# mean(HI_1_brierscore)
# HI_2_brierscore = brierscore(y_HI_only_hold_out~HI_prognosis_2, data=HI_predictions)
# mean(HI_2_brierscore)
# HI_3_brierscore = brierscore(y_HI_only_hold_out~HI_prognosis_3, data=HI_predictions)
# mean(HI_3_brierscore)
# HI_4_brierscore = brierscore(y_HI_only_hold_out~HI_prognosis_4, data=HI_predictions)
# mean(HI_4_brierscore)
# HI_5_brierscore = brierscore(y_HI_only_hold_out~HI_prognosis_5, data=HI_predictions)
# mean(HI_5_brierscore)

# HI_1_logscore = logscore(y_HI_only_hold_out~HI_prognosis_1, data=HI_predictions)
# sum(HI_1_logscore)
# HI_2_logscore = logscore(y_HI_only_hold_out~HI_prognosis_2, data=HI_predictions)
# sum(HI_2_logscore)
# HI_3_logscore = logscore(y_HI_only_hold_out~HI_prognosis_3, data=HI_predictions)
# sum(HI_3_logscore)
# HI_4_logscore = logscore(y_HI_only_hold_out~HI_prognosis_4, data=HI_predictions)
# sum(HI_4_logscore)
# HI_5_logscore = logscore(y_HI_only_hold_out~HI_prognosis_5, data=HI_predictions)
# sum(HI_5_logscore)

# produce ROC curves
labels = y_HI_only_hold_out
scores = HI_prognosis_1
plot(roc(labels, scores, direction="<"), col="blue", lwd=3, main="ROC curve for p(y) in HI test sample model_1")
roc_obj_HI_model_1 <- roc(y_HI_only_hold_out, HI_prognosis_1)
auc(roc_obj_HI_model_1)

scores = HI_prognosis_2
plot(roc(labels, scores, direction="<"), col="blue", lwd=3, main="ROC curve for p(y) in HI test sample model_2")
roc_obj_HI_model_2 <- roc(y_HI_only_hold_out, HI_prognosis_2)
auc(roc_obj_HI_model_2)

scores = HI_prognosis_3
plot(roc(labels, scores, direction="<"), col="blue", lwd=3, main="ROC curve for p(y) in HI test sample model_3")
roc_obj_HI_model_3 <- roc(y_HI_only_hold_out, HI_prognosis_3)
auc(roc_obj_HI_model_3)

scores = HI_prognosis_4
plot(roc(labels, scores, direction="<"), col="blue", lwd=3, main="ROC curve for p(y) in HI test sample model_4")
roc_obj_HI_model_4 <- roc(y_HI_only_hold_out, HI_prognosis_4)
auc(roc_obj_HI_model_4)
############################# demonstrate evaluation in HI prognosis model #############################

############################# demonstrate evaluation in differential response model #############################
# now we show the evaluation of the differential model based on accuracy
# we will demonstrate two different ways a differential prediction can be generated
# differential_prediction_1 will create differential predictions by subtracting two prognoses created from separate prognostic models (one for HI and one for LI) using only PHQ-9
# differential_prediction_2 will create differential predictions by subtracting two prognoses created from separate prognostic models (one for HI and one for LI) using a few variables
# differential_prediction_3 will create differential predictions by subtracting two prognoses created from separate prognostic models (one for HI and one for LI) using ENR
# differential_prediction_4 is your differential prediction model
# differential_prediction_5a and 5b will use the base response rates in each condition and add random noise (4a sd=.001, 4b sd=.05)
# differential_prediction_6 will create differential predictions from a single model, as done in DeRubeis et al., 2014 PAI paper (here, with a logistic regression with interactions between tx variable and predictors)

# create a temporary structure because you're going to overwrite the tx data
X_hold_out_fake_LI = X_hold_out
X_hold_out_fake_HI = X_hold_out

# make all patients in these fake holdouts have tx = LI assignment or tx = HI assignment to allow for prediction generation
X_hold_out_fake_LI$tx = factor(0, levels = c(0, 1)) # 
X_hold_out_fake_HI$tx = factor(1, levels = c(0, 1)) # 

# grab the hold-out test data, predictors only for use with the ENR model (see above)
X_hold_out_fake_LI_forENR = subset(X_hold_out_fake_LI,select=prediction_variables)
X_hold_out_fake_HI_forENR = subset(X_hold_out_fake_HI,select=prediction_variables)

# here we create a PAI-style model that includes variables and their interaction with tx
# we use this model as a demonstration:
differential_model_6 = glm(y ~ tx*(PHQ9_first+WSAS_first+Employment_binary+Ethnicity_binary+GAD7_first+Phobia_Q3_first), data = X_training, family = "binomial")
summary(differential_model_6)

# now generate predictions for differential_model 1
p_hat_LI_model_1 = predict(LI_prognostic_model1, X_hold_out_fake_LI, type = "response")
p_hat_HI_model_1 = predict(HI_prognostic_model1, X_hold_out_fake_HI, type = "response")
differential_prediction_model_1 = p_hat_HI_model_1-p_hat_LI_model_1
hist(differential_prediction_model_1)

# now generate predictions for differential_model 2
p_hat_LI_model_2 = predict(LI_prognostic_model2, X_hold_out_fake_LI, type = "response")
p_hat_HI_model_2 = predict(HI_prognostic_model2, X_hold_out_fake_HI, type = "response")
differential_prediction_model_2 = p_hat_HI_model_2-p_hat_LI_model_2
hist(differential_prediction_model_2)

# now generate predictions for differential_model 3
# this needs to use slightly different code (see above)
p_hat_LI_model_3 = predict(LI_prognostic_model3, newx=data.matrix(X_hold_out_fake_LI_forENR), type = "response", s = "lambda.1se")
p_hat_HI_model_3 = predict(HI_prognostic_model3, newx=data.matrix(X_hold_out_fake_HI_forENR), type = "response", s = "lambda.1se")
p_hat_LI_model_3 = as.numeric(p_hat_LI_model_3)
p_hat_HI_model_3 = as.numeric(p_hat_HI_model_3)
differential_prediction_model_3 = p_hat_HI_model_3-p_hat_LI_model_3
hist(differential_prediction_model_3)

# PLEASE ADD YOUR MODEL HERE - GENERATE YOUR DIFFERENTIAL PREDICTIONS IN R, OR LET ME KNOW 
# HOW TO GENERATE THEM IN SOME OTHER PROGRAM SO I CAN RECREATE IN THE TRUE HOLDOUT
# as a placeholder I put in the predictions from model_3 - but replace with your model below
differential_prediction_model_4 = differential_prediction_model_3
# most models will also be able to generate LI and HI predictions, so add those here
# if not, just leave it as LI and HI from model_3 and let me know
p_hat_LI_model_4 = p_hat_LI_model_3
p_hat_HI_model_4 = p_hat_HI_model_3

# now generate predictions for differential_model 5
# for these predictions, we use the base rates with random noise added
# (note: there are easier ways to code this but I wanted to show exactly what is happening)
mean(y_HI_only_hold_out)
# [1] 0.4363144
mean(y_LI_only_hold_out)
# [1] 0.3432836
# set seed so this is reproducible
set.seed(54321)
# add tiny random noise
p_hat_LI_model_5a = rep(mean(y_LI_only_hold_out),dim(X_hold_out)[1]) + rnorm(dim(X_hold_out)[1],mean=0,sd=.001) # using baserate in training sample
p_hat_HI_model_5a = rep(mean(y_HI_only_hold_out),dim(X_hold_out)[1]) + rnorm(dim(X_hold_out)[1],mean=0,sd=.001) # using baserate in training sample
differential_prediction_model_5a = p_hat_HI_model_5a-p_hat_LI_model_5a
# now add larger random noise
p_hat_LI_model_5b = rep(mean(y_LI_only_hold_out),dim(X_hold_out)[1]) + rnorm(dim(X_hold_out)[1],mean=0,sd=.05) # using baserate in training sample
p_hat_HI_model_5b = rep(mean(y_HI_only_hold_out),dim(X_hold_out)[1]) + rnorm(dim(X_hold_out)[1],mean=0,sd=.05) # using baserate in training sample
differential_prediction_model_5b = p_hat_HI_model_5b-p_hat_LI_model_5b
hist(differential_prediction_model_5a)
hist(differential_prediction_model_5b)

# now generate predictions for differential_model 5
p_hat_LI_model_6 = predict(differential_model_6, X_hold_out_fake_LI, type = "response")
p_hat_HI_model_6 = predict(differential_model_6, X_hold_out_fake_HI, type = "response")
differential_prediction_model_6 = p_hat_HI_model_5-p_hat_LI_model_6
hist(differential_prediction_model_6)

# grab the indices of which individuals got tx==0 (li) and tx==1 (hi)
tx_HI_i = which(X_hold_out$tx==1, arr.ind = TRUE)
tx_LI_i = which(X_hold_out$tx==0, arr.ind = TRUE)

step_size = 150
window_size = 300
bin_number = ceiling((dim(X_hold_out)[1]-window_size)/step_size)
# we will use step sizes that are half the window size
# note: insofar as the middle people are counted multiple times they have more influence - the tails are only counted once

# create a structure to save the results
model_evaluations = matrix(NA, nrow = 7, ncol = 6)
colnames(model_evaluations) = c("tstat", "slope", "observed_range", "predicted_range", "predicted_avg_diff", "cor_pred_diff")
model_predictions = matrix(NA, nrow = 7, ncol = bin_number)
model_results = matrix(NA, nrow = 7, ncol = bin_number)

# start at left, grab n=window_size, sort into LI and HI, calculate observed response, move right n=step_size, repeat.
for (k in 1:7){
  if (k==1){
    differential_prediction = differential_prediction_model_1
    x_label = "model_1 predicted differences"
    p_hat_LI = p_hat_LI_model_1
    p_hat_HI = p_hat_HI_model_1 
  } else if (k==2){
    differential_prediction = differential_prediction_model_2
    x_label = "model_2 predicted differences"
    p_hat_LI = p_hat_LI_model_2
    p_hat_HI = p_hat_HI_model_2 
  } else if (k==3){
    differential_prediction = differential_prediction_model_3
    x_label = "model_3 predicted differences"
    p_hat_LI = p_hat_LI_model_3
    p_hat_HI = p_hat_HI_model_3 
  } else if (k==4){
    differential_prediction = differential_prediction_model_4
    x_label = "model_4 predicted differences"
    p_hat_LI = p_hat_LI_model_4
    p_hat_HI = p_hat_HI_model_4
  } else if (k==5){
    differential_prediction = differential_prediction_model_5a
    x_label = "model_5a predicted differences"
    p_hat_LI = p_hat_LI_model_5a
    p_hat_HI = p_hat_HI_model_5a
  } else if (k==6){
    differential_prediction = differential_prediction_model_5b
    x_label = "model_5b predicted differences"
    p_hat_LI = p_hat_LI_model_5b
    p_hat_HI = p_hat_HI_model_5b
  } else if (k==7){
    differential_prediction = differential_prediction_model_6
    x_label = "your model's predicted differences"
    p_hat_LI = p_hat_LI_model_6
    p_hat_HI = p_hat_HI_model_6 
  } else {
    print("warning, out of bounds")
  }
  idx_of_sorted_subjects = sort(differential_prediction, decreasing = FALSE, index.return = TRUE)$ix

  observed_LI_rate = rep(NA,bin_number)
  observed_HI_rate = rep(NA,bin_number)
  observed_differential_response = rep(NA,bin_number)
  predicted_LI_rate = rep(NA,bin_number)
  predicted_HI_rate = rep(NA,bin_number)
  predicted_differential_response = rep(NA,bin_number)
  predicted_differential_response_B = rep(NA,bin_number)
  sample_size_LI = rep(NA,bin_number)
  sample_size_HI = rep(NA,bin_number)
  
    for (i in 1:bin_number){
      # this line of code shows you what the window bounds are 
      temp_i = idx_of_sorted_subjects[(1+(i-1)*step_size):(window_size+(i-1)*step_size)]
      if (i<bin_number){
        # print(c(i,(1+(i-1)*step_size),(window_size+(i-1)*step_size)))
        temp_i = idx_of_sorted_subjects[(1+(i-1)*step_size):(window_size+(i-1)*step_size)]
        observed_LI_rate[i] = mean(X_hold_out$y[intersect(temp_i,tx_LI_i)])
        observed_HI_rate[i] = mean(X_hold_out$y[intersect(temp_i,tx_HI_i)])
        observed_differential_response[i]=observed_HI_rate[i]-observed_LI_rate[i]
        predicted_LI_rate[i] = mean(p_hat_LI[intersect(temp_i,tx_LI_i)])
        predicted_HI_rate[i] = mean(p_hat_HI[intersect(temp_i,tx_HI_i)])
        predicted_differential_response[i] = mean(differential_prediction[temp_i])
      } else {
        # print(c(i,(1+(i-1)*step_size),dim(X_hold_out)[1]))
        # this makes the final window larger or smaller to include everyone who is left
        temp_i = idx_of_sorted_subjects[(1+(i-1)*step_size):dim(X_hold_out)[1]]
        observed_LI_rate[i] = mean(X_hold_out$y[intersect(temp_i,tx_LI_i)])
        observed_HI_rate[i] = mean(X_hold_out$y[intersect(temp_i,tx_HI_i)])
        observed_differential_response[i]=observed_HI_rate[i]-observed_LI_rate[i]
        predicted_LI_rate[i] = mean(p_hat_LI[intersect(temp_i,tx_LI_i)])
        predicted_HI_rate[i] = mean(p_hat_HI[intersect(temp_i,tx_HI_i)])
        predicted_differential_response[i] = mean(differential_prediction[temp_i])
      }
      sample_size_LI[i] = length(intersect(temp_i,tx_LI_i))
      sample_size_HI[i] = length(intersect(temp_i,tx_HI_i))
    }
  cat("evaluating",x_label,".......","\n")
  pred_diff_resp = round(predicted_differential_response,3)
  obs_diff_resp = round(observed_differential_response,3)
  print(cbind(sample_size_LI,sample_size_HI,pred_diff_resp,obs_diff_resp))
  plot(1:bin_number,observed_differential_response,'p',xlab=paste(x_label,"bins"))
  # the below command can be used to see what y limits and x limits should be used to standardize all plots
  cat("y_limits",round(c(min(observed_differential_response),max(observed_differential_response)),3),"\n")
  cat("x_limits",round(c(min(predicted_differential_response),max(predicted_differential_response)),3),"\n")
  cat("predicted avg differential response (full sample avg):",round(mean(differential_prediction),3),"\n")
  cat("predicted avg differential response (binned avg):",round(mean(predicted_differential_response),3),"\n")
  predicted_avg_diff = mean(differential_prediction)
  plot(predicted_differential_response,observed_differential_response,'p',ylim=c(-.1,.3),
       xlim = c(-.1,.3),xlab=x_label) 
  plot(predicted_differential_response,observed_differential_response,'p',ylim=c(-.1,.3),
       xlim = c(min(predicted_differential_response),max(predicted_differential_response)),xlab=x_label)
  
  cor_windows = cor(1:bin_number,observed_differential_response)
  cor_pred_diff = cor(predicted_differential_response,observed_differential_response) 
  cat("windows correlation = ",round(cor_windows,3),"\n")
  cat("predicted difference correlation = ",round(cor_pred_diff,3),"\n")
  
  # correlation itself isn't enough, slope doesn't work either
  # for example, small random fluctuations can result in high-ish correlations (see above) and very high slopes
  temp_data = data.frame(cbind(predicted_differential_response,observed_differential_response))
  slope_checker = lm(observed_differential_response ~ predicted_differential_response, data=temp_data)
  # print(summary(slope_checker))
  sc_coefs = slope_checker$coefficients
  sc_coefs_summary = data.frame(summary(slope_checker)$coefficients)
  # we can use the t-statistic to adjust for the error around the slope
  # t-value is the coefficient divided by its standard error 
  cat("slope t-stat = ",round(sc_coefs_summary$t.value[2],3),"\n")
  print(round(sc_coefs_summary,3))
  cat("\n")
  observed_range = max(observed_differential_response) - min(observed_differential_response)
  predicted_range = max(predicted_differential_response) - min(predicted_differential_response)
  tstat = sc_coefs_summary$t.value[2]
  slope = sc_coefs_summary$Estimate[2]
  
  model_evaluations[k,] = c(tstat, slope, observed_range, predicted_range, predicted_avg_diff, cor_pred_diff)
  model_predictions[k,] = predicted_differential_response
  model_results[k,] = observed_differential_response
}

print(round(model_evaluations,3))

# please email these 3 CSVs to me along with your code
write.csv(model_evaluations,"model_evaluation_for_zach.csv")
write.csv(model_predictions,"model_predictions_for_zach.csv")
write.csv(model_results,"model_results_for_zach.csv")

# also need to save the evaluations of prognostic models
auc_LI_1 = auc(roc_obj_LI_model_1)
auc_LI_2 = auc(roc_obj_LI_model_2)
auc_LI_3 = auc(roc_obj_LI_model_3)
auc_LI_4 = auc(roc_obj_LI_model_4)

auc_HI_1 = auc(roc_obj_HI_model_1)
auc_HI_2 = auc(roc_obj_HI_model_2)
auc_HI_3 = auc(roc_obj_HI_model_3)
auc_HI_4 = auc(roc_obj_HI_model_4)

LI_model_results = matrix(NA, nrow = 5, ncol = 3)
HI_model_results = matrix(NA, nrow = 5, ncol = 3)

colnames(LI_model_results) = c("brier_score","deviance","auc")
colnames(HI_model_results) = c("brier_score","deviance","auc")

LI_model_results[1,] = c(brier_score_for_LI_model_1,deviance_for_LI_model_1,auc_LI_1)
LI_model_results[2,] = c(brier_score_for_LI_model_2,deviance_for_LI_model_2,auc_LI_2)
LI_model_results[3,] = c(brier_score_for_LI_model_3,deviance_for_LI_model_3,auc_LI_3)
LI_model_results[4,] = c(brier_score_for_LI_model_4,deviance_for_LI_model_4,auc_LI_4)
LI_model_results[5,] = c(brier_score_for_LI_model_5,deviance_for_LI_model_5,NA)


HI_model_results[1,] = c(brier_score_for_HI_model_1,deviance_for_HI_model_1,auc_HI_1)
HI_model_results[2,] = c(brier_score_for_HI_model_2,deviance_for_HI_model_2,auc_HI_2)
HI_model_results[3,] = c(brier_score_for_HI_model_3,deviance_for_HI_model_3,auc_HI_3)
HI_model_results[4,] = c(brier_score_for_HI_model_4,deviance_for_HI_model_4,auc_HI_4)
HI_model_results[5,] = c(brier_score_for_HI_model_5,deviance_for_HI_model_5,NA)

write.csv(LI_model_results,"LI_model_results_for_zach.csv")
write.csv(HI_model_results,"HI_model_results_for_zach.csv")






