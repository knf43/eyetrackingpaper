#packages
library("readxl")
library(lme4)
library(lmerTest)
library(MuMIn)

#open data
file_path <- file.choose()
eyetracking_data <- read_excel(file_path)
head(eyetracking_data)
str(eyetracking_data)

# Convert columns to the correct data types
eyetracking_data$looking_at_target <- as.numeric(eyetracking_data$looking_at_target)
eyetracking_data$stress <- as.factor(eyetracking_data$stress)
eyetracking_data$syllable_struct <- as.factor(eyetracking_data$syllable_struct)
eyetracking_data$test_phase <- as.factor(eyetracking_data$test_phase)
eyetracking_data$participant_ID <- as.factor(eyetracking_data$participant_ID)
eyetracking_data$WM_score <- as.numeric(eyetracking_data$WM_score)
eyetracking_data$accuracy <- as.numeric(eyetracking_data$accuracy)
eyetracking_data$RT <- as.numeric(eyetracking_data$RT)
eyetracking_data$AvgAcc <- as.numeric(eyetracking_data$AvgAcc)
eyetracking_data$AvgRT_MS <- as.numeric(eyetracking_data$AvgRT_MS)
eyetracking_data$SumTrials <- as.numeric(eyetracking_data$SumTrials)

# Standardize the numeric variables
eyetracking_data$WM_score_scaled <- scale(eyetracking_data$WM_score)
eyetracking_data$AvgAcc_scaled <- scale(eyetracking_data$AvgAcc)
eyetracking_data$AvgRT_MS_scaled <- scale(eyetracking_data$AvgRT_MS)
eyetracking_data$SumTrials_scaled <- scale(eyetracking_data$SumTrials)

str(eyetracking_data)

# fit the GLMM models

# Base model with random intercept for participant
model_base <- glmer(looking_at_target ~ 1 + (1 | participant_ID), 
                    data = eyetracking_data, family = binomial)

summary(model_base)

# Model with stress
model_stress <- glmer(looking_at_target ~ stress + (1 | participant_ID), 
                      data = eyetracking_data, family = binomial)

summary(model_stress)

anova(model_base, model_stress)

# Calculate R2m and R2c for model_stress
r2_values <- r.squaredGLMM(model_stress)

# Print the R2m and R2c
r2_values

# Model with stress and syllable structure
model_stress_syllable <- glmer(looking_at_target ~ stress + syllable_struct + (1 | participant_ID), 
                               data = eyetracking_data, family = binomial)

summary(model_stress_syllable)

anova(model_stress, model_stress_syllable)

# Model with stress, syllable structure, and WM scores
model_stress_syllable_WM <- glmer(looking_at_target ~ stress + syllable_struct + WM_score + (1 | participant_ID), 
                                  data = eyetracking_data, family = binomial)
summary(model_stress_syllable_WM)

anova(model_stress_syllable, model_stress_syllable_WM)

# Model with test phase
model_test_phase <- glmer(looking_at_target ~ stress + syllable_struct + WM_score + test_phase + (1 | participant_ID), 
                    data = eyetracking_data, family = binomial)
summary(model_test_phase)

anova(model_stress_syllable_WM, model_test_phase)

# Model with AvgAcc
model_avgacc <- glmer(looking_at_target ~ stress + syllable_struct + WM_score_scaled + test_phase + AvgAcc_scaled + (1 | participant_ID), 
                      data = eyetracking_data, family = binomial)
summary(model_avgacc)

anova(model_test_phase, model_avgacc)

# Model with AvgRT_MS
model_avgrt <- glmer(looking_at_target ~ stress + syllable_struct + WM_score_scaled + test_phase + AvgAcc_scaled + AvgRT_MS_scaled + (1 | participant_ID), 
                     data = eyetracking_data, family = binomial)
summary(model_avgrt)

anova(model_avgacc, model_avgrt)

# Model with SumTrials
model_sumtrials <- glmer(looking_at_target ~ stress + syllable_struct + WM_score_scaled + test_phase + AvgAcc_scaled + AvgRT_MS_scaled + SumTrials_scaled + (1 | participant_ID), 
                         data = eyetracking_data, family = binomial)
summary(model_sumtrials)

anova(model_avgrt, model_sumtrials)

