
install.packages("MCMCglmm")

install.packages("dplyr")

install.packages("stringr")

install.packages("modeest")

library("MCMCglmm")
library("dplyr")
library("stringr")
library(modeest) #for descriptives
library(psych) #for everything

###### fit my model 

setwd("C:/0_RData/1")
dir()

a <- read.csv("data20220929_withQuality.csv", header = TRUE)
colnames(a)
str(a)
a$Quality <- as.factor(a$Quality)

Position <- filter(a, Element == "Position")
Claim <- filter(a, Element == "Claim")
Counterclaim <- filter(a, Element == "Counterclaim")
Rebuttal <- filter(a, Element == "Rebuttal")
Evidence <- filter(a, Element == "Evidence")

nrow(Evidence)






PositionL1 <- filter(Position, Language == "L1")
ClaimL1 <- filter(Claim, Language == "L1")
CounterclaimL1 <- filter(Counterclaim, Language == "L1")
RebuttalL1 <- filter(Rebuttal, Language == "L1")
EvidenceL1 <- filter(Evidence, Language == "L1")


PositionL2 <- filter(Position, Language == "L2")
ClaimL2 <- filter(Claim, Language == "L2")
CounterclaimL2 <- filter(Counterclaim, Language == "L2")
RebuttalL2 <- filter(Rebuttal, Language == "L2")
EvidenceL2 <- filter(Evidence, Language == "L2")





table(PositionL1$Quality)
table(ClaimL1$Quality)
table(CounterclaimL1$Quality)
table(RebuttalL1$Quality)
table(EvidenceL1$Quality)

table(PositionL2$Quality)
table(ClaimL2$Quality)
table(CounterclaimL2$Quality)
table(RebuttalL2$Quality)
table(EvidenceL2$Quality)





#################################################
setwd("C:/Users/terry/Documents/RData/1")
dir()
setwd("C:/0_RData/1")
dir()

a <- read.csv("data20230516_for_analyses.csv", header = TRUE)
str(a)
colnames(a)

L1 <- filter(a, Language == "L1")
L2 <- filter(a, Language == "L2")
str(L2)
L1$Language

################# descriptive statistics for L1 data  

table(L1$Element) # Check the numbers of the argument element labels 
colnames(L1)

Position <- filter(L1, Element == "Position")
predictors_Position <- Position[, c(7:12)]
stats <- describe(predictors_Position)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L1_Position_descriptives.csv")

Claim <- filter(L1, Element == "Claim")
predictors_Claim <- Claim[, c(7:12)]
stats <- describe(predictors_Claim)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L1_Claim_descriptives.csv")

Counterclaim <- filter(L1, Element == "Counterclaim")
predictors_Counterclaim <- Counterclaim[, c(6:15)]
stats <- describe(predictors_Counterclaim)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L1_Counterclaim_descriptives.csv")


Rebuttal <- filter(L1, Element == "Rebuttal")
predictors_Rebuttal <- Rebuttal[, c(6:15)]
stats <- describe(predictors_Rebuttal)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L1_Rebuttal_descriptives.csv")


Evidence <- filter(L1, Element == "Evidence")
predictors_Evidence <- Evidence[, c(7:12)]
stats <- describe(predictors_Evidence)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L1_Evidence_descriptives.csv")



########## descriptive statistics for L2 data  

table(L2$Element) # Check the numbers of the argument element labels 

Position <- filter(L2, Element == "Position")
predictors_Position <- Position[, c(7:12)]
stats <- describe(predictors_Position)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L2_Position_descriptives.csv")

Claim <- filter(L2, Element == "Claim")
predictors_Claim <- Claim[, c(7:12)]
stats <- describe(predictors_Claim)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L2_Claim_descriptives.csv")

Counterclaim <- filter(L2, Element == "Counterclaim")
predictors_Counterclaim <- Counterclaim[, c(7:12)]
stats <- describe(predictors_Counterclaim)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L2_Counterclaim_descriptives.csv")


Rebuttal <- filter(L2, Element == "Rebuttal")
predictors_Rebuttal <- Rebuttal[, c(7:12)]
stats <- describe(predictors_Rebuttal)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L2_Rebuttal_descriptives.csv")


Evidence <- filter(L2, Element == "Evidence")
predictors_Evidence <- Evidence[, c(7:12)]
stats <- describe(predictors_Evidence)
stats$mean <- round(stats$mean, 3)
stats$sd <- round(stats$sd, 3)
write.csv(stats, "L2_Evidence_descriptives.csv")

#################################################################

### build full models on the entire dataset 
a <- read.csv("data20230516_for_analyses.csv", header = TRUE)
str(a)
colnames(a)
nrow(a)

## Set different element as the baseline category for each model 
a$Element <- relevel(a$Element, ref="Position")
levels(a$Element)

# set k as the number of categories
# set I and J as the identity matrices. 
k <- length(levels(a$Element))
k
I <- diag(k-1)
I
J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))
J


# build a model with "Evidence" as the baseline level
m_position <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
               + trait # trait index columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
               + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                 Language 
                 +Prompt
                 + Mean_Typed_In_Pbursts_chars
                 + mean_length_Rburst_chars 
                 + mean_pause_time_in_seconds
                 + mean_pause_time_within_words
                 + mean_pause_time_between_words
                 + DeletionPercentage
                 + typing_score 
                 + vocab_score),
               
               # add random effect: Participants' ID. 
               random = ~ us(trait):ID,
               
               #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
               rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
               
               # add an informative prior
               # V is the (co)variance matrix. R is the residual error structure.
               # G is a list of every random effect (in this case, only one)
               # n is the degrees of freedom (k-1)
               prior = list(
                 R = list(fix = 1, V = 1/k * (I + J), n = 4), 
                 G = list(G1 = list(V = diag(4), n = 4)) 
               ),
               
               # specify the number of burnin, nitt, and thin.
               # formula: sample size = (nitt - burnin)/thin
               burnin = 10000, # the number of initial MCMC iterations that are discarded.
               nitt = 260000, # the number of MCMC iteractions.
               thin = 50, # a thinning interval. 
               
               family = "categorical", 
               data = a
)
# check the results 
summary(m_position)

sink("m_position.txt")
summary(m_position)
sink()


# write out the results for report
model_position <- summary(m_position)
write.csv(model_position$solutions, "m_position_results.csv")


# manipulate the result csv file 
dir()
results <- read.csv("m_position_results.csv", header = TRUE)
colnames(results)
results$X <- as.character(results$X)

results$X <- results$X %>%
  str_replace_all("traitElement\\.", "") %>%
  str_replace_all("Language", "0Language")%>%
  str_replace_all("Prompt", "0Prompt")%>%
  str_replace_all("Mean_Typed_In_Pbursts_chars", "1Mean_Typed_In_Pbursts_chars")%>%
  str_replace_all("mean_length_Rburst_chars", "2mean_length_Rburst_chars")%>%
  str_replace_all("mean_pause_time_in_seconds", "3mean_pause_time_in_seconds")%>%
  str_replace_all("mean_pause_time_within_words", "4mean_pause_time_within_words")%>%
  str_replace_all("mean_pause_time_between_words", "5mean_pause_time_between_words")%>%
  str_replace_all("DeletionPercentage", "6DeletionPercentage") 

results <- arrange(results, X)

results$post.mean <- round(results$post.mean, 3)
results$l.95..CI <- round(results$l.95..CI, 3)
results$u.95..CI <- round(results$u.95..CI, 3)
results$pMCMC <- round(results$pMCMC, 4)

write.csv(results, "Position_RESULTS.csv")


###########@@@@@@
## Set different element as the baseline category for each model 
a$Element <- relevel(a$Element, ref="Claim")
levels(a$Element)

# build a model with "Claim" as the baseline level
m_claim <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
                       + trait # trait index columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
                       + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                         Language 
                         +Prompt
                         + Mean_Typed_In_Pbursts_chars
                         + mean_length_Rburst_chars 
                         + mean_pause_time_in_seconds
                         + mean_pause_time_within_words
                         + mean_pause_time_between_words
                         + DeletionPercentage
                         + typing_score 
                         + vocab_score),
                       
                       # add random effect: Participants' ID. 
                       random = ~ us(trait):ID,
                       
                       #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
                       rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
                       
                       # add an informative prior
                       # V is the (co)variance matrix. R is the residual error structure.
                       # G is a list of every random effect (in this case, only one)
                       # n is the degrees of freedom (k-1)
                       prior = list(
                         R = list(fix = 1, V = 1/k * (I + J), n = 4), 
                         G = list(G1 = list(V = diag(4), n = 4)) 
                       ),
                       
                       # specify the number of burnin, nitt, and thin.
                       # formula: sample size = (nitt - burnin)/thin
                       burnin = 10000, # the number of initial MCMC iterations that are discarded.
                       nitt = 260000, # the number of MCMC iteractions.
                       thin = 50, # a thinning interval. 
                       
                       family = "categorical", 
                       data = a
)

# write out the results for report
model_claim <- summary(m_claim)
write.csv(model_claim$solutions, "m_claim_results.csv")


# manipulate the result csv file 
dir()
results <- read.csv("m_claim_results.csv", header = TRUE)
colnames(results)
results$X <- as.character(results$X)

results$X <- results$X %>%
  str_replace_all("traitElement\\.", "") %>%
  str_replace_all("Language", "0Language")%>%
  str_replace_all("Prompt", "0Prompt")%>%
  str_replace_all("Mean_Typed_In_Pbursts_chars", "1Mean_Typed_In_Pbursts_chars")%>%
  str_replace_all("mean_length_Rburst_chars", "2mean_length_Rburst_chars")%>%
  str_replace_all("mean_pause_time_in_seconds", "3mean_pause_time_in_seconds")%>%
  str_replace_all("mean_pause_time_within_words", "4mean_pause_time_within_words")%>%
  str_replace_all("mean_pause_time_between_words", "5mean_pause_time_between_words")%>%
  str_replace_all("DeletionPercentage", "6DeletionPercentage") 

results <- arrange(results, X)

results$post.mean <- round(results$post.mean, 3)
results$l.95..CI <- round(results$l.95..CI, 3)
results$u.95..CI <- round(results$u.95..CI, 3)
results$pMCMC <- round(results$pMCMC, 4)

write.csv(results, "Claim_RESULTS.csv")


###########@@@@@@
## Set different element as the baseline category for each model 
a$Element <- relevel(a$Element, ref="Counterclaim")
levels(a$Element)

# build a model with "Claim" as the baseline level
m_counterclaim <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
                    + trait # trait index columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
                    + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                      Language 
                      +Prompt
                      + Mean_Typed_In_Pbursts_chars
                      + mean_length_Rburst_chars 
                      + mean_pause_time_in_seconds
                      + mean_pause_time_within_words
                      + mean_pause_time_between_words
                      + DeletionPercentage
                      + typing_score 
                      + vocab_score),
                    
                    # add random effect: Participants' ID. 
                    random = ~ us(trait):ID,
                    
                    #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
                    rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
                    
                    # add an informative prior
                    # V is the (co)variance matrix. R is the residual error structure.
                    # G is a list of every random effect (in this case, only one)
                    # n is the degrees of freedom (k-1)
                    prior = list(
                      R = list(fix = 1, V = 1/k * (I + J), n = 4), 
                      G = list(G1 = list(V = diag(4), n = 4)) 
                    ),
                    
                    # specify the number of burnin, nitt, and thin.
                    # formula: sample size = (nitt - burnin)/thin
                    burnin = 10000, # the number of initial MCMC iterations that are discarded.
                    nitt = 260000, # the number of MCMC iteractions.
                    thin = 50, # a thinning interval. 
                    
                    family = "categorical", 
                    data = a
)

# write out the results for report
model_counterclaim <- summary(m_counterclaim)
write.csv(model_counterclaim$solutions, "m_counterclaim_results.csv")


# manipulate the result csv file 
dir()
results <- read.csv("m_counterclaim_results.csv", header = TRUE)
colnames(results)
results$X <- as.character(results$X)

results$X <- results$X %>%
  str_replace_all("traitElement\\.", "") %>%
  str_replace_all("Language", "0Language")%>%
  str_replace_all("Prompt", "0Prompt")%>%
  str_replace_all("Mean_Typed_In_Pbursts_chars", "1Mean_Typed_In_Pbursts_chars")%>%
  str_replace_all("mean_length_Rburst_chars", "2mean_length_Rburst_chars")%>%
  str_replace_all("mean_pause_time_in_seconds", "3mean_pause_time_in_seconds")%>%
  str_replace_all("mean_pause_time_within_words", "4mean_pause_time_within_words")%>%
  str_replace_all("mean_pause_time_between_words", "5mean_pause_time_between_words")%>%
  str_replace_all("DeletionPercentage", "6DeletionPercentage") 

results <- arrange(results, X)

results$post.mean <- round(results$post.mean, 3)
results$l.95..CI <- round(results$l.95..CI, 3)
results$u.95..CI <- round(results$u.95..CI, 3)
results$pMCMC <- round(results$pMCMC, 4)

write.csv(results, "Couterclaim_RESULTS.csv")








##############################################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



L1$Element <- str_replace_all(L1$Element, "Evidence", "0Evidence")
L1$Element <- as.factor(L1$Element)
levels(as.factor(L1$Element))


# build a model with "Evidence" as the baseline level
m1 <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
               + trait # trait index columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
               + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                Prompt
               + Mean_Typed_In_Pbursts_chars
               + mean_length_Rburst_chars 
               + mean_pause_time_in_seconds
               + mean_pause_time_within_words
               + mean_pause_time_between_words
               + DeletionPercentage
               + typing_score 
               + vocab_score),
               
               # add random effect: Participants' ID. 
               random = ~ us(trait):ID,
               
               #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
               rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
               
               # add an informative prior
               # V is the (co)variance matrix. R is the residual error structure.
               # G is a list of every random effect (in this case, only one)
               # n is the degrees of freedom (k-1)
               prior = list(
                R = list(fix = 1, V = 1/k * (I + J), n = 2), 
                G = list(G1 = list(V = diag(2), n = 2)) 
                ),
               
               # specify the number of burnin, nitt, and thin.
               # formula: sample size = (nitt - burnin)/thin
               burnin = 10000, # the number of initial MCMC iterations that are discarded.
               nitt = 260000, # the number of MCMC iteractions.
               thin = 50, # a thinning interval. 
               
               family = "categorical", 
               data = L1
             )
# check the results 
summary(m1)

sink("L1_model.txt")
summary(m1)
sink()
# check random effects 
m1$Random

# check the trace plots. If the each plot look like a "fat, hairy caterpillar", which is not bending in any direction, everything is fine.   
plot(m1$Sol)

#check autocorrelation. As a rule of thumb, the values should be < 0.1.
sink("L1model_autocorr")
autocorr(m1$Sol)
sink()


m1$VCV

###Build a parsimonious model with only the significant fixed effects 

m3 <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
               + trait # trait indexes columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
               + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                  chars_produced_per_minute_including_spaces
                 #+ Mean_Process_Time_Pbursts 
                 + Mean_Typed_In_Pbursts_chars
                 #+ mean_pause_time_in_seconds
                 #+ mean_pause_time_within_words 
                 + mean_pause_time_between_words
                 + Product_Process_Ratio),
                 #+ typing_score),
               
               # add random effect: Participants' ID. 
               random = ~ us(trait):ID,
               
               #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
               rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
               
               # add an informative prior
               # V is the (co)variance matrix. R is the residual error structure.
               # G is a list of every random effect (in this case, only one)
               # n is the degrees of freedom (k-1)
               prior = list(
                 R = list(fix = 1, V = 1/k * (I + J), n = 2), 
                 G = list(G1 = list(V = diag(2), n = 2)) 
               ),
               
               # specify the number of burnin, nitt, and thin.
               # formula: sample size = (nitt - burnin)/thin
               burnin = 10000, # the number of initial MCMC iterations that are discarded.
               nitt = 260000, # the number of MCMC iteractions.
               thin = 50, # a thinning interval. 
               
               family = "categorical", 
               data = L1
)

sink("L1_final_model.txt")
summary(m3)
sink()

sink("L1_final_model_autocorr.txt")
autocorr(m2$Sol)
sink()



# check the results 
L1_final_model_summary <- summary(m1)
L1_final_model_summary$solutions
write.csv(L1_final_model_summary$solutions, "L1_final_model_results.csv")

# manipulate the result csv file 
dir()
results <- read.csv("L1_final_model_results.csv", header = TRUE)
colnames(results)
results$X <- as.character(results$X)

results$X <- results$X %>%
  str_replace_all("traitElement\\.", "") %>%
  str_replace_all("Prompt", "0Prompt")%>%
  str_replace_all("Mean_Typed_In_Pbursts_chars", "1Mean_Typed_In_Pbursts_chars")%>%
  str_replace_all("mean_length_Rburst_chars", "2mean_length_Rburst_chars")%>%
  str_replace_all("mean_pause_time_in_seconds", "3mean_pause_time_in_seconds")%>%
  str_replace_all("mean_pause_time_within_words", "4mean_pause_time_within_words")%>%
  str_replace_all("mean_pause_time_between_words", "5mean_pause_time_between_words")%>%
  str_replace_all("DeletionPercentage", "6DeletionPercentage") 

results <- arrange(results, X)

results$post.mean <- round(results$post.mean, 3)
results$l.95..CI <- round(results$l.95..CI, 3)
results$u.95..CI <- round(results$u.95..CI, 3)
results$pMCMC <- round(results$pMCMC, 4)

write.csv(results, "L1_RESULTS_new.csv")

###########################################
#########build a MCMC model for L2 data 

L2 <- filter(a, Language == "L2")
# set k as the number of categories
# set I and J as the identity matrices. 
k <- length(levels(L2$Element))
k
I <- diag(k-1)
I
J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))
J


L2$Element <- str_replace_all(L2$Element, "Evidence", "0Evidence")
L2$Element <- as.factor(L2$Element)
levels(as.factor(L2$Element))


# build a model with "Evidence" as the baseline level
m1 <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
               + trait # trait indexes columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
               + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                 Prompt
                 + Mean_Typed_In_Pbursts_chars
                 + mean_length_Rburst_chars 
                 + mean_pause_time_in_seconds
                 + mean_pause_time_within_words
                 + mean_pause_time_between_words
                 + DeletionPercentage
                 + typing_score 
                 + vocab_score),
               
               # add random effect: Participants' ID. 
               random = ~ us(trait):ID,
               
               #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
               rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
               
               # add an informative prior
               # V is the (co)variance matrix. R is the residual error structure.
               # G is a list of every random effect (in this case, only one)
               # n is the degrees of freedom (k-1)
               prior = list(
                 R = list(fix = 1, V = 1/k * (I + J), n = 2), 
                 G = list(G1 = list(V = diag(2), n = 2)) 
               ),
               
               # specify the number of burnin, nitt, and thin.
               # formula: sample size = (nitt - burnin)/thin
               burnin = 10000, # the number of initial MCMC iterations that are discarded.
               nitt = 260000, # the number of MCMC iteractions.
               thin = 50, # a thinning interval. 
               
               family = "categorical", 
               data = L2
)
# check the results 
summary(m1)

sink("L2_model.txt")
summary(m1)
sink()
# check random effects 
m1$Random

# check the trace plots. If the each plot look like a "fat, hairy caterpillar", which is not bending in any direction, everything is fine.   
plot(m1$Sol)

#check autocorrelation. As a rule of thumb, the values should be < 0.1.
sink("L2model_autocorr")
autocorr(m1$Sol)
sink()


m1$VCV

###Build a parsimonious model with only the significant fixed effects 

m2 <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
               + trait # trait indexes columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
               + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                chars_produced_per_minute_including_spaces
                 #+ Mean_Process_Time_Pbursts 
                 + Mean_Typed_In_Pbursts_chars
                # + mean_pause_time_in_seconds 
                 + mean_pause_time_within_words
                 + MeanDeletionLen 
                 #+ Product_Process_Ratio 
                 + typing_score 
                 + vocab_score
                ),
               
               # add random effect: Participants' ID. 
               random = ~ us(trait):ID,
               
               #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
               rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
               
               # add an informative prior
               # V is the (co)variance matrix. R is the residual error structure.
               # G is a list of every random effect (in this case, only one)
               # n is the degrees of freedom (k-1)
               prior = list(
                 R = list(fix = 1, V = 1/k * (I + J), n = 2), 
                 G = list(G1 = list(V = diag(2), n = 2)) 
               ),
               
               # specify the number of burnin, nitt, and thin.
               # formula: sample size = (nitt - burnin)/thin
               burnin = 10000, # the number of initial MCMC iterations that are discarded.
               nitt = 260000, # the number of MCMC iteractions.
               thin = 50, # a thinning interval. 
               
               family = "categorical", 
               data = L2
)

sink("L2_final_model.txt")
summary(m2)
sink()



m3 <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
               + trait # trait indexes columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
               + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                 chars_produced_per_minute_including_spaces
                 + Mean_Process_Time_Pbursts 
                 + Mean_Typed_In_Pbursts_chars
                 + mean_pause_time_in_seconds 
                 + mean_pause_time_within_words
                 + MeanDeletionLen 
                 + typing_score),
               
               # add random effect: Participants' ID. 
               random = ~ us(trait):ID,
               
               #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
               rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
               
               # add an informative prior
               # V is the (co)variance matrix. R is the residual error structure.
               # G is a list of every random effect (in this case, only one)
               # n is the degrees of freedom (k-1)
               prior = list(
                 R = list(fix = 1, V = 1/k * (I + J), n = 4), 
                 G = list(G1 = list(V = diag(4), n = 4)) 
               ),
               
               # specify the number of burnin, nitt, and thin.
               # formula: sample size = (nitt - burnin)/thin
               burnin = 10000, # the number of initial MCMC iterations that are discarded.
               nitt = 260000, # the number of MCMC iteractions.
               thin = 50, # a thinning interval. 
               
               family = "categorical", 
               data = L2
)

sink("L2_final_model-v2.txt")
summary(m3)
sink()



m4 <- MCMCglmm(Element ~ -1  # suppress the overall intercept by using -1
               + trait # trait indexes columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
               + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
                 chars_produced_per_minute_including_spaces
                 + Mean_Process_Time_Pbursts 
                 + Mean_Typed_In_Pbursts_chars
                 + mean_pause_time_within_words
                 + MeanDeletionLen 
                 + typing_score),
               
               # add random effect: Participants' ID. 
               random = ~ us(trait):ID,
               
               #residual variance structure R. Use a fully parametrized covariance matrix to allow each linear predictor to have a unique residual.
               rcov = ~ us(trait):units, #rcov: residual covariance structure. units here indexes the rows of the response matrix, or the individual observations. 
               
               # add an informative prior
               # V is the (co)variance matrix. R is the residual error structure.
               # G is a list of every random effect (in this case, only one)
               # n is the degrees of freedom (k-1)
               prior = list(
                 R = list(fix = 1, V = 1/k * (I + J), n = 4), 
                 G = list(G1 = list(V = diag(4), n = 4)) 
               ),
               
               # specify the number of burnin, nitt, and thin.
               # formula: sample size = (nitt - burnin)/thin
               burnin = 10000, # the number of initial MCMC iterations that are discarded.
               nitt = 100000, # the number of MCMC iteractions.
               thin = 50, # a thinning interval. 
               
               family = "categorical", 
               data = L2
)

sink("L2_final_model-v3.txt")
summary(m4)
sink()




sink("L2_final_model_autocorr.txt")
autocorr(m2$Sol)
sink()



# check the results 
L2_final_model_summary <- summary(m1)
L2_final_model_summary$solutions
write.csv(L2_final_model_summary$solutions, "L2_final_model_results.csv")

# manipulate the result csv file 
dir()
results <- read.csv("L2_final_model_results.csv", header = TRUE)
colnames(results)
results$X <- as.character(results$X)

results$X <- results$X %>%
  str_replace_all("traitElement\\.", "") %>%
  str_replace_all("Prompt", "0Prompt")%>%
  str_replace_all("Mean_Typed_In_Pbursts_chars", "1Mean_Typed_In_Pbursts_chars")%>%
  str_replace_all("mean_length_Rburst_chars", "2mean_length_Rburst_chars")%>%
  str_replace_all("mean_pause_time_in_seconds", "3mean_pause_time_in_seconds")%>%
  str_replace_all("mean_pause_time_within_words", "4mean_pause_time_within_words")%>%
  str_replace_all("mean_pause_time_between_words", "5mean_pause_time_between_words")%>%
  str_replace_all("DeletionPercentage", "6DeletionPercentage") 

results <- arrange(results, X)

results$post.mean <- round(results$post.mean, 3)
results$l.95..CI <- round(results$l.95..CI, 3)
results$u.95..CI <- round(results$u.95..CI, 3)
results$pMCMC <- round(results$pMCMC, 4)

write.csv(results, "L2_RESULTS.csv")

mean(L1$typing_score)
sd(L1$typing_score)
mean(L1$vocab_score)
sd(L1$vocab_score)

mean(L2$typing_score)
sd(L2$typing_score)
mean(L2$vocab_score)
sd(L2$vocab_score)


setwd("C:/Users/terry/Documents/RData/1")
dir()
a <- read.csv("demographic20220825.csv", header = T)
colnames(a)
L2 <- filter(a, Nativeness == "L2")
L2$Age
mean(L2$Age)
sd(L2$Age)

