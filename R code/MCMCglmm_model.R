
install.packages("MCMCglmm")
library("MCMCglmm")

install.packages("dplyr")
library("dplyr")

install.packages("stringr")
library("stringr")



###### fit my model 


setwd("C:/Users/terry/Documents/RData/1") # change this to your work directory
dir()

a <- read.csv("keystroke_features_for_argument_element.csv", header = TRUE)
a$element <- as.factor(a$element)

# set k as the number of categories
# set I and J as the identity matrices. 
k <- length(levels(a$element))

I <- diag(k-1)

J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))


# build a model with "Data" as the baseline level
m1 <- MCMCglmm(element ~ -1  # suppress the overall intercept by using -1
               + trait # trait indexes columns of the response matrix. By fitting trait as a fixed effect we allow the responses to have different means.
               + trait:( # By fitting trait:(...) we allow different regression slopes of the traits on all the predictors listed within the parenthesis. 
               Prompt
               + product_process_ratio
               + mean_process_time_in_p_burst_pt2000
               + mean_typed_chars_in_p_burst_pt2000
               + mean_pause_time_in_seconds_pt200
               + mean_pause_time_sec_within_words_pt200
               + mean_pause_time_sec_between_words_pt200),
               
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
               nitt = 250000, # the number of MCMC iteractions.
               thin = 50, # a thinning interval. 
               
               family = "categorical", 
               data = a
             )
# check the results 
summary(m1)

# check random effects 
m1$Random

# check the trace plots. If the each plot look like a "fat, hairy caterpillar", which is not bending in any direction, everything is fine.   
plot(m1$Sol)

#check autocorrelation. As a rule of thumb, the values should be < 0.1.
autocorr(m1$Sol)

m1$VCV



