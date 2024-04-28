#######
### Multiwave sampling Estimation Comparisons
#######

## This script uses simulated version of the iris dataset
## (with n=996) to conduct a simple
## simulation example of (three-wave) multiwave sampling.
## Over 1,000 iterations, it conducts stratified sampling
## to sample from the 3 strata of species, with x-optimal allocation
## in Wave 1 and then use Neyman allocation in Waves 2 and 3. It then
## estimates the mean Sepal Width using 5 different strategies


######
### Load packages ------------------------------------------------------
#####
library(datasets)
library(dplyr)
library(optimall)
library(survey)
library(tidyr)

#######
### Set parameters -------------------------------------------------
########
set.seed(1)

#######
### Function for one iteration --------------------------------------------
#######
run_sim_pstrat <- function(n_per_wave = 50){

  #####
  ## Generate data
  #####
  n <- c(rmultinom(1, 996, c(1/3, 1/3, 1/3)))
  col1 <- c(rep("setosa", times = n[1]),
            rep("versicolor", times = n[2]),
            rep("virginica", times = n[3]))
  pl <- c(rnorm(n[1], 1.462, 0.173),
          rnorm(n[2], 4.260, 0.470),
          rnorm(n[3], 5.552, 0.552))
  sl <- c(rnorm(n[1], pl[1:n[1]] * 3.35, 0.341),
          rnorm(n[2], pl[(n[1]+1):(n[1]+n[2])] * 1.32, 0.366),
          rnorm(n[3],  sl[(n[2]+1):996] * 1.14, 0.302))
  full_data <- data.frame("id" = 1:996,
                          "Species" = as.factor(col1),
                          "Sepal.Length" = sl,
                          "Petal.Length" = pl)
  phase1_data <- full_data[,-4]

  ####
  ## Multiwave object setup
  ####

  Survey <- multiwave(phases = 2, waves = c(1, 3),
                      phase1 = phase1_data)

  set_mw(Survey, phase = 2, slot = "metadata") <- list(id = "id",
                                                       strata = "Species",
                                                       design_strata = "strata",
                                                       include_probs = TRUE)

  ####
  ## Wave 1
  ####

  ### Allocation: X-allocate with Phase 1 sepal length
  Survey <- apply_multiwave(Survey, phase = 2, wave = 1,
                            fun = "optimum_allocation",
                            y = "Sepal.Length",
                            nsample = n_per_wave, method = "Neyman")

  ### Select samples
  Survey <- apply_multiwave(Survey, phase = 2, wave = 1,
                            fun = "sample_strata",
                            n_allocated = "stratum_size",
                            probs = ~stratum_size/npop)

  ### "Collect" data
  set_mw(Survey, phase = 2, wave = 1, slot = "sampled_data") <-
    full_data[full_data$id %in% get_mw(Survey, phase = 2, wave = 1,
                                       slot = "samples")$ids,
              c("id", "Petal.Length")]

  Survey <- merge_samples(Survey, phase = 2, wave = 1)

  ####
  ## Wave 2
  ####

  ### Allocation: Neyman allocation with already-collected phase 2 data.
  Survey <- apply_multiwave(Survey, phase = 2, wave = 2,
                            fun = "allocate_wave",
                            y = "Petal.Length",
                            nsample = n_per_wave, allocation_method = "Neyman",
                            already_sampled = "sampled_phase2")

  ### Select samples
  Survey <- apply_multiwave(Survey, phase = 2, wave = 2,
                            fun = "sample_strata",
                            n_allocated = "n_to_sample",
                            probs = ~n_to_sample/(npop - nsample_prior),
                            already_sampled = "sampled_phase2")

  ### "Collect" data
  set_mw(Survey, phase = 2, wave = 2, slot = "sampled_data") <-
    full_data[full_data$id %in% get_mw(Survey, phase = 2, wave = 2,
                                       slot = "samples")$ids,
              c("id", "Petal.Length")]
  Survey <- merge_samples(Survey, phase = 2, wave = 2)

  ####
  ## Wave 3
  ####

  ### Allocation: Neyman allocation with already-collected phase 2 data.
  Survey <- apply_multiwave(Survey, phase = 2, wave = 3,
                            fun = "allocate_wave",
                            y = "Petal.Length",
                            nsample = n_per_wave, allocation_method = "Neyman",
                            already_sampled = "sampled_phase2")

  ### Select samples
  Survey <- apply_multiwave(Survey, phase = 2, wave = 3,
                            fun = "sample_strata",
                            n_allocated = "n_to_sample",
                            probs = ~n_to_sample/(npop - nsample_prior),
                            already_sampled = "sampled_phase2")

  ### "Collect" data
  set_mw(Survey, phase = 2, wave = 3, slot = "sampled_data") <-
    full_data[full_data$id %in% get_mw(Survey, phase = 2, wave = 3,
                                       slot = "samples")$ids,
              c("id", "Petal.Length")]
  Survey <- merge_samples(Survey, phase = 2, wave = 3)

  ### Final dataset for analysis
  survey_data <- get_mw(Survey, phase = 2, wave = 3, slot = "data")

  ### Clean up for printing
  survey_data <- survey_data[,c("id", "Species", "Sepal.Length",
                                "Petal.Length", "sampled_phase2",
                                "sampled_wave2.1", "sampled_wave2.2",
                                "sampled_wave2.3", "sampling_prob")]
  survey_data <- survey_data[order(-survey_data$sampled_phase2,
                                   survey_data$id), , drop = FALSE]


  #######
  #### 1. Post-stratification
  #######

  pst_design <- twophase(id = list(~id, ~id), strata = list(NULL, ~Species),
                         subset = ~as.logical(sampled_phase2),
                         data = survey_data, method = "simple")
  pst_est <- svymean(~Petal.Length, design = pst_design)
  pst_est_CI <- confint(pst_est)

  ######
  ##### 1b. Post-stratification with raking
  #######
  pst_design_rake <- calibrate(pst_design, formula = ~Species + Sepal.Length,
                               phase = 2, calfun = "raking")
  pst_est_rake <- svymean(~Petal.Length, design = pst_design_rake)
  pst_est_rake_CI <- confint(pst_est_rake)

  #######
  #### 2. Wave-specific conditional probabilities
  #######

  denom_data <- survey_data %>%
    dplyr::mutate(wave = case_when(sampled_wave2.1 == 1 ~ 1,
                                   sampled_wave2.2 == 1 ~ 2,
                                   sampled_wave2.3 == 1 ~ 3))

  survey_data <- survey_data %>%
    dplyr::left_join(dplyr::select(denom_data, c(id, wave)), by = "id")

  ## Find denominator from eq. 6 for each wave, species
  denom_data <- denom_data %>%
    dplyr::select(Species, wave, sampling_prob) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(wave)) %>%
    arrange(Species, wave) %>%
    group_by(Species) %>%
    dplyr::mutate(denom = ifelse(wave == 1, sampling_prob,
                                 sampling_prob * cumprod(1 - lag(sampling_prob, default = 0))))%>%
    ungroup()

  ## Merge back with original dataframe, attaching the denominator to each obs.
  survey_data <- survey_data %>%
    dplyr::left_join(dplyr::select(denom_data, Species, wave, denom),
                     by = c("Species","wave"))

  ####
  #### Specify design
  cp_design <- twophase(id = list(~id, ~id), strata = list(NULL, ~Species),
                        subset = ~as.logical(sampled_phase2),
                        data = survey_data, probs = list(NULL, ~denom))

  cp_est <- svymean(~Petal.Length, design = cp_design)
  cp_est_CI <- confint(cp_est)

  ######
  ##### 2b. Conditional probs with raking
  #######
  cp_design_rake <- calibrate(cp_design, formula = ~Sepal.Length,
                              phase = 2, calfun = "raking")
  cp_est_rake <- svymean(~Petal.Length + Species, design = cp_design_rake)
  cp_est_rake_CI <- confint(cp_est_rake)

  ######
  ##### 3. Replace denominator above (demon of Eq. 6 in Estimation Vignette)
  ##### with wave-specific sampling prob (not conditioning on prior waves)
  #####
  ##### Note: There are two ways of doing this which lead to the same
  ##### estimate (asymptotic SE differs, but neither closely approximates ESE).
  ##### One is using weights and the other is using the sampling probabilities.
  ##### Here we use weights.

  # Wave 1 weights
  wave1_data <- get_mw(Survey, phase = 2, wave = 1, slot = "data")
  samp1 <- wave1_data[wave1_data$sampled_phase2 == 1,]
  wave1_design <- get_mw(Survey, phase = 2, wave = 1, slot = "design")
  wgt <- as.data.frame(table(wave1_data$Species) / table(wave1_data$Species[wave1_data$sampled_phase2==1]))
  colnames(wgt) <- c("Species","weight")
  samp1 <- merge(samp1,wgt)
  # sum(samp1$weight)


  # Wave 2 weights
  wave2_data <- get_mw(Survey, phase = 2, wave = 2, slot = "data")
  wave2_data <- wave2_data[wave2_data$sampled_wave2.1 == 0,]
  samp2 <- wave2_data[wave2_data$sampled_phase2 == 1,]
  wave2_design <- get_mw(Survey, phase = 2, wave = 2, slot = "design")
  wgt <- as.data.frame(table(wave2_data$Species) / wave2_design$n_to_sample)
  colnames(wgt) <- c("Species","weight")
  samp2 <- merge(samp2,wgt)
  # sum(samp2$weight)

  # Wave 3 weights
  wave3_data <- get_mw(Survey, phase = 2, wave = 3, slot = "data")
  wave3_data <- wave3_data[wave3_data$sampled_wave2.1 == 0 & wave3_data$sampled_wave2.2 == 0,]
  samp3 <- wave3_data[wave3_data$sampled_phase2 == 1,]
  wave3_design <- get_mw(Survey, phase = 2, wave = 3, slot = "design")
  wgt <- as.data.frame(table(wave3_data$Species) / wave3_design$n_to_sample)
  colnames(wgt) <- c("Species","weight")
  samp3 <- merge(samp3,wgt)
  # sum(samp3$weight)

  # Normalization of weights: the sum of weights of the overall sample # must reproduce the total of units of first phase (10335), but the # contribution of each sample is made proportional to the different # subpopulations represented

  total <- sum(samp1$weight) + sum(samp2$weight) + sum(samp3$weight)
  samp1$weight_norm <- samp1$weight * (sum(nrow(survey_data)/sum(samp1$weight))) * sum(samp1$weight) /total
  # sum(samp1$weight_norm)

  samp2$weight_norm <- samp2$weight * (sum(nrow(survey_data)/sum(samp2$weight))) * sum(samp2$weight) / total
  # sum(samp2$weight_norm)

  samp3$weight_norm <- samp3$weight * (sum(nrow(survey_data)/sum(samp3$weight))) * sum(samp3$weight) / total
  # sum(samp3$weight_norm)

  # Pooling: the pooled sample represent the whole first phase population
  samp <- data.frame(rbind(samp1[,-which(names(samp1) == "sampled_wave2.1")],
                           samp2[,-which(names(samp2) %in% c("sampled_wave2.1", "sampled_wave2.2"))],
                           samp3[-which(names(samp3) %in% c("sampled_wave2.1", "sampled_wave2.2",
                                                            "sampled_wave2.3"))]))
  # sum(samp$weight_norm)

  # Estimate and sampling variance samp$new_strata <- as.factor(samp$new_strata)
  pool_design <- twophase(data=samp, id=list(~id, ~id),
                          strata = list(NULL, ~Species),
                          subset = ~as.logical(sampled_phase2),
                          method = "approx",
                          weights= list(NULL, ~weight_norm))

  # Note: the following code generates the exact same estimate without requiring
  # the weights to be calculated/normalized.
  # cp_design <- twophase(id = list(~id, ~id), strata = list(NULL, ~Species),
  #                       subset = ~as.logical(sampled_phase2),
  #                       data = survey_data, probs = list(NULL, ~sampling_prob))

  pool_est <- svymean(~Petal.Length, design = pool_design)
  pool_est
  pool_est_CI <- confint(pool_est)

  return(c(pst_est[1], SE(pst_est), pst_est_CI[1], pst_est_CI[2],
           pst_est_rake[1], SE(pst_est_rake)[1], pst_est_rake_CI[1],
           pst_est_rake_CI[2],
           cp_est[1], SE(cp_est)[1], cp_est_CI[1], cp_est_CI[2],
           cp_est_rake[1], SE(cp_est_rake)[1], cp_est_rake_CI[1],
           cp_est_rake_CI[2],
           pool_est[1], SE(pool_est)[1], pool_est_CI[1], pool_est_CI[2]))
}

########
#### Run simulations ----------------------------------------------
#########

###
### Case 1: n = 50


### Run this analysis repeatedly
estimates1 <- c()
estimates2 <- c()
estimates3 <- c()
estimates4 <- c()
estimates5 <- c()
ase1 <- c()
ase2 <- c()
ase3 <- c()
ase4 <- c()
ase5 <- c()
lower1 <- c()
lower2 <- c()
lower3 <- c()
lower4 <- c()
lower5 <- c()
upper1 <- c()
upper2 <- c()
upper3 <- c()
upper4 <- c()
upper5 <- c()

nreps <- 1000
for (i in 1:nreps){
  results <- run_sim_pstrat(n_per_wave = 50)
  estimates1 <- c(estimates1, results[1])
  estimates2 <- c(estimates2, results[5])
  estimates3 <- c(estimates3, results[9])
  estimates4 <- c(estimates4, results[13])
  estimates5 <- c(estimates5, results[17])
  ase1 <- c(ase1, results[2])
  ase2 <- c(ase2, results[6])
  ase3 <- c(ase3, results[10])
  ase4 <- c(ase4, results[14])
  ase5 <- c(ase5, results[18])
  lower1 <- c(lower1, results[3])
  lower2 <- c(lower2, results[7])
  lower3 <- c(lower3, results[11])
  lower4 <- c(lower4, results[15])
  lower5 <- c(lower5, results[19])
  upper1 <- c(upper1, results[4])
  upper2 <- c(upper2, results[8])
  upper3 <- c(upper3, results[12])
  upper4 <- c(upper4, results[16])
  upper5 <- c(upper5, results[20])
}

### True mean
true_mean <- mean(c(1.462, 4.260, 5.552))

### oracle_SE function
cover <- function(true, est, SE){
  ## return indicator mu in the interval (est-1.96SE, est+1.96SE)
  return((true > est-qnorm(.975)*SE)*(true<est+qnorm(0.975)*SE))
}

### Output dataset
stats <- data.frame(case = c(1,2,3,4,5),
                    true_mean = rep(true_mean, 5),
                    est = c(mean(unlist(estimates1)),
                            mean(unlist(estimates2)),
                            mean(unlist(estimates3)),
                            mean(unlist(estimates4)),
                            mean(unlist(estimates5))
                    ),
                    coverage = c(sum(true_mean >= lower1 & true_mean <= upper1)/nreps,
                                 sum(true_mean >= lower2 & true_mean <= upper2)/nreps,
                                 "-",
                                 "-",
                                 "-"),
                    ASE = c(mean(unlist(ase1)),
                            mean(unlist(ase2)),
                            "-",
                            "-",
                            "-"),
                    ESE = c(sqrt(var(unlist(estimates1))),
                            sqrt(var(unlist(estimates2))),
                            sqrt(var(unlist(estimates3))),
                            sqrt(var(unlist(estimates4))),
                            sqrt(var(unlist(estimates5)))),
                    RMSE = c(sqrt(mean((unlist(estimates1)- true_mean)^2)),
                             sqrt(mean((unlist(estimates2)- true_mean)^2)),
                             sqrt(mean((unlist(estimates3)- true_mean)^2)),
                             sqrt(mean((unlist(estimates4)- true_mean)^2)),
                             sqrt(mean((unlist(estimates5)- true_mean)^2))))

# stats$bias <- stats$est - true_mean
stats50 <- stats
stats50

###
### Case 2: n = 20


### Run this analysis repeatedly
estimates1 <- c()
estimates2 <- c()
estimates3 <- c()
estimates4 <- c()
estimates5 <- c()
ase1 <- c()
ase2 <- c()
ase3 <- c()
ase4 <- c()
ase5 <- c()
lower1 <- c()
lower2 <- c()
lower3 <- c()
lower4 <- c()
lower5 <- c()
upper1 <- c()
upper2 <- c()
upper3 <- c()
upper4 <- c()
upper5 <- c()

nreps <- 1000
for (i in 1:nreps){
  results <- run_sim_pstrat(n_per_wave = 20)
  estimates1 <- c(estimates1, results[1])
  estimates2 <- c(estimates2, results[5])
  estimates3 <- c(estimates3, results[9])
  estimates4 <- c(estimates4, results[13])
  estimates5 <- c(estimates5, results[17])
  ase1 <- c(ase1, results[2])
  ase2 <- c(ase2, results[6])
  ase3 <- c(ase3, results[10])
  ase4 <- c(ase4, results[14])
  ase5 <- c(ase5, results[18])
  lower1 <- c(lower1, results[3])
  lower2 <- c(lower2, results[7])
  lower3 <- c(lower3, results[11])
  lower4 <- c(lower4, results[15])
  lower5 <- c(lower5, results[19])
  upper1 <- c(upper1, results[4])
  upper2 <- c(upper2, results[8])
  upper3 <- c(upper3, results[12])
  upper4 <- c(upper4, results[16])
  upper5 <- c(upper5, results[20])
}

### True mean
true_mean <- mean(c(1.462, 4.260, 5.552))

### oracle_SE function
cover <- function(true, est, SE){
  ## return indicator mu in the interval (est-1.96SE, est+1.96SE)
  return((true > est-qnorm(.975)*SE)*(true<est+qnorm(0.975)*SE))
}

### Output dataset
stats <- data.frame(case = c(1,2,3,4,5),
                    true_mean = rep(true_mean, 5),
                    est = c(mean(unlist(estimates1)),
                            mean(unlist(estimates2)),
                            mean(unlist(estimates3)),
                            mean(unlist(estimates4)),
                            mean(unlist(estimates5))
                    ),
                    coverage = c(sum(true_mean >= lower1 & true_mean <= upper1)/nreps,
                                 sum(true_mean >= lower2 & true_mean <= upper2)/nreps,
                                 "-",
                                "-",
                                "-"),
                    ASE = c(mean(unlist(ase1)),
                            mean(unlist(ase2)),
                            "-",
                            "-",
                            "-"),
                    ESE = c(sqrt(var(unlist(estimates1))),
                            sqrt(var(unlist(estimates2))),
                            sqrt(var(unlist(estimates3))),
                            sqrt(var(unlist(estimates4))),
                            sqrt(var(unlist(estimates5)))),
                    RMSE = c(sqrt(mean((unlist(estimates1)- true_mean)^2)),
                             sqrt(mean((unlist(estimates2)- true_mean)^2)),
                             sqrt(mean((unlist(estimates3)- true_mean)^2)),
                             sqrt(mean((unlist(estimates4)- true_mean)^2)),
                             sqrt(mean((unlist(estimates5)- true_mean)^2))))

# stats$bias <- stats$est - true_mean
stats20 <- stats
stats20

###
### Case 3: n = 80


### Run this analysis repeatedly
estimates1 <- c()
estimates2 <- c()
estimates3 <- c()
estimates4 <- c()
estimates5 <- c()
ase1 <- c()
ase2 <- c()
ase3 <- c()
ase4 <- c()
ase5 <- c()
lower1 <- c()
lower2 <- c()
lower3 <- c()
lower4 <- c()
lower5 <- c()
upper1 <- c()
upper2 <- c()
upper3 <- c()
upper4 <- c()
upper5 <- c()

nreps <- 1000
for (i in 1:nreps){
  results <- run_sim_pstrat(n_per_wave = 80)
  estimates1 <- c(estimates1, results[1])
  estimates2 <- c(estimates2, results[5])
  estimates3 <- c(estimates3, results[9])
  estimates4 <- c(estimates4, results[13])
  estimates5 <- c(estimates5, results[17])
  ase1 <- c(ase1, results[2])
  ase2 <- c(ase2, results[6])
  ase3 <- c(ase3, results[10])
  ase4 <- c(ase4, results[14])
  ase5 <- c(ase5, results[18])
  lower1 <- c(lower1, results[3])
  lower2 <- c(lower2, results[7])
  lower3 <- c(lower3, results[11])
  lower4 <- c(lower4, results[15])
  lower5 <- c(lower5, results[19])
  upper1 <- c(upper1, results[4])
  upper2 <- c(upper2, results[8])
  upper3 <- c(upper3, results[12])
  upper4 <- c(upper4, results[16])
  upper5 <- c(upper5, results[20])
}

### True mean
true_mean <- mean(c(1.462, 4.260, 5.552))

### oracle_SE function
cover <- function(true, est, SE){
  ## return indicator mu in the interval (est-1.96SE, est+1.96SE)
  return((true > est-qnorm(.975)*SE)*(true<est+qnorm(0.975)*SE))
}

### Output dataset
stats <- data.frame(case = c(1,2,3,4,5),
                    true_mean = rep(true_mean, 5),
                    est = c(mean(unlist(estimates1)),
                            mean(unlist(estimates2)),
                            mean(unlist(estimates3)),
                            mean(unlist(estimates4)),
                            mean(unlist(estimates5))
                    ),
                    coverage = c(sum(true_mean >= lower1 & true_mean <= upper1)/nreps,
                                 sum(true_mean >= lower2 & true_mean <= upper2)/nreps,
                                 "-",
                                 "-",
                                 "-"),
                    ASE = c(mean(unlist(ase1)),
                            mean(unlist(ase2)),
                            "-",
                            "-",
                            "-"),
                    ESE = c(sqrt(var(unlist(estimates1))),
                            sqrt(var(unlist(estimates2))),
                            sqrt(var(unlist(estimates3))),
                            sqrt(var(unlist(estimates4))),
                            sqrt(var(unlist(estimates5)))),
                    RMSE = c(sqrt(mean((unlist(estimates1)- true_mean)^2)),
                             sqrt(mean((unlist(estimates2)- true_mean)^2)),
                             sqrt(mean((unlist(estimates3)- true_mean)^2)),
                             sqrt(mean((unlist(estimates4)- true_mean)^2)),
                             sqrt(mean((unlist(estimates5)- true_mean)^2))))

# stats$bias <- stats$est - true_mean
stats80 <- stats
stats80

