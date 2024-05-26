#######
### Multiwave sampling Estimation Comparisons
#######

## This script uses simulated version of the iris dataset
## (with n=996) to conduct a simple
## simulation example of (three-wave) multiwave sampling.
## Over 1,000 iterations, it conducts stratified sampling
## to sample from the 3 strata of species, with x-optimal allocation
## in Wave 1 and then use Neyman allocation in Waves 2 and 3. It then
## estimates the mean Petal Length using 5 different strategies


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
  pl <- c(rnorm(n[1], 1.462, 0.432),
          rnorm(n[2], 4.260, 0.470),
          rnorm(n[3], 5.552, 0.552))
  sl <- c(rnorm(n[1], pl[1:n[1]] * 3.35, 0.341),
          rnorm(n[2], pl[(n[1]+1):(n[1]+n[2])] * 1.32, 0.366),
          rnorm(n[3],  pl[(n[2]+1):996] * 1.14, 0.302))
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

  ### Computing this estimator requires some extra work (equation 6 of vignette),
  ### but the steps are made less complex by optimall and survey

  ###
  ### Step 1: Calculate denominator of Estimation Vignette equation 6

  ## Prepare data
  denom_data <- survey_data %>%
    dplyr::mutate(wave = case_when(sampled_wave2.1 == 1 ~ 1,
                                   sampled_wave2.2 == 1 ~ 2,
                                   sampled_wave2.3 == 1 ~ 3))

  survey_data <- survey_data %>%
    dplyr::left_join(dplyr::select(denom_data, c(id, wave)), by = "id")

  ## Find denominator for each wave, species
  denom_data <- denom_data %>%
    dplyr::select(Species, wave, sampling_prob) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(wave)) %>%
    arrange(Species, wave) %>%
    group_by(Species) %>%
    dplyr::mutate(denom = ifelse(wave == 1, sampling_prob,
                                 sampling_prob * cumprod(1 - lag(sampling_prob, default = 0))))%>%
    ungroup()

  flag <- ifelse(nrow(denom_data) == 9, 1, 0)

  ## Merge back with original dataframe, attaching the denominator to each obs.
  survey_data <- survey_data %>%
    dplyr::left_join(dplyr::select(denom_data, Species, wave, denom),
                     by = c("Species","wave"))

  ##### Step 2: Estimate total using computed denominator as weight
  #####
  ##### If not every stratum is sampled in each wave, we have to compute the mean
  ##### stratum-wise (as long as we guaranteed that each sample had a non-zero sampling
  ##### probability in at least one wave):
  phase2_data <- survey_data[survey_data$sampled_phase2 == 1,]
  phase2_data$weighted_obs <- phase2_data$Petal.Length/phase2_data$denom

  sampled_waves <- denom_data[denom_data$sampling_prob > 0,]
  n_waves_sampled <- table(sampled_waves$Species)

  total_est <- (sum(phase2_data[phase2_data$Species == "setosa","weighted_obs"])*3/
                  n_waves_sampled["setosa"] +
                  sum(phase2_data[phase2_data$Species == "virginica","weighted_obs"])*3/
                  n_waves_sampled["virginica"]+
                  sum(phase2_data[phase2_data$Species == "versicolor","weighted_obs"])*3/
                  n_waves_sampled["versicolor"])/3
  cp_est <- total_est/nrow(phase1_data)

  ## Step 3: Construct variance estimator

  ## Here we compute an estimator for variance according to vignette equation 7
  ## Augment denom dataframe with the N and n at each wave. These N and n are
  ## stored in the design dataframe
  designW123 <- dplyr::bind_rows(cbind(phase2_wave = 1,
                                       get_mw(Survey, 2, 1, "design")),
                                 cbind(phase2_wave = 2,
                                       get_mw(Survey, 2, 2, "design")),
                                 cbind(phase2_wave = 3,
                                       get_mw(Survey, 2, 3, "design")))

  ### Merge any differing column names, compute dependent probability as n_k/N_k*
  ### n_k/(N_k-1), merge with phase2_data
  designW123 <- designW123 |>
    dplyr::mutate(n_to_sample = dplyr::coalesce(n_to_sample, stratum_size),
                  nsample_prior = ifelse(is.na(nsample_prior),
                                         0 , nsample_prior))

  ### Now add three columns to design: prob of two obs being sampled in a given
  ### wave, prob of neither being sampled in given wave, or prob of only (specific)
  ### one being sampled in given wave. pp = pairwise probability.
  designW123 <- designW123 |>
    dplyr::mutate(both_pp = n_to_sample/(npop - nsample_prior)*
                    (n_to_sample-1)/(npop - nsample_prior - 1), #n/N*(n-1)/(N-1)
                  onlyone_pp = n_to_sample/(npop - nsample_prior)*
                    (npop- nsample_prior- n_to_sample)/(npop - nsample_prior - 1),
                  neither_pp =
                    (npop- nsample_prior- n_to_sample)/(npop - nsample_prior)*
                    (npop- nsample_prior- n_to_sample-1)/(npop - nsample_prior - 1),
                  single_prob = n_to_sample/(npop - nsample_prior))

  # Wide version for merging with survey_data
  designW123_wide <- designW123 %>%
    dplyr::select(phase2_wave, strata, both_pp, onlyone_pp, neither_pp, single_prob) %>%
    tidyr::pivot_wider(names_from = phase2_wave, values_from = c(both_pp, onlyone_pp,
                                                                 neither_pp, single_prob))

  ### Now calculate estimator for variance (vignette equation 7)
  ### Only observations in same wave and stratum are dependent
  phase2_ids <- dplyr::filter(survey_data, sampled_phase2 == 1)$id
  pairwise_df <- expand.grid("id1" = phase2_ids, "id2" = phase2_ids) %>%
    dplyr::left_join(dplyr::select(survey_data, id, "Species1" = Species,
                                   "wave1" = wave,
                                   "denom1" = denom,
                                   "Petal.Length1" = Petal.Length),
                     by = c("id1" = "id")) %>%
    dplyr::left_join(dplyr::select(survey_data, id, "Species2" = Species,
                                   "wave2" = wave,
                                   "denom2" = denom,
                                   "Petal.Length2" = Petal.Length),
                     by = c("id2" = "id")) %>%
    dplyr::left_join(designW123_wide, by = c("Species1" = "strata")) %>%
    dplyr::mutate(pairwise_prob =
                    case_when(id1 == id2 ~ denom1,
                              Species1 != Species2 ~ denom1*denom2,
                              wave1 == 1 & wave2 == 1 ~ both_pp_1,
                              wave1 == 2 & wave2 == 2 ~ neither_pp_1*both_pp_2,
                              wave1 == 3 & wave2 == 3 ~ neither_pp_1*neither_pp_2*both_pp_3,
                              TRUE ~ denom1*denom2),
                  phase2_variance_contribution = (1-denom1*denom2/pairwise_prob)*
                    Petal.Length1*Petal.Length2/(denom1*denom2)
    )

  phase2_variance_est <- sum(pairwise_df$phase2_variance_contribution)/nrow(phase1_data)^2/9


  ##
  ## Final variance estimator combines the phase 2 variance that we just calculated with
  ## phase 1 variance (which was already calculated in post-stratified estimator)

  ### Extract phase 1 variance estimate from pst_est
  phase1_variance_est <- attr(SE(pst_est), "phases")$phase1[1]

  ### Estimate variance with two-phase()
  cp_est_ase <- sqrt(phase2_variance_est + phase1_variance_est)

  ### Compute confidence interval
  cp_est_CI <- c(cp_est - qnorm(.975)*cp_est_ase, cp_est + qnorm(.975)*cp_est_ase)

  ##
  ##
  ## Note: using svymean() in survey leads to almost the exact phase 2 variance
  ## calculation.

  # phase2_data <- survey_data[survey_data$sampled_phase2 == 1,]
  # phase2_data$strata_int <- paste0(phase2_data$Species, ".", phase2_data$wave)
  # svydesign_phase2 <- svydesign(data = phase2_data,
  #                               ids = ~id,
  #                               strata = ~strata_int,
  #                               probs = ~denom)
  #
  # cp_est_ase <- sqrt(phase1_variance_est + (SE(svytotal(~Petal.Length, design = svydesign_phase2))/996/3)^2)
  # cp_est_CI <- c(cp_est - qnorm(.975)*cp_est_ase, cp_est + qnorm(.975)*cp_est_ase)


  ######
  ### The below example is NOT recommended, nor included in the estimation vignette
  ### but demonstrates the performance of an unconditional estimator that simply uses
  ### the wave-specific probabilities as the sampling probabilities without conditioning
  ### on previous waves.
  ######

  ######
  ##### 3. Replace denominator above (demon of Eq. 6 in Estimation Vignette)
  ##### with wave-specific sampling prob (not conditioning on prior waves)
  #####
  ##### Note: There are two ways of doing this which lead to the same
  ##### estimate (Asymptotic ASE estimates are not computed in this case).
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
                          subset = ~as.logical(sampled_phase2),
                          method = "simple",
                          weights= list(NULL, ~weight_norm))

  # Note: the following code generates the exact same estimate without requiring
  # the weights to be calculated/normalized.
  # pool_design <- twophase(id = list(~id, ~id), strata = list(NULL, ~Species),
  #                       subset = ~as.logical(sampled_phase2), method = "simple",
  #                       data = survey_data, probs = list(NULL, ~sampling_prob))

  pool_est <- svymean(~Petal.Length, design = pool_design)[1]

  return(c(pst_est[1], SE(pst_est), pst_est_CI[1], pst_est_CI[2],
           pst_est_rake[1], SE(pst_est_rake)[1], pst_est_rake_CI[1],
           pst_est_rake_CI[2],
           cp_est[1], cp_est_ase, cp_est_CI[1], cp_est_CI[2],
           pool_est[1],  flag))
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
ase1 <- c()
ase2 <- c()
ase3 <- c()
ase4 <- c()
lower1 <- c()
lower2 <- c()
lower3 <- c()
lower4 <- c()
upper1 <- c()
upper2 <- c()
upper3 <- c()
upper4 <- c()
flags <- c()

nreps <- 2000
for (i in 1:nreps){
  results <- run_sim_pstrat(n_per_wave = 50)
  estimates1 <- c(estimates1, results[1])
  estimates2 <- c(estimates2, results[5])
  estimates3 <- c(estimates3, results[9])
  estimates4 <- c(estimates4, results[13])
  ase1 <- c(ase1, results[2])
  ase2 <- c(ase2, results[6])
  ase3 <- c(ase3, results[10])
  lower1 <- c(lower1, results[3])
  lower2 <- c(lower2, results[7])
  lower3 <- c(lower3, results[11])
  upper1 <- c(upper1, results[4])
  upper2 <- c(upper2, results[8])
  upper3 <- c(upper3, results[12])
  flags <- c(flags, results[14])
}

### True mean
true_mean <- mean(c(1.462, 4.260, 5.552))

### Output dataset
stats <- data.frame(case = c(1,2,3,4),
                    true_mean = rep(true_mean, 4),
                    est = c(median(unlist(estimates1)),
                            median(unlist(estimates2)),
                            median(unlist(estimates3)),
                            median(unlist(estimates4))),
                    coverage = c(sum(true_mean >= lower1 & true_mean <= upper1)/nreps,
                                 sum(true_mean >= lower2 & true_mean <= upper2)/nreps,
                                 sum(true_mean >= lower3 & true_mean <= upper3)/nreps,
                                "-"),
                    ASE = c(median(unlist(ase1)),
                            median(unlist(ase2)),
                            median(unlist(ase3)),
                            "-"),
                    ESE = c(sqrt(var(unlist(estimates1))),
                            sqrt(var(unlist(estimates2))),
                            sqrt(var(unlist(estimates3[flags == 1]))),
                            sqrt(var(unlist(estimates4)))),
                    RMSE = c(sqrt(mean((unlist(estimates1)- true_mean)^2)),
                             sqrt(mean((unlist(estimates2)- true_mean)^2)),
                             sqrt(mean((unlist(estimates3)- true_mean)^2)),
                             sqrt(mean((unlist(estimates4)- true_mean)^2))))

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
ase1 <- c()
ase2 <- c()
ase3 <- c()
ase4 <- c()
lower1 <- c()
lower2 <- c()
lower3 <- c()
lower4 <- c()
upper1 <- c()
upper2 <- c()
upper3 <- c()
upper4 <- c()
flags <- c()

nreps <- 2000
for (i in 1:nreps){
  results <- run_sim_pstrat(n_per_wave = 20)
  estimates1 <- c(estimates1, results[1])
  estimates2 <- c(estimates2, results[5])
  estimates3 <- c(estimates3, results[9])
  estimates4 <- c(estimates4, results[13])
  ase1 <- c(ase1, results[2])
  ase2 <- c(ase2, results[6])
  ase3 <- c(ase3, results[10])
  lower1 <- c(lower1, results[3])
  lower2 <- c(lower2, results[7])
  lower3 <- c(lower3, results[11])
  upper1 <- c(upper1, results[4])
  upper2 <- c(upper2, results[8])
  upper3 <- c(upper3, results[12])
  flags <- c(flags, results[14])
}

### True mean
true_mean <- mean(c(1.462, 4.260, 5.552))

### Output dataset
stats <- data.frame(case = c(1,2,3,4),
                    true_mean = rep(true_mean, 4),
                    est = c(median(unlist(estimates1)),
                            median(unlist(estimates2)),
                            median(unlist(estimates3)),
                            median(unlist(estimates4))),
                    coverage = c(sum(true_mean >= lower1 & true_mean <= upper1)/nreps,
                                 sum(true_mean >= lower2 & true_mean <= upper2)/nreps,
                                 sum(true_mean >= lower3 & true_mean <= upper3)/nreps,
                                 "-"),
                    ASE = c(median(unlist(ase1)),
                            median(unlist(ase2)),
                            median(unlist(ase3)),
                            "-"),
                    ESE = c(sqrt(var(unlist(estimates1))),
                            sqrt(var(unlist(estimates2))),
                            sqrt(var(unlist(estimates3[flags == 1]))),
                            sqrt(var(unlist(estimates4)))),
                    RMSE = c(sqrt(mean((unlist(estimates1)- true_mean)^2)),
                             sqrt(mean((unlist(estimates2)- true_mean)^2)),
                             sqrt(mean((unlist(estimates3)- true_mean)^2)),
                             sqrt(mean((unlist(estimates4)- true_mean)^2))))

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
ase1 <- c()
ase2 <- c()
ase3 <- c()
ase4 <- c()
lower1 <- c()
lower2 <- c()
lower3 <- c()
lower4 <- c()
upper1 <- c()
upper2 <- c()
upper3 <- c()
upper4 <- c()
flags <- c()

nreps <- 2000
for (i in 1:nreps){
  results <- run_sim_pstrat(n_per_wave = 80)
  estimates1 <- c(estimates1, results[1])
  estimates2 <- c(estimates2, results[5])
  estimates3 <- c(estimates3, results[9])
  estimates4 <- c(estimates4, results[13])
  ase1 <- c(ase1, results[2])
  ase2 <- c(ase2, results[6])
  ase3 <- c(ase3, results[10])
  lower1 <- c(lower1, results[3])
  lower2 <- c(lower2, results[7])
  lower3 <- c(lower3, results[11])
  upper1 <- c(upper1, results[4])
  upper2 <- c(upper2, results[8])
  upper3 <- c(upper3, results[12])
  flags <- c(flags, results[14])
}

### True mean
true_mean <- mean(c(1.462, 4.260, 5.552))

### Output dataset
stats <- data.frame(case = c(1,2,3,4),
                    true_mean = rep(true_mean, 4),
                    est = c(median(unlist(estimates1)),
                            median(unlist(estimates2)),
                            median(unlist(estimates3)),
                            median(unlist(estimates4))),
                    coverage = c(sum(true_mean >= lower1 & true_mean <= upper1)/nreps,
                                 sum(true_mean >= lower2 & true_mean <= upper2)/nreps,
                                 sum(true_mean >= lower3 & true_mean <= upper3)/nreps,
                                 "-"),
                    ASE = c(median(unlist(ase1)),
                            median(unlist(ase2)),
                            median(unlist(ase3)),
                            "-"),
                    ESE = c(sqrt(var(unlist(estimates1))),
                            sqrt(var(unlist(estimates2))),
                            sqrt(var(unlist(estimates3[flags == 1]))),
                            sqrt(var(unlist(estimates4)))),
                    RMSE = c(sqrt(mean((unlist(estimates1)- true_mean)^2)),
                             sqrt(mean((unlist(estimates2)- true_mean)^2)),
                             sqrt(mean((unlist(estimates3)- true_mean)^2)),
                             sqrt(mean((unlist(estimates4)- true_mean)^2))))

# stats$bias <- stats$est - true_mean
stats80 <- stats
stats80
