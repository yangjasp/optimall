## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(optimall)
library(survey)
library(datasets)
library(dplyr)
library(tidyr)
library(MASS)

data(MatWgt_Sim, package = "optimall")

## ----example, echo = FALSE, results = 'hide'----------------------------------
#####
## Generate data
#####
set.seed(1)
n <- c(rmultinom(1, 1000, c(1/3, 1/3, 1/3)))
col1 <- c(rep("setosa", times = n[1]),
          rep("versicolor", times = n[2]),
          rep("virginica", times = n[3]))
pl <- c(rnorm(n[1], 1.462, 0.432),
        rnorm(n[2], 4.260, 0.470),
        rnorm(n[3], 5.552, 0.552))
sl <- c(rnorm(n[1], pl[1:n[1]] * 3.35, 0.341),
        rnorm(n[2], pl[(n[1]+1):(n[1]+n[2])] * 1.32, 0.366),
        rnorm(n[3],  pl[(n[2]+1):1000] * 1.14, 0.302))
full_data <- data.frame("id" = 1:1000,
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
                          nsample = 50, method = "Neyman")

# get_mw(Survey, phase = 2, wave = 1, slot ="design")

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
                          nsample = 50, allocation_method = "Neyman",
                          already_sampled = "sampled_phase2")

# get_mw(phase = 2, wave = 2, slot = "design")

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
                          nsample = 50, allocation_method = "Neyman",
                          already_sampled = "sampled_phase2")

# get_mw(phase = 2, wave = 3, slot = "design")

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
                                 survey_data$id), , drop = FALSE] %>%
  dplyr::mutate(wave = case_when(sampled_wave2.1 == 1 ~ 1,
                                 sampled_wave2.2 == 1 ~ 2,
                                 sampled_wave2.3 == 1 ~ 3))

## -----------------------------------------------------------------------------
head(survey_data)

## -----------------------------------------------------------------------------
pst_design <- twophase(id = list(~id, ~id), strata = list(NULL, ~Species),
                       subset = ~as.logical(sampled_phase2),
                       data = survey_data, method = "simple")

## -----------------------------------------------------------------------------
pst_est <- svymean(~Petal.Length, design = pst_design)
pst_est

## -----------------------------------------------------------------------------
pst_design_rake <- calibrate(pst_design, ~Sepal.Length + Species,
                             phase = 2, calfun = "raking")
pst_est_rake <- svymean(~Petal.Length, design = pst_design_rake)
pst_est_rake

## -----------------------------------------------------------------------------
## Find denominator from eq. 6 for each wave, species
denom_data <- survey_data %>%
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

## -----------------------------------------------------------------------------
cp_design <- twophase(id = list(~id, ~id), strata = list(NULL, ~Species),
                      subset = ~as.logical(sampled_phase2),
                      data = survey_data, probs = list(NULL, ~denom))

cp_est <- svymean(~Petal.Length, design = cp_design)
cp_est[1]

## -----------------------------------------------------------------------------
phase2_data <- survey_data[survey_data$sampled_phase2 == 1,]

# Weight observations by denominator from equation 6
phase2_data$weighted_obs <- phase2_data$Petal.Length/phase2_data$denom

# Determine number of waves each stratum was sampled in  
sampled_waves <- denom_data[denom_data$sampling_prob > 0,]
n_waves_sampled <- table(sampled_waves$Species)

# Compute estimate for total in equation 6
total_est <- (sum(phase2_data[phase2_data$Species == "setosa",
                              "weighted_obs"])/ n_waves_sampled["setosa"] +
                sum(phase2_data[phase2_data$Species == "virginica",
                                "weighted_obs"])/n_waves_sampled["virginica"]+
                sum(phase2_data[phase2_data$Species == "versicolor",
                                "weighted_obs"])/n_waves_sampled["versicolor"])
names(total_est) <- "Petal.Length"

# Finally, compute the mean
cp_est <- total_est/nrow(survey_data) 
cp_est # this matches svymean() output above if all strata sampled in all waves

## -----------------------------------------------------------------------------
### Combine design dataframes for waves 1-3. 
design_all_waves <- dplyr::bind_rows(cbind(phase2_wave = 1, 
                                           get_mw(Survey, 2, 1, "design")),
                                     cbind(phase2_wave = 2, 
                                           get_mw(Survey, 2, 2, "design")),
                                     cbind(phase2_wave = 3, 
                                           get_mw(Survey, 2, 3, "design"))) %>%
  dplyr::mutate(n_to_sample = dplyr::coalesce(n_to_sample, stratum_size),
                nsample_prior = ifelse(is.na(nsample_prior), 
                                       0 , nsample_prior))

###
### Now add three columns to design: prob of two obs being sampled in a given
### wave, prob of neither being sampled in given wave, or prob of only (specific) 
### one being sampled in given wave
design_all_waves <- design_all_waves |>
  dplyr::mutate(both_pp = n_to_sample/(npop - nsample_prior)*
                  (n_to_sample-1)/(npop - nsample_prior - 1), #n/N*(n-1)/(N-1)
                onlyone_pp = n_to_sample/(npop - nsample_prior)*
                  (npop- nsample_prior- n_to_sample)/(npop - nsample_prior - 1),
                neither_pp = 
                  (npop- nsample_prior- n_to_sample)/(npop - nsample_prior)*
                  (npop- nsample_prior- n_to_sample-1)/(npop - nsample_prior - 1),
                single_prob = n_to_sample/(npop - nsample_prior)) 

### One row for each stratum
design_all_waves_wide <- design_all_waves %>%
  dplyr::select(phase2_wave, strata, both_pp, onlyone_pp, neither_pp, single_prob) %>%
  tidyr::pivot_wider(names_from = phase2_wave, values_from = c(both_pp, onlyone_pp,
                                                               neither_pp, single_prob))

### Compute pairwise probability of inclusion and variance contribution
### for each pair of Phase 2 samples
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
  dplyr::left_join(design_all_waves_wide, by = c("Species1" = "strata")) %>%
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

phase2_variance_est <- sum(pairwise_df$phase2_variance_contribution)/nrow(phase1_data)^2/3^2

##
## Final variance estimator combines the phase 2 variance that we just calculated with
## phase 1 variance (which was already calculated in post-stratified estimator)

### Extract phase 1 variance estimate from pst_est
phase1_variance_est <- attr(SE(pst_est), "phases")$phase1[1]

### Estimate variance with two-phase()
cp_est_ase <- sqrt(phase2_variance_est + phase1_variance_est)
cp_est_ase

## -----------------------------------------------------------------------------
pairwise_probs <- matrix(pairwise_df$pairwise_prob,
                         ncol = length(phase2_ids), byrow = TRUE)

phase2_data <- survey_data[survey_data$sampled_phase2 == 1,]
cp_design_phase2 <- svydesign(id = ~id,
                              data = phase2_data, probs = ~denom,
                              pps = ppsmat(pairwise_probs))

phase2_variance_est <- (SE(svytotal(~Petal.Length, design = cp_design_phase2))/nrow(phase1_data)/3)^2

cp_est_ase <- sqrt(phase2_variance_est + phase1_variance_est)
cp_est_ase

## -----------------------------------------------------------------------------
phase2_data <- survey_data[survey_data$sampled_phase2 == 1,]
phase2_data$strata_int <- paste0(phase2_data$Species, ".", phase2_data$wave)
svydesign_phase2 <- svydesign(data = phase2_data,
                              ids = ~id,
                              strata = ~strata_int,
                              probs = ~denom)

estimate_approx <- svytotal(~Petal.Length, svydesign_phase2)
estimate_approx[1]/nrow(phase1_data)/3
variance_approx <- sqrt(phase1_variance_est + (SE(estimate_approx)/nrow(phase1_data)/3)^2)
variance_approx

## ----appendix, ref.label = "example", echo=TRUE, eval=FALSE-------------------
#  #####
#  ## Generate data
#  #####
#  set.seed(1)
#  n <- c(rmultinom(1, 1000, c(1/3, 1/3, 1/3)))
#  col1 <- c(rep("setosa", times = n[1]),
#            rep("versicolor", times = n[2]),
#            rep("virginica", times = n[3]))
#  pl <- c(rnorm(n[1], 1.462, 0.432),
#          rnorm(n[2], 4.260, 0.470),
#          rnorm(n[3], 5.552, 0.552))
#  sl <- c(rnorm(n[1], pl[1:n[1]] * 3.35, 0.341),
#          rnorm(n[2], pl[(n[1]+1):(n[1]+n[2])] * 1.32, 0.366),
#          rnorm(n[3],  pl[(n[2]+1):1000] * 1.14, 0.302))
#  full_data <- data.frame("id" = 1:1000,
#                          "Species" = as.factor(col1),
#                          "Sepal.Length" = sl,
#                          "Petal.Length" = pl)
#  phase1_data <- full_data[,-4]
#  
#  ####
#  ## Multiwave object setup
#  ####
#  
#  Survey <- multiwave(phases = 2, waves = c(1, 3),
#                      phase1 = phase1_data)
#  
#  set_mw(Survey, phase = 2, slot = "metadata") <- list(id = "id",
#                                                       strata = "Species",
#                                                       design_strata = "strata",
#                                                       include_probs = TRUE)
#  
#  ####
#  ## Wave 1
#  ####
#  
#  ### Allocation: X-allocate with Phase 1 sepal length
#  Survey <- apply_multiwave(Survey, phase = 2, wave = 1,
#                            fun = "optimum_allocation",
#                            y = "Sepal.Length",
#                            nsample = 50, method = "Neyman")
#  
#  # get_mw(Survey, phase = 2, wave = 1, slot ="design")
#  
#  ### Select samples
#  Survey <- apply_multiwave(Survey, phase = 2, wave = 1,
#                            fun = "sample_strata",
#                            n_allocated = "stratum_size",
#                            probs = ~stratum_size/npop)
#  
#  ### "Collect" data
#  set_mw(Survey, phase = 2, wave = 1, slot = "sampled_data") <-
#    full_data[full_data$id %in% get_mw(Survey, phase = 2, wave = 1,
#                                       slot = "samples")$ids,
#              c("id", "Petal.Length")]
#  
#  Survey <- merge_samples(Survey, phase = 2, wave = 1)
#  
#  ####
#  ## Wave 2
#  ####
#  
#  ### Allocation: Neyman allocation with already-collected phase 2 data.
#  Survey <- apply_multiwave(Survey, phase = 2, wave = 2,
#                            fun = "allocate_wave",
#                            y = "Petal.Length",
#                            nsample = 50, allocation_method = "Neyman",
#                            already_sampled = "sampled_phase2")
#  
#  # get_mw(phase = 2, wave = 2, slot = "design")
#  
#  ### Select samples
#  Survey <- apply_multiwave(Survey, phase = 2, wave = 2,
#                            fun = "sample_strata",
#                            n_allocated = "n_to_sample",
#                            probs = ~n_to_sample/(npop - nsample_prior),
#                            already_sampled = "sampled_phase2")
#  
#  ### "Collect" data
#  set_mw(Survey, phase = 2, wave = 2, slot = "sampled_data") <-
#    full_data[full_data$id %in% get_mw(Survey, phase = 2, wave = 2,
#                                       slot = "samples")$ids,
#              c("id", "Petal.Length")]
#  Survey <- merge_samples(Survey, phase = 2, wave = 2)
#  
#  ####
#  ## Wave 3
#  ####
#  
#  ### Allocation: Neyman allocation with already-collected phase 2 data.
#  Survey <- apply_multiwave(Survey, phase = 2, wave = 3,
#                            fun = "allocate_wave",
#                            y = "Petal.Length",
#                            nsample = 50, allocation_method = "Neyman",
#                            already_sampled = "sampled_phase2")
#  
#  # get_mw(phase = 2, wave = 3, slot = "design")
#  
#  ### Select samples
#  Survey <- apply_multiwave(Survey, phase = 2, wave = 3,
#                            fun = "sample_strata",
#                            n_allocated = "n_to_sample",
#                            probs = ~n_to_sample/(npop - nsample_prior),
#                            already_sampled = "sampled_phase2")
#  
#  ### "Collect" data
#  set_mw(Survey, phase = 2, wave = 3, slot = "sampled_data") <-
#    full_data[full_data$id %in% get_mw(Survey, phase = 2, wave = 3,
#                                       slot = "samples")$ids,
#              c("id", "Petal.Length")]
#  Survey <- merge_samples(Survey, phase = 2, wave = 3)
#  
#  ### Final dataset for analysis
#  survey_data <- get_mw(Survey, phase = 2, wave = 3, slot = "data")
#  
#  ### Clean up for printing
#  survey_data <- survey_data[,c("id", "Species", "Sepal.Length",
#                                "Petal.Length", "sampled_phase2",
#                                "sampled_wave2.1", "sampled_wave2.2",
#                                "sampled_wave2.3", "sampling_prob")]
#  survey_data <- survey_data[order(-survey_data$sampled_phase2,
#                                   survey_data$id), , drop = FALSE] %>%
#    dplyr::mutate(wave = case_when(sampled_wave2.1 == 1 ~ 1,
#                                   sampled_wave2.2 == 1 ~ 2,
#                                   sampled_wave2.3 == 1 ~ 3))

