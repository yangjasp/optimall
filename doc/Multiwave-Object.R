## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

#packages
library(optimall)
library(DiagrammeR)
library(dplyr)

#data
data(MatWgt_Sim, package = "optimall")
MatWgt_Sim <- MatWgt_Sim %>%
  dplyr::mutate(race = 
                  dplyr::case_when(race == "Asian" ~ "Other",
                                   race == "Other" ~ "Other",
                                   race == "White" ~ "White",
                                   race == "Black" ~ "Black"))
phase1 <- dplyr::select(MatWgt_Sim, -mat_weight_true)

phase1$strata <- phase1$race #initialize a strata column first
set.seed(452)
phase1 <- split_strata(data = phase1, strata = "strata", split = NULL, 
                       split_var = "mat_weight_est", 
                       type = "global quantile", 
                       split_at = c(0.25,0.75),
                       trunc = "MWC_est")
#Trunc argument specifies how to refer to mat_weight_est in new strata names

#Make Phase 1 data dict
phase1_data_dictionary <- data.frame("Variable" = c("id",
                                                    "race",
                                                    "mat_weight_est",
                                                    "diabetes"),
                                     "Description" = c("unique identifier",
                                                       "race of mother",
                                                       "error-prone estimate of maternal weight change during pregnancy",
                                                       "1/0 indicator for diabetes in the mother during pregnancy"))

## -----------------------------------------------------------------------------
MySurvey <- new_multiwave(phases = 2, waves = c(1,3))

## -----------------------------------------------------------------------------
#To access overall metadata
MySurvey@metadata

#To write overall metadata. We may want to include a title.
MySurvey@metadata <- list(title = "Maternal Weight Survey")

#To access Phase 2 metadata
MySurvey@phases$phase2@metadata

#To access Phase 2, Wave 2 design
MySurvey@phases$phase2@waves$wave2@design

## -----------------------------------------------------------------------------
#To access overall metadata
get_data(MySurvey, phase = NA, slot = "metadata")

#To write overall metadata
get_data(MySurvey, phase = NA, slot = "metadata") <- list(title = "Maternal Weight Survey")

#To access Phase 2 metadata
get_data(MySurvey, phase = 2, slot = "metadata")

#To access Phase 2, Wave 2 design
get_data(MySurvey, phase = 2, wave = 2, slot = "design")

## -----------------------------------------------------------------------------
head(phase1)

get_data(MySurvey, phase = 1, slot = "data") <- phase1

#Make Phase 1 data dict
phase1_data_dictionary <- data.frame(
  "Variable" = c( "id", "race", "mat_weight_est", "diabetes", "obesity"),
  "Description" = c("unique identifier", 
                    "race of mother",
                    "error-prone estimate of maternal weight change 
                    during pregnancy",
                    "1/0 indicator for diabetes in the mother during 
                    pregnancy",
                    "1/0 indicator for childhood obesity in child"))

head(phase1_data_dictionary)

get_data(MySurvey, phase = 1, slot = "metadata") <- list(data_dict = phase1_data_dictionary)

## ---- eval = FALSE------------------------------------------------------------
#  multiwave_diagram(MySurvey)

## ---- fig.align='center', fig.height = 5.5, fig.width = 5.5, echo= FALSE, eval = FALSE----
#  multiwave_diagram(MySurvey)

## -----------------------------------------------------------------------------

# Initialize Multiwave
IrisSurvey <- new_multiwave(phases = 2, waves = c(1,3))

# Add id column to iris dataset
iris <- cbind(datasets::iris, id = 1:150)

# To place iris data in Phase 1
get_data(IrisSurvey, phase = 1, slot = "data") <-
    subset(iris, select = -Sepal.Width)

## -----------------------------------------------------------------------------
IrisSurvey <- apply_multiwave(IrisSurvey, phase = 2, wave = 1,
                            fun = "optimum_allocation",
                            strata = "Species", y = "Sepal.Length",
                            nsample = 30, method = "WrightII")

## -----------------------------------------------------------------------------
get_data(IrisSurvey, phase = 2, slot = "metadata") <-
  list(strata = "Species")

## -----------------------------------------------------------------------------
IrisSurvey <- apply_multiwave(IrisSurvey, phase = 2, wave = 1,
                            fun = "optimum_allocation",
                            y = "Sepal.Length",
                            nsample = 30, method = "WrightII")

## -----------------------------------------------------------------------------
get_data(IrisSurvey, phase = 2, wave = 1, slot = "design")

## -----------------------------------------------------------------------------
IrisSurvey <- apply_multiwave(IrisSurvey, phase = 2, wave = 1,
                            fun = "sample_strata", id = "id",
                            design_strata = "strata",
                            n_allocated = "stratum_size")

## -----------------------------------------------------------------------------
get_data(IrisSurvey, phase = 2, wave = 1, slot = "samples")

## -----------------------------------------------------------------------------
get_data(IrisSurvey, phase = 2, wave = 1, slot = "sampled_data") <-
  iris[iris$id %in% get_data(IrisSurvey,
                             phase = 2,
                             wave = 1,
                             slot = "samples"),
       c("id", "Sepal.Width")]

## -----------------------------------------------------------------------------
IrisSurvey <- merge_samples(IrisSurvey, phase = 2, wave = 1,
                          id = "id", sampled_ind = "sampled_phase2")

## -----------------------------------------------------------------------------
head(get_data(IrisSurvey, phase = 2, wave = 1, slot = "data"))

## -----------------------------------------------------------------------------
# Metadata for Phase 2 including description,
# and column names to be used in function calls.
# Note that each element name corresponds to at least one argument of a 
# function that will be called later on in the multi-wave workflow.
get_data(MySurvey, phase = 2, slot = "metadata") <- 
  list(description = "Phase 2 of Maternal Weight Survey in which we
       seek to validate 750 samples across three waves.",
       strata = "new_strata", # strata column in data (used in multiple funcs)
       id = "id", # name of id column (used in sample_strata and merge_samples)
       y = "mat_weight_true", # col for which to minimize variance 
                                  # (used in optimum_allocation)
       design_strata = "strata", # strata column in designs (used for sample_strata)
       n_allocated = "n_to_sample" # n allocated to strata in designs
                                    # (used for sample_strata)
       )

# Then, metadata for Wave 1 of Phase 2 
get_data(MySurvey, phase = 2, wave = 1, slot = "metadata") <- 
  list(description = "First wave of 250 
       sampled using proportional sampling")

## -----------------------------------------------------------------------------
#Design for Wave 1
get_data(MySurvey, phase = 2, wave = 1, slot = "design") <- data.frame(
  strata = names(table(phase1$new_strata)), 
  n_to_sample = round(250.3*as.vector(table(phase1$new_strata))/10335)
  ) #250.3 to make sure 250 samples after rounding

get_data(MySurvey, phase = 2, wave = 1, slot = "design")

## -----------------------------------------------------------------------------

# Get list of ids to sample using stratified random sampling 
# without replacement
set.seed(456)
MySurvey <- apply_multiwave(MySurvey, phase = 2, wave = 1,
                            fun = "sample_strata",
                            strata = "new_strata",
                            id = "id",
                            wave2a = NULL, #No one has been sampled yet
                            design_strata = "strata", #from design
                            n_allocated = "n_to_sample"
                            )
# check that it worked

head(get_data(MySurvey, phase = 2, wave = 1, slot = "samples"))
length(get_data(MySurvey, phase = 2, wave = 1, slot = "samples"))

# But, notice that we had already specified most of the arguments to 
# sample_strata in the phase metadata. So, we can get the same result
# with a much shorter call to the function
set.seed(456)

MySurvey <- apply_multiwave(MySurvey, phase = 2, wave = 1, fun = "sample_strata")

ids_wave1 <- get_data(MySurvey, phase = 2, wave = 1, slot = "samples") 

#Check that  it gives same results
head(ids_wave1)
length(ids_wave1)

## -----------------------------------------------------------------------------

# We can use these ids to get the data:
get_data(MySurvey, phase = 2, wave = 1, slot = "sampled_data") <- 
  MatWgt_Sim[MatWgt_Sim$id %in% ids_wave1, c("id", "mat_weight_true")]

## -----------------------------------------------------------------------------
MySurvey <- apply_multiwave(MySurvey, phase = 2, wave = 1, fun = "merge_samples",
                            sampled_ind = "already_sampled_ind")

## ---- include = F-------------------------------------------------------------
# Old, before added sampled_ind you had to specify yourself

get_data(MySurvey, phase = 2, wave = 1) <- 
  get_data(MySurvey, phase = 2, wave = 1) %>%
  dplyr::mutate(already_sampled_ind = 
                  ifelse(id %in% 
                           get_data(MySurvey, 
                                    phase = 2, 
                                    wave = 1,
                                    slot = "samples"), 1, 0))

## ---- eval = FALSE------------------------------------------------------------
#  multiwave_diagram(MySurvey)

## ---- fig.align='center', fig.height = 5.5, fig.width = 5.5, echo= FALSE, eval = FALSE----
#  multiwave_diagram(MySurvey)

## -----------------------------------------------------------------------------
MySurvey <- apply_multiwave(MySurvey, 
                                    phase = 2, 
                                    wave = 2, 
                                    fun = "allocate_wave",
                                    nsample = 250,
                                    already_sampled = "already_sampled_ind")

get_data(MySurvey, phase = 2, wave = 2, slot = "design")

