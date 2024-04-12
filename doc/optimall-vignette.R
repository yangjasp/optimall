## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(optimall)
library(DiagrammeR)
library(survey)
library(datasets)
library(dplyr)

data(MatWgt_Sim, package = "optimall")

## -----------------------------------------------------------------------------
library(optimall)
library(datasets)

iris <- datasets::iris
table(iris$Species)
iris2 <- split_strata(data = iris,
                     strata = "Species",
                     split = c("setosa", "virginica"), 
                     split_var = "Sepal.Width",
                     split_at = c(0.5), type = "local quantile")
table(iris2$new_strata)

## -----------------------------------------------------------------------------
iris3 <- merge_strata(data = iris,
                     strata = "Species",
                     merge = c("setosa", "versicolor"), 
                     name = "set_or_vers")
table(iris3$new_strata)

## ----Example 1----------------------------------------------------------------
sampling_design <- optimum_allocation(data = iris, strata = "Species", 
                                      y = "Sepal.Width",
                                      nsample = 40, method = "WrightII")
sampling_design

## -----------------------------------------------------------------------------
iris_summary <- data.frame(strata = unique(iris$Species),
                           size = c(50, 50, 50),
                           sd = c(0.3791, 0.3138, 0.3225))

optimum_allocation(data = iris_summary, strata = "strata",
                   sd_h = "sd", N_h = "size", 
                   nsample = 40, method = "WrightII")

## -----------------------------------------------------------------------------
iris$id <- 1:150
set.seed(743)

iris <- sample_strata(data = iris, strata = "Species", id = "id",
                               design_data = sampling_design, design_strata = "strata",
                               n_allocated = "stratum_size")

## -----------------------------------------------------------------------------
head(iris)

## -----------------------------------------------------------------------------
ids_to_sample <- iris$id[iris$sample_indicator == 1]
head(ids_to_sample)
length(ids_to_sample)

## ---- fig.align='center', echo=FALSE, include = FALSE, eval=FALSE-------------
#  DiagrammeR::grViz("digraph {
#    graph [layout = dot, rankdir = TB]
#  
#    node [shape = rectangle, fixedsize = true, width = 8.5, height = 1.2, fontname = Helvetica, fontsize  = 20]
#    rec1 [label = <<b>Phase-I:</b><br/>A large sample of inexpensive variables, <br/> define strata based on results.>]
#    rec2 [label = <<b>Phase-IIa:</b><br/>Initial stratified sample of expensive variable taken without <br/>optimum allocation>]
#    rec3 [label = <<b>Phase-IIb:</b><br/>Second sample of expensive variable with optimum allocation <br/> based on Phase-IIa results>]
#    rec4 [label = <<b>Phase-IIc - Phase-IIfinal:</b><br/>Combine previous waves of Phase-II together and repeat <br/> sampling with re-calculated optimum allocation until desired total <br/> sample size for expensive variable is reached at Phase-IIfinal>]
#  
#    # edge definitions with the node IDs
#    rec1 -> rec2 -> rec3 -> rec4
#  
#    }",
#    height = 250, width = 700)

## -----------------------------------------------------------------------------
# Set up Wave 1
wave1_design <- data.frame(strata = c("setosa",
                                       "virginica",
                                      "versicolor"),
                           stratum_size = c(7, 16, 7))

# Collect Sepal.Width from only the 30 samples
phase1_data <- subset(datasets::iris, select =  -Sepal.Width)

phase1_data$id <- 1:nrow(phase1_data) #Add id column

set.seed(234)
phase1_data <- sample_strata(data = phase1_data, 
                               strata = "Species", id = "id",
                               design_data = wave1_design,
                               design_strata = "strata",
                               n_allocated = "stratum_size")

wave1_ids <- iris$id[phase1_data$sample_indicator == 1]

wave1_sampled_data <- iris[iris$id %in% wave1_ids, c("id","Sepal.Width")]

wave1_data <- merge(phase1_data, wave1_sampled_data, by = "id", 
                    no.dups =  TRUE, all.x = TRUE)

# We have our 30 samples
table(is.na(wave1_data$Sepal.Width), wave1_data$Species)

# Now, allocate the next 10:
wave2_design <- allocate_wave(data = wave1_data,
                              strata = "Species", 
                              y = "Sepal.Width",
                              already_sampled = "sample_indicator",
                              nsample = 10, 
                              detailed = TRUE)

wave2_design

## -----------------------------------------------------------------------------
 # Run sample_strata
wave2_data <- sample_strata(data = wave1_data, strata = "Species",
                            id = "id", already_sampled = "sample_indicator",
                            design_data = wave2_design)

 # Extract the 10 ids to sample
wave2_ids <- iris$id[wave2_data$sample_indicator == 1]

wave2_ids

## ----first view of data-------------------------------------------------------
data(MatWgt_Sim, package = "optimall")
head(MatWgt_Sim)

## -----------------------------------------------------------------------------

# Get phase 1 data, remove mat_weight_true which we assume is unknown
phase1 <- subset(MatWgt_Sim, select =  -mat_weight_true)
dim(phase1)

## -----------------------------------------------------------------------------
phase1$strata <- phase1$race #initialize a strata column first

# Merge the smallest race categories

phase1 <- merge_strata(data = phase1, 
                          strata = "race",
                          merge = c("Other","Asian"),
                          name = "Other")

phase1 <- split_strata(data = phase1, strata = "new_strata", split = NULL, 
                       split_var = "mat_weight_est", 
                       type = "global quantile", 
                       split_at = c(0.25,0.75),
                       trunc = "MWC_est")
# Trunc argument specifies how to refer to mat_weight_est in new strata names

## -----------------------------------------------------------------------------
head(phase1)
table(phase1$new_strata) # 9 strata

## -----------------------------------------------------------------------------
phase2a_design <- data.frame(
  strata_name = names(table(phase1$new_strata)), 
  strata_prop = as.vector(table(phase1$new_strata))/10335,
  strata_n = round(250.3*as.vector(table(phase1$new_strata))/10335)
  ) # 250.3 to make sure 250 samples after rounding
sum(phase2a_design$strata_n) 
phase2a_design

## -----------------------------------------------------------------------------
phase1 <- sample_strata(data = phase1, strata = "new_strata", 
                        id = "id", design_data = phase2a_design, 
                        design_strata = "strata_name", 
                        n_allocated = "strata_n")
ids_to_sample2a <- phase1[phase1$sample_indicator == 1,"id"]
length(ids_to_sample2a) # 250 ids to sample

## -----------------------------------------------------------------------------
phase2a_samples <- MatWgt_Sim[MatWgt_Sim$id %in% ids_to_sample2a, 
                              c("id","mat_weight_true")]

phase2a <- merge(phase1, phase2a_samples, by = "id",
                 no.dups =  TRUE, all.x = TRUE)
names(phase2a)
table(is.na(phase2a$mat_weight_true)) 

## -----------------------------------------------------------------------------
phase2b <- phase2a

# Add indicator for units that were already sampled
phase2b$already_sampled <- ifelse(phase2b$id %in% ids_to_sample2a, 1, 0)

# Make design 
phase2b_design <- allocate_wave(data = phase2b, strata = "new_strata",
                             y = "mat_weight_true",
                             already_sampled = "already_sampled", 
                             nsample = 250,
                             detailed = TRUE)
phase2b_design

## -----------------------------------------------------------------------------
phase2b <- split_strata(data = phase2b, 
                        split = "White.MWC_est_(9.75,15.06]",
                        strata = "new_strata", 
                        split_var = "mat_weight_est",
                        type = "local quantile", split_at = c(1/3,2/3))

## -----------------------------------------------------------------------------
phase2b_design <- allocate_wave(data = phase2b, 
                             strata = "new_strata",
                             y = "mat_weight_true",
                             already_sampled = "already_sampled",
                             nsample = 250)
phase2b_design

# Extract IDs to sample
ids_to_sample2b <- sample_strata(data = phase2b, 
                                 strata = "new_strata", 
                                 id = "id", 
                                 already_sampled = "already_sampled", 
                                 design_data = phase2b_design,
                                 design_strata = "strata", 
                                 n_allocated = "n_to_sample")
ids_to_sample2b <- 
  ids_to_sample2b[ids_to_sample2b$sample_indicator == 1,"id"]
length(ids_to_sample2b)

## ---- include=TRUE------------------------------------------------------------
# Take samples from true data
phase2b_samples <- MatWgt_Sim[MatWgt_Sim$id %in% ids_to_sample2b,
                              c("id","mat_weight_true")]

# Merge extracted samples with our data.
# Note we use dplyr's coalesce here to merge sampled mat_weight_true with 
# the mat_weight_true solumn already present in phase2b, but manner of 
# merging of samples may vary from project to project.
# For simpler merges, see 'Multiwave Object' vignette.

phase2b <- merge(phase2b, phase2b_samples, by = "id", 
                 no.dups = TRUE, all.x = TRUE)

library(dplyr)

phase2b$mat_weight_true <- 
  dplyr::coalesce(phase2b$mat_weight_true.x,
                  phase2b$mat_weight_true.y)
phase2b <- subset(phase2b, select = -c(mat_weight_true.x,
                                       mat_weight_true.y))

table(is.na(phase2b$mat_weight_true)) # All are NA besides already sampled.

## -----------------------------------------------------------------------------
phase2c <- phase2b

# Add indicator for units that were already sampled
phase2c$already_sampled <- ifelse(phase2b$id %in% c(ids_to_sample2a,
                                                    ids_to_sample2b), 
                                  1, 0)

# Make Design
phase2c_design <- allocate_wave(data = phase2c, strata = "new_strata",
                             y = "mat_weight_true",
                             already_sampled = "already_sampled", 
                             nsample = 250,
                             detailed = TRUE)
phase2c_design

# Find IDs to sample
ids_to_sample2c <- sample_strata(data = phase2c, 
                                 strata = "new_strata", 
                                 id = "id", 
                                 already_sampled = "already_sampled", 
                                 design_data = phase2c_design,
                                 design_strata = "strata", 
                                 n_allocated = "n_to_sample")
ids_to_sample2c <- 
  ids_to_sample2c[ids_to_sample2c$sample_indicator == 1,"id"]
length(ids_to_sample2c)

# Sample
phase2c_samples <- MatWgt_Sim[MatWgt_Sim$id %in% ids_to_sample2c,
                              c("id","mat_weight_true")]

# Add samples to phase2c dataset
phase2c <- merge(phase2c, phase2c_samples, by = "id", 
                 no.dups = TRUE, all.x = TRUE)
phase2c$mat_weight_true <- 
  dplyr::coalesce(phase2c$mat_weight_true.x,
                  phase2c$mat_weight_true.y)
phase2c <- subset(phase2c, select = -c(mat_weight_true.x,
                                       mat_weight_true.y))

