#' Select Sampling Units based on Stratified Random Sampling
#'
#' Given one dataframe indicating which stratum each population unit belongs to and a second specifying the n allocated to each stratum, `sample_strata` selects the units to sample by selecting a random sample of the desired size within each stratum. The second dataframe specifying the n allocated to each stratum is often the output of `allocate_wave` or `optimum_allocation`.
#' @param data1 A data frame or matrix with one row for each sampling unit in the population, one column specifying each unit's stratum, and one column with a unique identifier for each unit.
#' @param strata1 a character string specifying the name of column in `data1` which indicates the stratum that each unit belongs to.
#' @param id a character string specifying the name of the column in `data1` which uniquely identifies each unit.
#' @param data2 a dataframe or matrix with one row for each stratum that subdivides the population, one column specifying the stratum name, and one column indicating the number of samples allocated to each stratum.
#' @param strata2 a character string specifying the name of the column in `data2` which indicates the stratum. Defaults to "strata".
#' @param n_allocated a character string specifying the name of the column in `data2` which indicates the n allocated to each stratum. Defaults to "n_to_sample".
#' @export
#' @return returns a `data1` as a dataframe with a new column containing a binary (1/0) indicator of whether each unit should be sampled.

sample_strata <- function(data1, strata1, id, data2, strata2, n_allocated){
  if(is.matrix(data1)|is.matrix(data2)){
    data1 <- as.data.frame(data1)
    data2 <- as.data.frame(data2)
  }
  if(is.data.frame(data1) == FALSE|is.data.frame(data2) == FALSE){
    stop("'data1' and 'data2' must be a dataframe or matrix with named columns")
  }
  if(any(c(strata1, id) %in% names(data1) == FALSE)){
    stop("'strata1' and 'id' must be strings matching a column name of 'data1'")
  }
  if(any(c(strata2, n_allocated) %in% names(data2) == FALSE)){
    stop("'strata2' and 'n_allocated' must be strings matching a column name of 'data2'")
  }
  if(length(unique(data2[,strata2])) != length(data2[,strata2])){
    stop("'data2' may only conatine one row per stratum")
  }
  if(any(data2[,strata2] %in% data1[,strata1] == FALSE)){
    stop("strata names in 'data2' must all match strata names in 'data1'.")
  }
  sampled_ids <- list()
  for (i in 1:nrow(data2)){
    stratum <- data2[,strata2][i]
    strata_data <- data1[data1[,strata1] == stratum,c(id,strata1)]
    sampled_ids[[i]] <- sample(x = strata_data[,id], size = data2[,n_allocated][i])
  }
  sampled_ids <- unlist(sampled_ids)
  names(data1)[names(data1) == id] <- "id"
  output_df <- data1 %>%
    dplyr::mutate(sample_indicator = ifelse(id %in% sampled_ids, 1, 0))
  return(output_df)
}
