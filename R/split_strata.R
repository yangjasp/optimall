#' Split Strata
#'
#' Splits pre-defined sampling strata into smaller ones based on values of a continuous or categorical variable.
#' @param data a dataframe or matrix with one row for each sampling unit, one column specifying each unit's current stratum, one column containing the continuous or categorical values that will define the split, and any other relevant columns.
#' @param strata a character string specifying the name of the column that defines each unit's current strata.
#' @param split the name of the stratum to be split, exactly as it appears in `strata`. Defaults to NULL, which indicates that all strata in \code{strata} will be split.
#' @param split_at the percentile, value, or name(s) which \code{split_var} should be split at. The interpretation of this input depends on \code{type}. For \code{"quantile"} types, input must be between \code{0} and \code{1}. Defaults to \code{0.5} (median).
#' @param type a character string specifying how the function should interpret the \code{split_at} argument. Must be one of:
#'\itemize{
#'\item \code{"global quantile"}, the default, splits the strata at the quantiles specified in \code{split_at} defined along the entire, unfiltered \code{split_var} column .
#'\item \code{"local quantile"} splits the strata at the quantiles specified in \code{split_at} defined along the filtered \code{split_var} column which only includes units in the stratum being split.
#'\item \code{"value"} splits the strata at the values specified in \code{split_at} along \code{split_var} column.
#'\item \code{"categorical"} splits the strata into two new strata, one that contains each unit where \code{split_var} matches an input of \code{split_at}, and a second that contains every other unit.
#'}
#' @examples
#' x <- split_strata(iris, "Sepal.Length", strata = c("Species"),split = "setosa", split_var = "Sepal.Width", split_at = c(0.5), type = "global quantile")
#' @export
#' @return Returns the input dataframe with a new column named 'new_strata' that holds the name of the stratum that each sample belongs to after the split. The column containing the previous strata names is retained and given the name "old_strata".


split_strata <- function(data, strata, split = NULL, split_var, type = "global quantile", split_at = .5 ){
  if(is.matrix(data)){
    data <- as.data.frame(data)
  }
  if(is.data.frame(data) == FALSE){
    stop("'data' must be a dataframe or matrix with named columns")
  }
  if (split_var %in% names(data) == FALSE){
    stop("'split_var' must be a string matching a column name of 'data'")
  }
  if (all(strata %in% names(data)) == FALSE) {
    stop("'Strata' must be a string or vector of strings matching column names of data.")
  }
  if (all(strata != "old_strata") & "old_strata" %in% names(data)){
    data <- dplyr::select(data, -old_strata) #fixes error from assigning duplicate names
  }
  if(length(strata) > 1){
    strata_q <- enquo(strata)
    strata_interact <- data %>%
      dplyr::select(!!strata_q)
    strata_interact <- interaction(strata_interact)
    data <- cbind(data, strata_interact)
    data <- dplyr::select(data, -!!strata_q)
    names(data)[names(data) == "strata_interact"] <- "old_strata"
  } else{
  names(data)[names(data) == strata] <- "old_strata"
  }
  if (is.null(split) == FALSE){
  split <- match.arg(split, unique(data[,"old_strata"]))
    if(split %in% data[,"old_strata"] ==  FALSE){
      stop(paste(paste("'",split,"'",sep = ""), "does not match any value in 'strata'"))
    }
  }
  names(data)[names(data) == split_var] <- "split_variable"
  data$old_strata <- as.character(data$old_strata)
  type <- match.arg(type, c("global quantile", "local quantile", "value", "categorical"))
  if(type %in% c("global quantile","local quantile", "value","categorical") == FALSE){
    stop("'type' must be one of 'global quantile', 'local quantile', 'value', 'categorical'")
  }
  if(type %in% c("global quantile","local quantile", "value")){
    if (is.numeric(data$split_variable) == F){
      stop("'split_var' must be a column of 'data' holding numeric values. If you want to split on a categorical variable, use type = 'categorical'." )
    }
    if (type == "global quantile" | (is.null(split) & type == "local quantile")){
      cut_point <- sort(stats::quantile(data[,"split_variable"],
                                        split_at)) #Find cut points
    }
    if (type == "local quantile" & is.null(split) == FALSE){
      cut_point <- sort(stats::quantile(data[data$old_strata == split,"split_variable"],split_at))

    }
    if (type == "value"){
      cut_point <- sort(split_at)
      if(is.null(split) == FALSE){
        if(any(split_at < min(data[data$old_strata == split,"split_variable"])) | any(split_at > max(data[data$old_strata == split,"split_variable"]))){
          warning("value(s) of 'split_at' are outside of the range of values in 'split'")
        }
      }
      if(is.null(split)){
        if(any(split_at < min(data$split_variable)) | any(split_at > max(data$split_variable))){
          warning("value(s) of 'split_at' are outside of the range of values in 'split'")
        }
      }
    }
    if(is.null(split) == FALSE){
      data_filtered <- data %>%
        dplyr::filter(old_strata == split)
    }
    if(is.null(split) == TRUE){
      data_filtered <- data
    }

    if (length(cut_point) == 1){
      data_filtered <- data_filtered %>%
        dplyr::mutate(split_var_updated = ifelse(split_variable <= cut_point[1],
                                                 paste(split_var,
                                                       paste("[",round(min(data_filtered$split_variable), digits = 2), ",",round(cut_point[1], digits = 2),"]", sep = ""),
                                                       sep = "_"),
                                                 paste(split_var,
                                                       paste("(", round(cut_point[1], digits = 2), ",",round(max(data_filtered$split_variable), digits = 2),"]", sep = ""),
                                                       sep = "_")))
    }
    if (length(cut_point) >1){
      cut_point <- c(min(data_filtered$split_variable),
                     cut_point,
                     max(data_filtered$split_variable))
      data_filtered$split_var_updated <- data_filtered$split_variable
      data_filtered <- data_filtered %>%
        mutate(split_var_updated = ifelse(split_variable <= cut_point[2],
                                          paste(split_var,
                                                paste("[",round(cut_point[1], digits = 2), ",",round(cut_point[2], digits = 2),"]", sep = ""),
                                                sep = "_"),
                                          "other"
                                          ))
      for (i in 3:length(cut_point)) {
        data_filtered <- data_filtered %>%
          mutate(split_var_updated = ifelse(split_variable > cut_point[1] & split_variable > cut_point[i-1] & split_variable <= cut_point[i],
                                            paste(split_var,
                                                  paste("(",round(cut_point[i-1], digits = 2), ",",round(cut_point[i], digits = 2),"]", sep = ""),
                                            sep = "_"),
                                            split_var_updated))
      }
    }
    new_strata <- interaction(dplyr::select(data_filtered,old_strata,split_var_updated))
    small_df <- cbind(new_strata, data_filtered)
    small_df <- dplyr::select(small_df,-split_var_updated)
  }
  if (type == "categorical"){
    data$split_variable <- as.character(data$split_variable)
    if (is.null(split) == TRUE){
      data_filtered <- data
    } else{
      data_filtered <- dplyr::filter(data, old_strata == split)
    }
    data_filtered <- data_filtered %>%
      dplyr::mutate(split_var_updated = ifelse(split_variable %in% split_at,"1","0"))
    new_strata <- interaction(dplyr::select(data_filtered,old_strata,split_var_updated))
    data_filtered <- cbind(new_strata, data_filtered)
    small_df <- dplyr::select(data_filtered, -split_var_updated)
  }
  if (is.null(split)){
    output_df <- small_df
  }
  if (is.null(split) == FALSE){
    output_df <- data %>%
      dplyr::filter(old_strata != split) %>%
      dplyr::mutate(new_strata = old_strata)
    output_df <- rbind(output_df, small_df)
  }
  names(output_df)[names(output_df) == "split_variable"] <- split_var
  column_names_other <- names(output_df)[names(output_df) != "old_strata" & names(output_df) != "new_strata"]
  column_names_other <- enquo(column_names_other)
  output_df <- dplyr::select(output_df, new_strata, old_strata, !!column_names_other )
  return(output_df)
}
