#' Split Strata
#'
#' Splits strata
#' @param data
#' @param strata a character string or vector of character strings specifying the columns that are being used to create the strata, leaving out the column that should be split.
#' @param split a character string specifying the column to split on.
#'

split_strata <- function(data, y, strata, split, split_perc = 0.5){
  if(is.numeric(data[,split])) {
    names(data)[names(data) == split] <- "split_col"
    cut_point <- sort(stats::quantile(data[,"split_col"],split_perc)) #Find cut points
    data <- data %>%
      dplyr::mutate(split_updated = ifelse(split_col < cut_point[1],
                                           paste(split,
                                                 paste("[",min(data$split_col), ",",cut_point[1],"]", sep = ""),
                                                 sep = "_"),
                                           paste(split,
                                                 paste("(",cut_point[1], ",",max(data$split_col),"]", sep = ""),
                                                 sep = "_")))
    if (length(cut_point) >1){
      cut_point <- c(cut_point,max(data$split_col))
      for (i in 2:length(cut_point)) {
        data <- data %>%
          mutate(split_updated = ifelse(split_col > cut_point[i-1] & split_col <= cut_point[i],
                                        paste(split,
                                              paste("(",cut_point[i-1], ",",cut_point[i],"]", sep = ""),
                                              sep = "_"),
                                        split_updated))
      }
    }
    #data <- data %>%
      #dplyr::mutate(split_updated = ifelse(split_col > cut_point,
                                           #paste(split,"under",cut_point, sep = "_"),
                                           #paste(split,"over", cut_point, sep = "_")))
    strata <- enquo(strata)
    group <- interaction(dplyr::select(data,!!strata, -split_col, split_updated))
    output_df <- cbind(group, data)
    return(output_df)
    #df <- optimall::optimal_allocation(data, strata = group, y = y, nsample = 10)
  }
  if (is.numeric(data[,split] == F)){
    names(data)[names(data) == split] <- "split_col"
    strata <- enquo(strata)
    group <- interaction(dplyr::select(data,!!strata, split_col))
    output_df <- cbind(group, data)
    return(output_df)
  }
}
