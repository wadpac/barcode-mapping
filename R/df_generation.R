#' df_generation
#'
#' @param bouts_values ...
#' @param bouts_lengths ...
#' @return df
#' @export
df_generation<- function(bouts_values, bouts_lengths){
  df <- data.frame(bvalue = bouts_values,
                   blength = bouts_lengths,
                   code = rep(NA,length(bouts_lengths)))

  df$code[df$bvalue == 0 & df$blength == 1] <- 0
  df$code[df$bvalue == 0 & df$blength == 2] <- 0
  df$code[df$bvalue == 0 & df$blength == 3] <- 0
  df$code[df$bvalue == 0 & df$blength == 4] <- 0
  
  df$code[df$bvalue == 1 & df$blength == 1] <- 3
  df$code[df$bvalue == 1 & df$blength == 2] <- 3
  df$code[df$bvalue == 1 & df$blength == 3] <- 2
  df$code[df$bvalue == 1 & df$blength == 4] <- 1
  
  df$code[df$bvalue == 2 & df$blength == 1] <- 4
  df$code[df$bvalue == 2 & df$blength == 2] <- 4
  df$code[df$bvalue == 2 & df$blength == 3] <- 5
  df$code[df$bvalue == 2 & df$blength == 4] <- 6
  
  df$code[df$bvalue == 3 & df$blength == 1] <- 7
  df$code[df$bvalue == 3 & df$blength == 2] <- 8
  df$code[df$bvalue == 3 & df$blength == 3] <- 9
  df$code[df$bvalue == 3 & df$blength == 4] <- 9
  
  df$code[df$bvalue == 4 & df$blength == 1] <- 10
  df$code[df$bvalue == 4 & df$blength == 2] <- 11
  df$code[df$bvalue == 4 & df$blength == 3] <- 12
  df$code[df$bvalue == 4 & df$blength == 4] <- 12
  
  df$code[df$bvalue == 5 & df$blength == 1] <- 15
  df$code[df$bvalue == 5 & df$blength == 2] <- 16
  df$code[df$bvalue == 5 & df$blength == 3] <- 17
  df$code[df$bvalue == 5 & df$blength == 4] <- 18
  return(df)
}

