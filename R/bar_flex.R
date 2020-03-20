#' bar_flex
#'
#' @param bouts_values ...
#' @param bouts_lengths ...
#' @param f ...
#' @param bts ...
#' @return barcodes
#' @export

# rewrite teh sequencing
bar_flex<- function(bouts_values,bouts_lengths,f, bts){
  BLcopy = bouts_lengths
  btss=bts*f # probably better to simply ask for btss as impute
  # change lengths by 4 classes:
  bouts_lengths[which(BLcopy >= btss[1] & BLcopy < btss[2])] = 1
  bouts_lengths[which(BLcopy >= btss[2] & BLcopy < btss[3])] = 2
  bouts_lengths[which(BLcopy >= btss[3] & BLcopy < btss[4])] = 3
  bouts_lengths[which(BLcopy >= btss[4])] = 4
  # generate barcodes
  dff <- df_generation(bouts_values,bouts_lengths)
  # reformat barcodes to be time series
  barcodes= rep(dff$code, times = BLcopy)
  return(barcodes)
}

