timestep.hours <- function(datetime1, datetime2){
  if(tibble::is_tibble(datetime1) | tibble::is_tibble(datetime2)){
    stop("This function does not work with tibbles, please convert to data frame object.")
  }
  dt  <- difftime(datetime1, datetime2, units='hours')
  dt <- abs(as.numeric(dt))
  return(dt)
}
