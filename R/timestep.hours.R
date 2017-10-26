timestep.hours <- function(datetime1, datetime2){
  dt  <- difftime(datetime1, datetime2, units='hours')
  dt <- abs(as.numeric(dt))
  return(dt)
}
