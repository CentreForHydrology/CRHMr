f2c <- function(t.f){
  # converts air temps from F to C
  t.c <- (t.f - 32) / 1.8
  return(t.c)
}