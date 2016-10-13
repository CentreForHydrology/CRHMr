#Newton-Raphston Iteration Function
ff <- function(Tai, Ti1, Li, mw, Ru, RHi){
  val <- -Ti1+Tai-(Li*Dv(Tai)/Ka(Tai))*(phivt(mw,Ti1,Ru) - phiv(mw,RHi,Tai,Ru))
  return(val)
}
