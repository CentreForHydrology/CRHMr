CRHMtoZoo <-
function(CRHMdata){
  # converts CRHM data to zoo object
  # used by several functions
  
  cols <- ncol(CRHMdata)
  datetime <- CRHMdata[,1]
  vars <- CRHMdata[,-1]
  zoo.object <- zoo::zoo(vars, order.by=datetime)
}
