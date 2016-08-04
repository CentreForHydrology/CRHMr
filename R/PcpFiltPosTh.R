#' Cleans up a cumulative precipitation time series
#'
#' @description Cleans up a cumulative precipitation time series by transferring changes below a specified threshold to neighbouring periods, and eliminating large negative changes associated with gauge servicing (bucket emptying). The filtering is done using a brute-force algorithm that identifies small or negative changes (below dPcpTh) then transfers them to neighbouring positive changes thus aggregating all changes to values above dPcpTh. The transfers are made in ascending order, starting with the lowest (most negative). The cumulative total remains unchanged. This description (and most of the comments in the code) were written by Alan Barr. 
#' @param Pcp Required. Measured cumulative precipitation (mm).
#' @param dPcpTh Optional. Minimum interval precipitation (mm). Default is 0.1 mm.
#' @param dpServicingTh Optional. Threshold for change in storage due to servicing (mm). Default is -100 mm.
#' @param quiet Optional. Suppresses display of messages, except for errors. If set \code{quiet=FALSE}, the output will pause for 5 seconds ever 100 iterations, which may be useful for debugging your parameters. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default), which causes fewer parameters to be output, without pausing, every 1000 iterations. 
#'
#' @return If unsuccessful, returns the value \code{FALSE}. If successful, returns a time series of cleaned values. Writes parameter values to screen.
#' @author Kevin Shook. The code is copied from a MATLAB program PcpFiltPosTh.m written by Alan Barr, 28 Aug 2012.

#' @export
#'
#' @examples
#' \dontrun{
#' testp <- PcpFiltPosTh(dl2$precip, dPcpTh = 0.05, dpServicingTh = -50)}
PcpFiltPosTh <- function(Pcp, dPcpTh=0.1, dpServicingTh=-100, quiet=TRUE){

  #	Base the analysis on non-missing values only
  #	by abstracting a sub-time series xPcp.
  
  # display headings
  
  if (quiet == TRUE){
    cat('iterations   drops  mindrop  maxdrop\n')
  }
  else{
    cat('iterations  mindrop_loc neighbor_loc   mindrop  neighbor drop\n')
  }
  
  iYaN <- which(!is.na(Pcp)) 
  #nYaN <- length(iYaN) 
  xPcp <- Pcp[iYaN]
  xPcp <- as.vector(xPcp)
  
  #	Base the analysis on interval precip dxPcp.
  
  dxPcp<-c(0, diff(xPcp))
  
  #	Eliminate servicing drops from array
  
  # itServicing <- which(dxPcp < dpServicingTh) 
  iYaN <- iYaN[dxPcp >= dpServicingTh]
  dxPcp <- dxPcp[dxPcp >= dpServicingTh]
  
  
  #	Identify small Drops to be aggregated to dPcpTh or higher	
  iDrops <- which((dxPcp < dPcpTh) & (dxPcp != 0)) 
  nDrops <- length(iDrops)
  
  #	Transfer the small Drops one at a time, 
  #	and reassign small Drops after each transfer. 
  #	Drops are transferred to Gets. 
  
  iPass <- 0 
  while (nDrops > 0) {
    iPass <- iPass + 1 
    # output every 1000 iterations
    if (((iPass %% 1000) == 0) & quiet ){
      cat(format(iPass, width=10), 
          format(nDrops, width=7), 
          format(min(dxPcp, na.rm=TRUE), width=8), 
          format(max(dxPcp, na.rm=TRUE), width=8), 
          '\n', sep=' ')
    }
     

    # find lowest dxPcp to be eliminated.
    jDrop <- which.min(dxPcp[iDrops])
    iDrop <- iDrops[jDrop]
    
    # find nearest neighbout Get to transfer Drop to.
    # include in neighbour id criteria not only the distance 
    # between points d2Neighbour
    # but also the neighbours' dxPcp value (with a small weighting) 
    # to select higher dxPcp if two neighbours are equidistant 
    
    iGets <- which(dxPcp > 0)
    iGets <- setdiff(iGets, iDrop)

    d2Neighbour <- abs(iGets - iDrop) # number of periods apart.
    dxPcpNeighbour <- dxPcp[iGets]

    jGet <-which.min(d2Neighbour - dxPcpNeighbour/1e5)
    iGet <- iGets[jGet]
    
    # transfer Drop to Get and set Drop to zero. 
    
    dxPcp[iGet] <- dxPcp[iGet] + dxPcp[iDrop]
    dxPcp[iDrop] <- 0
    
    # reset iDrops and nDrops
    
    iDrops <- which((dxPcp< dPcpTh) & (dxPcp!=0))
    nDrops <- length(iDrops)
    
    if (!quiet){
      cat(format(iPass, width=11),
          format(iDrop, width=12), 
          format(iGet, width=13), 
          format(dxPcp[iDrop], width=10),
          format(dxPcp[iGet], width=15),
          '\n', sep='')
      
      if((jDrop %% 100) == 0)
        Sys.sleep(5)  # pause for 5 seconds
    } 
  
  } # while nDrops>0

# Assign output PcpClean from sum of cleaned dxPcp values.

PcpClean <- NA_real_ * Pcp 
PcpClean[iYaN] <- nancumsum(dxPcp)
return(PcpClean)

}