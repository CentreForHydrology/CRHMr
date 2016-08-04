#' Summarize daily CRHM water outputs by HRU group
#'
#' @description Calculates daily totals of water storages and fluxes for groups of HRUs, for models \emph{without} sub-basins.
#' @param dailyWater Required. The output from the \code{simpleDailyWater} command.
#' @param vars Optional. Variable column numbers to be used (not including the datetime). The default value \option{all} selects all columns.
#' @param prjFile Required. The CRHM model .prj file.
#' @param HRUgroups Required. This can be either a vector or list. If a vector, it contains strings used to group the selected HRU names in the .prj file. If a list, it contains vectors of the HRU numbers to be grouped. See the examples for both usages.
#' @param basinMean Required. This refers to how the daily water values were calculated by \code{simpleDailyWater}. If \code{TRUE} then the aggregated water depths/volumes are computed over the whole basin. If \code{FALSE} then the the aggregated water depths/volumes are computed over area of the HRU groups.
#' @param quiet Optional. Suppresses display of messages, except for errors. The default is TRUE. If you are calling this function in an R script, you will usually leave quiet=TRUE (i.e. the default). If you are working interactively, you will probably want to set quiet=FALSE.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful returns a data frame containing each variable summarized by each HRU group. The names of the variables are in the form: \code{hruGroup.variable.group_total} or \code{hruGroup.variable.basin_total}. If unsuccessful, returns the value FALSE.
#' @export
#'
#' @examples \dontrun{
#' # First, calculate daily water values. In these examples, the water is 
#' # in mm per HRU
#' 
#' #' b <- readOutputFile('Bologna1984_30y_output.txt', timezone='MST')
#' # get daily values without aggregation; values calculated w.r.t the HRU area 
#' dailyHRU <- simpleDailyWater(b, prjFile='Bologna1984_02.prj', basinMean=FALSE)
#'
#' # summarize by groups of HRUs with similar names
#' daily_group <- hruGroupSummary(daily, prjFile = 'Bologna1984_02.prj', 
#' HRUgroups=c('firn','ice','bl'), basinMean=FALSE)
#'
#' # summarize by named groups of HRUs with specified numbers
#' groups <- list(group1=c(1,2,3), group2=c(4,5,6))
#' daily_group <- hruGroupSummary(daily, prjFile = 'Bologna1984_02.prj', 
#' HRUgroups=groups, basinMean=FALSE)
#' }
hruGroupWaterSummary <- function(dailyWater, vars='all', prjFile, HRUgroups, basinMean, 
                                 quiet=TRUE, logfile=''){
  # check parameters
  if(is.null(dailyWater)){
    cat('Error: no daily values specified\n')
    return(FALSE)
  }
  if (nrow(dailyWater) == 0){
    cat('Error: missing daily values\n')
    return(FALSE)
  }
  dailyWater_name <- deparse(substitute(dailyWater))
  
  # read in .prj file
  if (prjFile == ''){
    cat('Error: no specified model .prj file\n')
    return(FALSE)
  }
  
  if (is.null(HRUgroups)){
    cat('Error: missing HRU groups\n')
    return(FALSE)
  }
  if(is.null(basinMean)){
    cat('Error: basinMean not specified\n')
    return(FALSE)
  }
  
  # create output data frame
  groupSummary <- data.frame(dailyWater$date)
  names(groupSummary) <- 'date'
  
  # get variables
  if (length(vars) == 1){
    if (vars != 'all'){
      var_cols <- seq(2, (ncol(dailyWater)-1))
      dailyWater <- dailyWater
    }
  }
  else{
    var_cols <- vars + 1
    dailyWater <- dailyWater[,c(1,var_cols)]
  }
  
  # create variable dataframe
  variable_name <- names(dailyWater)[-1]
  # dailyWater <- dailyWater[,-1]
  variable_count <- ncol(dailyWater)
  
  # get variables in daily water values
  variable_col <- seq(from=2, to=(variable_count))
  variable_names <- names(dailyWater)[-1]
  
  hru_group_count <- length(HRUgroups)
  parsed <- stringr::str_split_fixed(variable_name, stringr::fixed('.'),3)
  variable_hru_num <- as.numeric(parsed[,2])
  variable_name <- parsed[,1]
  variables <- data.frame(variable_name, variable_col, variable_hru_num)
  var_name_types <- unique(variable_name)
  
  # figure out aggregation functions
  agg_functions <- rep.int('mean', variable_count)
  
  # instantaneous flow variables
  flow_vars <- stringr::str_detect(variables$units, stringr::fixed('m^3/s'))
  cols <- variable_col[flow_vars]
  agg_functions[cols] <- 'mean'
  
  # depth/interval variables
  depth_vars <- stringr::str_detect(variables$units, stringr::fixed('/int'))
  cols <- variable_col[depth_vars]
  agg_functions[cols] <- 'sum'
  
  
  prj <- readPrj(prjFile)
  
  # get dimensions
  model_dims <- prjDimensions(prj)
  hru_count <- model_dims[1]
  if (!quiet)
    cat(hru_count, ' HRUs found in ', prjFile,'\n', sep='')
  # get hru names from prj
  hru_name <- readPrjTextVals(prj, 'hru_names', hru_count)
  hru_area <- readPrjNumVals(prj, 'hru_area', hru_count)
  hru_num <- seq(from=1, to=hru_count)

  
  # get HRUs to aggregate by
  
  if (!is.list(HRUgroups)){
    # HRU names are specified
  
    # loop through HRU names
    for (i in 1:hru_group_count){
      hru_group <- HRUgroups[i]  # name to aggregate by
      
      # find all HRUs in prj with specified name
      group_hrus <- stringr::str_detect(hru_name, hru_group)
      group_hrus_num <- hru_num[group_hrus]
      group_hrus_name <- hru_name[group_hrus]
      group_hrus_area <- hru_area[group_hrus]
      group_hrus <- data.frame(group_hrus_num, group_hrus_name, group_hrus_area)
      
  
      # aggregate by each variable
      for (var_name in var_name_types){
        # find columns to select
        # select by variable name
        selected_vars <- variables[variables$variable_name == var_name,]
        
        # find col nums of selected HRUs
        var_hru <- merge(selected_vars, group_hrus, by.x='variable_hru_num', by.y='group_hrus_num')
        
        # check for situation where # of variable HRUs < # of selected HRUs
        # this happens for basin flows
        
        if (nrow(var_hru) == length(group_hrus_num)){
          selected_hru_cols <- var_hru$variable_col
          
          dailyWater_group <- dailyWater[,selected_hru_cols]
          
          if (!basinMean){
            # sum all values in group
            group_sum <- rowSums(dailyWater_group)
            groupSummary <- cbind(groupSummary, group_sum)
            names(groupSummary)[ncol(groupSummary)] <- paste(hru_group,'.',var_name,'.group_total', sep='')
          }
          else{
            # apply hru weightings 
            total_area <- sum(var_hru$group_hrus_area)
            var_hru$weighting <- var_hru$group_hrus_area / total_area
            dailyWater_group_weighted <- dailyWater_group * var_hru$weighting
            group_sum <- rowSums(dailyWater_group_weighted)
            groupSummary <- cbind(groupSummary, group_sum)
            names(groupSummary)[ncol(groupSummary)] <-  paste(hru_group,'.',var_name,'.basin_total', sep='')
            
          }
        }
      } 
    }  
  }
  else{
    # HRUs are specified in a list
    # find number of groups and their names - create names if necessary
    hru_group_count <- length(HRUgroups)
    group_names <- names(HRUgroups)
    if(is.null(group_names)){
      if(!quiet)
        cat('Group names missing; names will be assigned\n')
      group_nums <- seq(1:hru_group_count)
      group_names <- paste('group', group_nums, sep='')
    }
    
      # aggregate by each variable
      # loop through HRU names
      for (group_num in 1:hru_group_count){
        group_hrus_num <- HRUgroups[[group_num]]
        group_name <- group_names[group_num]

        # find all HRUs in specified group
        group_hrus_name <- hru_name[group_hrus_num]
        group_hrus_area <- hru_area[group_hrus_num]
        group_hrus <- data.frame(group_hrus_num, group_hrus_name, group_hrus_area)
        
        for (var_name in var_name_types){
        # find columns to select
        # select by variable name
        selected_vars <- variables[variables$variable_name == var_name,]
        
        # find col nums of selected HRUs
        var_hru <- merge(selected_vars, group_hrus, by.x='variable_hru_num', by.y='group_hrus_num')
        
        # check for situation where # of variable HRUs < # of selected HRUs
        # this happens for basin flows
        
        if (nrow(var_hru) == length(group_hrus_num)){
          selected_hru_cols <- var_hru$variable_col
          
          dailyWater_group <- dailyWater[,selected_hru_cols]
          
          if (!basinMean){
            # sum all values in group
            group_sum <- rowSums(dailyWater_group)
            groupSummary <- cbind(groupSummary, group_sum)
            names(groupSummary)[ncol(groupSummary)] <- paste(group_name,'.',var_name,'.group_total', sep='')
          }
          else{
            # apply hru weightings 
            total_area <- sum(var_hru$group_hrus_area)
            var_hru$weighting <- var_hru$group_hrus_area / total_area
            dailyWater_group_weighted <- dailyWater_group * var_hru$weighting
            group_sum <- rowSums(dailyWater_group_weighted)
            groupSummary <- cbind(groupSummary, group_sum)
            names(groupSummary)[ncol(groupSummary)] <-  paste(group_name,'.',var_name,'.basin_total', sep='')
            
          }
        }
      } 
    } 
  }
    
  # output results
  
  comment <- paste('hruGroupWaterSummary dailyWater:',dailyWater_name ,
                   ' prjFile:',prjFile, ' basinMean:', basinMean, sep='')
  result <- logAction(comment, logfile)
  
  if(result)
    return(groupSummary)
  else
    return(result)
  
}