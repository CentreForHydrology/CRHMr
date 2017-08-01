#' Reads in CRHM debug file
#'
#' @param debugFile Required. CRHM debug file name.
#' @param returnData Optional. The data to be returned. If \option{all}
#' (the default) then all sections will be returned as a list of data frames.
#' Other options are \option{header}, \option{initalconditions},
#' \option{basinsummary}, and \option{groupsummary}. Note that if
#' \option{groupsummary} is specified, and there are no groups, then an
#' error will result.
#'
#' @return If successful, returns a list of data frames, or a single specified
#' data frame. If unsuccessful, returns \code{FALSE}.
#' @export
#'
#' @examples /dontrun{
#' allDebug <- readDebug("07-08_debug.txt")
#' # show header
#' View(allDebug$header)
#' # show group summary
#' View(allDebug$groupSummary)}
readDebugFile <- function(debugFile="", returnData="all"){
  groupNames <- c(0)              # only needed if groups are present
  groupModules <- c(0)            # only needed if groups are present
  headerVars <- c(0)
  headerVals <- c(0)

  allHRUs <- c()
  allgroups <- c()
  allmodules <- c()
  allvariables <- c()
  allunits <- c()
  allvalues <- c()

  allGroupSummaryGroup <- c()
  allGroupSummaryModule <- c()
  allGroupSummaryVariable <- c()
  allGroupSummaryUnit <- c()
  allGroupSummaryValue <- c()

  allModuleSummaryModule <- c()
  allModuleSummaryVariable <- c()
  allModuleSummaryUnit <- c()
  allModuleSummaryValue <- c()

  allBasinSummaryModule <- c()
  allBasinSummaryVariable <- c()
  allBasinSummaryUnit <- c()
  allBasinSummaryValue <- c()

  # check parameters
  if (debugFile == ''){
    cat('Error: must specify a debug file name\n')
    return(FALSE)
  }

  # read file
  con <- file(debugFile, "r", blocking = FALSE, encoding="ISO_8859-2")
  debugLines <- readLines(con, n=-1, warn=FALSE)
  close(con)

  # remove all extraneous crap from file
  # remove single quotes
  debugClean1 <- gsub("'", "", debugLines, fixed=TRUE )

  # remove missing lines
  stringLengths <- sapply(debugClean1, stringr::str_length)
  singles <- (stringLengths < 10)
  debugClean2 <- debugClean1[!singles]


  # get sections of debug file
  numLines <- length(debugClean2)
  endHeaderLineNum <- grep("start of run", debugClean2, fixed=TRUE)
  header <- debugClean2[1:endHeaderLineNum]

  endInitialConditionsLineNum <-  grep("End of model run:", debugClean2,
                                       fixed=TRUE)
  initialConditionsLines <- debugClean2[(endHeaderLineNum + 1):
                                          (endInitialConditionsLineNum-1)]

  finalConditionsLines <- debugClean2[(endInitialConditionsLineNum+1):
                                        (numLines-1)]

  ###############################################################
  #
  #               read header and assemble data frame
  #
  ###############################################################

   # parse header lines
  projectFileLineNum <- grep("Project file:", header, fixed=TRUE)
  projectFile <- stringr::str_split_fixed(header[projectFileLineNum], ":", 2)[2]

  obsFileLineNum <- grep("Observation file:", header, fixed=TRUE)
  obsFile <- stringr::str_split_fixed(header[obsFileLineNum], ":", 2)[2]

  modelTimeLineNum <- grep("Time of model run", header, fixed=TRUE)
  model1 <- stringr::str_split_fixed(header[modelTimeLineNum], ":", 3)
  modelRunTime <- stringr::str_split_fixed(model1[2], stringr::fixed("."), 1)[1]
  programVersion <- model1[3]

  timeStepLineNum <- grep("timestep ", header, fixed=TRUE)
  timestep <- stringr::str_split_fixed(header[timeStepLineNum],
                                       "timestep ", 2)[2]

  moduleListLineNum <- grep("Module List", header, fixed=TRUE)
  moduleCSV <- stringr::str_split_fixed(header[moduleListLineNum], '"', 3)[2]

  # check if there are groups

  if( (timeStepLineNum - moduleListLineNum) > 1 )
    groupsPresent <- TRUE
  else
    groupsPresent <- FALSE

  # strip terminal .
  moduleCSV <- stringr::str_replace(moduleCSV, stringr::fixed("."), "")

  # get individual modules
  modules <- stringr::str_split(moduleCSV, ",")
  modules <- unlist(modules)

  numModules <- length(modules)

  if(groupsPresent){
    groups <- modules
    numGroups <- numModules
    for (i in 1:numGroups){
      groupLineNum <- moduleListLineNum + i
      groupLine <- header[groupLineNum]

      # get group name and names of modules
      splitVals <- stringr::str_split_fixed(groupLine, '->', 2)
      groupNames[i] <- stringr::str_trim(splitVals[1])
      groupModules[i] <- splitVals[2]
    }
  }

  startOfRun <- stringr::str_trim(
    stringr::str_split_fixed(header[endHeaderLineNum], '-', 3)[2])


  # assemble header
  headerVars[1] <- 'Project file'
  headerVals[1] <- projectFile

  headerVars[2] <- 'Obs file'
  headerVals[2] <- obsFile

  headerVars[3] <- 'Time of model run'
  headerVals[3] <- modelRunTime

  headerVars[4] <- 'Start of model run'
  headerVals[4] <- startOfRun

  headerVars[5] <- 'Time step'
  headerVals[5] <- timestep

  if(!groupsPresent){
    headerVars[6] <- 'Modules'
    headerVals[6] <- modules
  } else {
    headerVars <- c(headerVars, groupNames)
    headerVals <- c(headerVals, groupModules)
  }

  header <- data.frame(headerVars, headerVals)
  names(header) <- c('Variable', 'Value')


  ###############################################################
  #
  #       read initial conditions and assemble data frame
  #
  ###############################################################
  # remove parenthesis
  initialConditionsLines <- stringr::str_replace_all(initialConditionsLines,
                                                     stringr::fixed("("), "")
  initialConditionsLines <- stringr::str_replace_all(initialConditionsLines,
                                                     stringr::fixed(")"), "")

  # split into columns
  cols1 <- reshape2::colsplit(initialConditionsLines, ": ",
                              c("HRU","rest","values"))

  # get values
  cols2 <- reshape2::colsplit(cols1$values, " ", c("v1", "v2", "v3"))

  # get rest of data
  s <- sapply(cols1$rest, parseText)
  x <- t(s)
  names(x) <- NULL
  cols3 <- data.frame(x)
  names(cols3) <- c("group", "module", "variable","unit1", "unit2", "unit3")

  # combine to create data frame
  if(groupsPresent){
    HRU <- as.character(cols1$HRU)
    group <-  as.character(cols3$group)
    module <- as.character(cols3$module)
    variable <- as.character(cols3$variable)
    v1 <- cols2$v1
    v2 <- cols2$v2
    v3 <- cols2$v3

    initialConditions <- data.frame(HRU, group, module, variable, v1, v2, v3)

    names(initialConditions) <- c("HRU","group", "module", "variable",
                                  as.character(cols3$unit1[1]),
                                  as.character(cols3$unit2[1]),
                                  as.character(cols3$unit3[1]))
  } else {
    HRU <- as.character(cols1$HRU)
    module <- as.character(cols3$module)
    variable <- as.character(cols3$variable)
    v1 <- cols2$v1
    v2 <- cols2$v2
    v3 <- cols2$v3

    initialConditions <- data.frame(HRU, group, module, variable, v1, v2, v3)

    names(initialConditions) <- c("HRU","group", "module", "variable",
                                  as.character(cols3$unit1[1]),
                                  as.character(cols3$unit2[1]),
                                  as.character(cols3$unit3[1]))
  }


  ###############################################################
  #
  #       read final conditions and assemble data frame
  #
  ###############################################################

  # remove parenthesis
  finalConditionsLines <- stringr::str_replace_all(finalConditionsLines,
                                                   stringr::fixed("("), " ")
  finalConditionsLines <- stringr::str_replace_all(finalConditionsLines,
                                                   stringr::fixed(")"), " ")

  # get HRUs
  HRUs <- unique(initialConditions$HRU)

  if(!groupsPresent)
    modules <- headerVars[6]
  else{
    groups <- groupNames
    modules <- stringr::str_c(groupModules, collapse= " ")
    modules <- stringr::str_split(modules, " ")
    modules <- unlist(modules)
    modules <- unique(modules[modules !=""])
  }

  numFinalConditionsLines <- length(finalConditionsLines)

  # loop through HRUs, groups, modules, and variables to assemble data
  for (i in 1:numFinalConditionsLines){
    inLine <- finalConditionsLines[i]
    first3 <- substr(stringr::str_trim(inLine), 1, 3)
    HRUpresent <- stringr::str_detect(first3,
                                      stringr::coll("HRU", ignore_case = FALSE))

    if (HRUpresent){
      # get HRU, group, variables, values
      pieces <- stringr::str_split_fixed(inLine, ":", 3)
      HRU <- pieces[1]
      values <- parseNums(pieces[3])
      values <- values[!is.na(values)]
      numValues <- length(values)
      rest <- stringr::str_trim(pieces[2])
      pieces <- parseText(rest)
      numPieces <- length(pieces)

      # figure out if the variable name is composed of 1 or 2 pieces
      variablePieces <- numPieces - (numValues + 2)

      group <- stringr::str_trim(pieces[1])
      module <- stringr::str_trim(pieces[2])

      if(variablePieces == 1){
        variable <- stringr::str_trim(pieces[3])
        units <- pieces[4:(3+numValues)]
      } else {
        variable <- paste(stringr::str_trim(pieces[3]),
                          stringr::str_trim(pieces[4]), sep=" ")
        units <- pieces[5:(4+numValues)]

      }

      # assemble output
      allunits <- c(allunits, units)

      HRUs <- rep.int(HRU, numValues)
      allHRUs <- c(allHRUs, HRUs)

      allvalues <- c(allvalues, values)

      variables <- rep.int(variable, numValues)
      allvariables <- c(allvariables, variables)

      modules <- rep.int(module, numValues)
      allmodules <- c(allmodules, modules)

      if(groupsPresent){
        group <- pieces[1]
        groups <- rep.int(module, numValues)
        allgroups <- c(allgroups, groups)
      }

      # sanity check of array lengths
      if ((length(allunits) != length(allvalues)) |
          (length(allvariables) != length(allvalues)) |
           (length(allunits) != length(allvariables))) {
             cat('Problem: lengths of arrays are not the same\n')
           }

  } else {
    # no HRU, so must be summary, either by group, module or basin
      groupSummary <- stringr::str_detect(inLine,
                                          stringr::coll("GRP",
                                                        ignore_case = TRUE))

      # spilt by colon
      pieces <- stringr::str_split_fixed(inLine, ":", 2)

      # parse text
      pieces2 <- parseText(pieces[1])
      numPieces <- length(pieces2)

      # check to see if module summary
      moduleSummary <- stringr::str_trim(pieces[1]) ==
        stringr::str_trim((pieces[2]))

      # if not group or module then must be basin summary
      if(!groupSummary & !moduleSummary)
        basinSummary <- TRUE
      else
        basinSummary <- FALSE

      if(groupSummary){
        # parse value
        values <- parseNums(pieces[2])
        numValues <- length(values)
        if(numValues > 1)
          values <- values[!is.na(values)]
        numValues <- length(values)
        # figure out if the variable name is composed of 1 or 2 pieces
        variablePieces <- numPieces - (numValues + 2)

        # add sanity check
        # if ((variablePieces > 2 ) |
        #     (variablePieces < 1)) {
        #   cat('Problem with variable name length:',
        #       variablePieces,'parts \n')
        # }
          group <- stringr::str_trim(pieces2[1])
          module <- stringr::str_trim(pieces2[2])

          if(variablePieces == 1){
            variable <- stringr::str_trim(pieces2[3])
            unit <- stringr::str_trim(pieces2[4])
          } else {
            variable <- stringr::str_c(stringr::str_trim(
              pieces2[3:(numPieces-1)]), collapse=" ")
            unit <- pieces2[numPieces]

          }

          # sanity check of array lengths
          if ((length(group) != length(module)) |
              (length(group) != length(variable)) |
              (length(group) != length(unit)) |
              (length(group) != length(values))) {
            cat('Problem: lengths of arrays are not the same\n')
          }

          # add to arrays
          allGroupSummaryGroup <- c(allGroupSummaryGroup, group)
          allGroupSummaryModule <- c(allGroupSummaryModule, module)
          allGroupSummaryVariable <- c(allGroupSummaryVariable, variable)
          allGroupSummaryUnit <- c(allGroupSummaryUnit, unit)
          allGroupSummaryValue <- c(allGroupSummaryValue, values)

        } else if (moduleSummary){
          # parse value
          values <- parseNums(pieces[2])
          numValues <- length(values)
          if(numValues > 1)
            values <- values[!is.na(values)]
          numValues <- length(values)
          # figure out if the variable name is composed of 1 or 2 pieces
          variablePieces <- numPieces - (numValues + 2)

          # add sanity check
          if ((variablePieces > 2 ) |
              (variablePieces < 1)) {
            cat('Problem with variable name length:',
                variablePieces,'parts \n')
          }

          module <- stringr::str_trim(pieces2[1])
          variable <- stringr::str_trim(pieces2[3])
          unit <- stringr::str_trim(pieces2[4])

          # add to arrays
          allModuleSummaryModule <- c(allModuleSummaryModule, module)
          allModuleSummaryVariable <- c(allModuleSummaryVariable, variable)
          allModuleSummaryUnit <- c(allModuleSummaryUnit, unit)
          allModuleSummaryValue <- c(allModuleSummaryValue, values)

        } else {

          values <- parseNums(pieces[2])

          # basin summary
          module <- stringr::str_trim(pieces2[1])
          variable <- stringr::str_trim(pieces2[2])
          unit <- stringr::str_trim(pieces2[3])

          # add to arrays
          allBasinSummaryModule <- c(allBasinSummaryModule, module)
          allBasinSummaryVariable <- c(allBasinSummaryVariable, variable)
          allBasinSummaryUnit <- c(allBasinSummaryUnit, unit)
          allBasinSummaryValue <- c(allBasinSummaryValue, values)

          # sanity check of array lengths
          if ((length(allBasinSummaryModule)
               != length(allBasinSummaryVariable)) |
              (length(allBasinSummaryModule)
               != length(allBasinSummaryUnit)) |
              (length(allBasinSummaryModule)
               != length(allBasinSummaryValue)))
            cat('Problem: lengths of arrays are not the same\n')
      }
    }
  }

  # assemble dataframes
  if (groupsPresent){
    finalConditions <- data.frame(allHRUs, allgroups, allmodules, allvariables,
                                  allunits, allvalues)
    names(finalConditions) <- c("HRU", "group", "module", "variable",
                                "unit", "value")
  } else {
    finalConditions <- data.frame(allHRUs, allmodules, allvariables,
                                  allunits, allvalues)
    names(finalConditions) <- c("HRU", "module", "variable", "unit", "value")
  }

  # do summary dataframes

  if(groupsPresent){
    groupSummary <- data.frame(allGroupSummaryGroup, allGroupSummaryModule,
                               allGroupSummaryVariable, allGroupSummaryUnit,
                               allGroupSummaryValue)
    names(groupSummary) <- c("group", "module", "variable", "unit", "value")
  } else{
    moduleSummary <- data.frame(allGroupSummaryModule,
                               allGroupSummaryVariable,
                               allGroupSummaryUnit,
                               allGroupSummaryValue)
    names(moduleSummary) <- c("module", "variable", "unit", "value")

  }


  basinSummary <- data.frame(allBasinSummaryModule,
                              allBasinSummaryVariable,
                              allBasinSummaryUnit,
                              allBasinSummaryValue)
  names(basinSummary) <- c("module", "variable", "unit", "value")

  if (groupsPresent){
    if (returnData == "all")
      returnList <- list(header=header,
                         initialConditions=initialConditions,
                         groupSummary=groupSummary,
                         basinSummary=basinSummary)
      else if (stringr::str_to_lower(returnData) == "header")
        returnList <- header
      else if (stringr::str_to_lower(returnData) == "initialconditions")
        returnList <- initialConditions
      else if (stringr::str_to_lower(returnData) == "groupsummary")
        returnList <- groupSummary
      else
        returnList <- basinSummary
  } else {
    if (returnData == "all")
      returnList <- list(header=header,
                         initialConditions=initialConditions,
                         moduleSummary=moduleSummary,
                         basinSummary=basinSummary)
      else if (stringr::str_to_lower(returnData) == "header")
        returnList <- header
      else if (stringr::str_to_lower(returnData) == "initialconditions")
        returnList <- initialConditions
      else if (stringr::str_to_lower(returnData) == "groupsummary"){
        cat('No groups present in file\n')
        return(FALSE)
      }
      else
        returnList <- basinSummary

  }
  return(returnList)
}
