count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if(length((cause)) < 1)
  {
    stop('cause needs to be one of the following: “asphyxiation”, “blunt force”, “other”, “shooting”, “stabbing”, “unknown”')  
  }
    
  regexStr <- "^([aA]sphyxiation|[bB]lunt [fF]orce|[oO]ther|[sS]hooting|[sS]tabbing|[uU]nknown)$"
  regexResult <- regexpr(regexStr, cause)
  regexMatches <- regmatches(cause, regexResult)
  if(length(regexMatches) == 0 ){
    stop('cause needs to be one of the following: “asphyxiation”, “blunt force”, “other”, “shooting”, “stabbing”, “unknown”')    
  } else if(cause=="asphyxiation" || cause == "Asphyxiation"){
    causeString <- "[cC]ause: [aA]sphyxiation"
  }else if(cause=="blunt force"){
    causeString <- "[cC]ause: [bB]lunt [fF]orce"
  }else if(cause=="other"){
    causeString <- "[cC]ause: [oO]ther"
  }else if(cause =="shooting" || cause == "Shooting"){
    causeString <- "[cC]ause: [sS]hooting"
  }else if(cause=="stabbing"){
    causeString <- "[cC]ause: [sS]tabbing"
  }else if(cause=="unknown"){
   causeString <- "[cC]ause: [uU]nknown"
  }

  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  ## Extract causes of death
  searchRegEx <- regexpr(causeString, homicides)
  searchMatches <- regmatches(homicides, searchRegEx)
  
  return(length(searchMatches))
  ## Return integer containing count of homicides for that cause
}