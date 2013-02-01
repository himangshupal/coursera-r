agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if(length(age) == 0) {
    stop("Please enter age correctly")
  }
  age<- paste0(" ", age, " ")
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is
  ## given
  index <-regexpr("[yY]ears [oO]ld</dd>", homicides)
  ageNum <- substr(homicides, index-4, index-1)
  countVector <- grep(age, ageNum)
  ## Return integer containing count of homicides for that age
  return(length(countVector))
  
}