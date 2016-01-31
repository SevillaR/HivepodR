# Private Auxiliar functions.
# Not exposed in the public package API.


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' Encode a URLComponent  
#' 
#' Returns a value protected for URL encoding.
#' 
#' @param value The value to encode.
#' 
#' @return Returns a value protected for URL encoding. 
#' Add quotes if value is a string. 
#' Convert R logical value to JSON booleans.
#' @examples 
#' urlEncode("A B") 
#' output> "A%20B"

urlEncode <- function(value) {
  if (mode(value) == "numeric") {
    return (value)   
  }
  if (mode(value) == "character") {
    return  (paste0("\"", value, "\"")) 
  }
  if (mode(value) == "logical") {
    if (value==TRUE) {
      return ("true")      
    } else {
      return ("false")      
    }
  }
  return (value)
}

buildQueryConditions <- function(conditionList=NULL) {
  if (is.null(conditionList)) {
    return(NA)
  }  
  res <- "conditions={"
  prefix <-""
  
  for(i in 1:length(conditionList)) {
    res <- paste0(res, prefix, conditionList[i])
    prefix=","    
  }
  
  res <- paste0(res, "}")
  return(res)
}