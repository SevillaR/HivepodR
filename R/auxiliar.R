
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

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

to_dataframe <- function(response, keep_metadata = "clean"){
  if(!keep_metadata %in% c("clean", "raw", "raw_links")){
    message("keep_metadata has to be clean, raw or raw_links")
  }else{
    responseNA <- lapply(response, nullToNA)
    dat <- unlist(responseNA)
    nvariales <- length(unlist(responseNA[[1]]))
    ncases <- length(responseNA)
    datamatrix <- matrix(data = dat, nrow = ncases, ncol = nvariales, byrow = TRUE) 
    colnames(datamatrix) <- names(unlist(responseNA[[1]]))
    datamatrix <- as.data.frame(datamatrix)
    head(datamatrix)
    idnum <- grep("_id", colnames(datamatrix), fixed = TRUE)
    links <- grep("_links.", colnames(datamatrix), fixed = TRUE)
    optionalsid <- grep("_", colnames(datamatrix), fixed = TRUE)
    optionals <- optionalsid[-which(optionalsid == idnum)]
    if(keep_metadata == "clean"){
      clean <- datamatrix[,-optionals]
      return(clean)
    }
    if(keep_metadata == "raw"){
      raw <- datamatrix[,-links]
      return(raw)
    }
    if(keep_metadata == "raw_links"){
      raw_links <- datamatrix
      return(raw_links)
    }
  }
}