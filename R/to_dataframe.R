#' Converts returned JSON data into an R dataframe.
#' 
#' Private function. Converts returned JSON data into an R dataframe.
#' 
#' @param response The raw response data after a query.
#' @param keep_metadata Flag to indicate if metadatata should be handled.
#'  Suported values are: 
#'     - "clean"     -  remove metadata (default one)
#'     - "raw"       -  remove metadata
#'     - "raw_links" -  remove metadata links
#' 
#' @return \code{resource()} returns a resource object for a Hivepod backend. 
#' This resource object can be used to make queries, count and manipulate data in the backend. 
#' @examples 
#' cnx <- connect("http://jacaton-r.herokuapp.com", "demo", "1234") 
#' exo <- resource(cnx, "exoplanets") 
#' whe <- resource(cnx, "wheathers") 
#' oly <- resource(cnx, "olympicMedals") 

to_dataframe <- function(response, keep_metadata = "clean") {
  if(!keep_metadata %in% c("clean", "raw", "raw_links")) {
    message("keep_metadata has to be clean, raw or raw_links")
  } else {
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