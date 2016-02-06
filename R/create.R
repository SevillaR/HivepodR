#' Creates data in the Hivepod backend.
#' 
#' Creates  data in the cloud backend.
#' 
#' @param resource The resource to work with. Returned by the \code{resource()} function.
#' @param rows Rows to create.
#' 
#' @return Saved data. 
#' For new created data Hivepod will setup a  value for the \code{_id} field.
#' @export
#' @examples 

create <- function(resource, rows) {
  if (is.null(rows)) {
    message("save() needs a non NULL rows parameter.")
    return(NULL)
  }

  urlbase <- httr::handle(resource[[1]])
  aut <- httr::authenticate(resource[[2]], resource[[3]])
  ah <- httr::add_headers("Content-Type"="application/json")

  payload <- jsonlite::toJSON(rows, auto_unbox=TRUE)
  
  query <- paste0("api/", resource[[5]])
  
  httr::reset_config()
  httr::set_config(ah)
  httr::set_config(aut)
  httpResponse <- httr::POST(config=ah, path=query, body=payload, handle=urlbase)
  createdItems <- httr::content(httpResponse, type="application/json")
  df <- to_dataframe(createdItems)
  return (df)
}