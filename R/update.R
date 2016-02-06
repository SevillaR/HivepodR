#' Updates data in the Hivepod backend.
#' 
#' Updates data in the cloud backend.
#' 
#' @param resource The resource to work with. Returned by the \code{resource()} function.
#' @param rows Rows to update (must contains the \code{_id} field).
#' 
#' @return Updated data. 
#' @export
#' @examples 

update <- function(resource, rows) {
  urlbase <- httr::handle(resource[[1]])
  aut <- httr::authenticate(resource[[2]], resource[[3]])
  
  if (is.null(rows)) {
    message("save() needs a non NULL rows parameter.")
    return(NULL)
  }
  query = paste0("api/", resource[[5]])
  httpResponse <- httr::PUT(handle=urlbase, config=aut, path=query, body=payload, encode="json")
  updatedItems <- httr::content(httpResponse, type="application/json")
  df <- to_dataframe(updatedItems)
  return (df)
}