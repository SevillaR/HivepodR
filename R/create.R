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
#' cnx <- connect("http://jacaton-r.herokuapp.com", "demo", "1234") 
#' not <- resource(cnx, "noticias") 
#' newItem <- create(not, list(titular="a", fecha="2016-02-05T12:56:00.345Z", cuerpo="body")) 

create <- function(resource, rows) {
  if (is.null(rows)) {
    message("save() needs a non NULL rows parameter.")
    return(NULL)
  }

  urlbase <- httr::handle(resource[[1]])
  aut <- httr::authenticate(resource[[2]], resource[[3]])
  ah <- add_headers("Content-Type"="application/json")

  payload = toJSON(rows, auto_unbox=TRUE)
  
  query = paste0("api/", resource[[5]])
  
  reset_config()
  set_config(ah)
  set_config(aut)
  httpResponse <- httr::POST(config=ah, path=query, body=payload, handle=urlbase)
  createdItems <- httr::content(httpResponse, type="application/json")
  df <- to_dataframe(createdItems)
  return (df)
}