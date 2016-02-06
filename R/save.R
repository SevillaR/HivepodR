#' Saves data in the Hivepod backend.
#' 
#' Saves data in the cloud backend. Creates or update records based on the presence of the \code{_id} field.
#' 
#' @param resource The resource to work with. Returned by the \code{resource()} function.
#' @param rows Rows to save.
#' 
#' @return Saved data. 
#' New data with no defined \code{_id} property will be created as new. POST operation will be atempted.
#' Preexisting records with a value in \code{_id} property will be updated in the backend. PUT operation will be attempted.
#' @export
#' @examples 
#' cnx <- connect("http://jacaton-r.herokuapp.com", "demo", "1234") 
#' not <- resource(cnx, "noticias") 
#' res <- save(not, list(titular="a", fecha="2016-02-05T12:56:00.345Z", cuerpo="body")) 


save <- function(resource, rows) {
}

