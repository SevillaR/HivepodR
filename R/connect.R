#' Setups a connection.
#' 
#' Setup a connection to a given Hivepod backend with credentials.
#' 
#' @param url The base URL to connect to.
#' @param user The user credential.
#' @param pass The password for the user.
#' 
#' @return connect returns a connection object for a Hivepod backend. 
#' This connection object can be used to discover and access to resources. 
#' @export
#' @examples 
#' cnx <- connect("http://jacaton-r.herokuapp.com", "demo", "1234") 

connect <- function(url, user, pass) {
  urlbase <- handle(url)
  rping <- GET(handle=urlbase, path="ping")
  dataPing <- content(rping, type="application/json")

  status <- GET(handle=urlbase,  config=authenticate(user, pass), path="api/status")
  dataQ1 <- content(status, type="application/json")
  
  con <- list(url, user, pass, status)
  return (con)
}