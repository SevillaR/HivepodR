#' Count the number of rows in a query. 
#' 
#' Returns the total count of rows in a query.
#' 
#' @param resource The resource to query.
#' @param conditions A set of conditions to constraint the query on the server side. Use the function \code{buildCondition()} to create the conditions.0
#' @param distinct Provides a distict criteria. (Not yet implemented) 
#' 
#' @return Returns the total count of rows in a query.
#' @export
#' @examples 
#' cnx <- connect("http://jacaton-r.herokuapp.com", "demo", "1234") 
#' off <- resource(cnx, "oficinas") 
#' 
#' count(off)
#' count(off, conditions=buildCondition("nombre", "!=", "Seville")  )

count <- function(resource, conditions=NULL, distinct=NULL) {
  
  urlbase <- handle(resource[[1]])
  aut <- authenticate(resource[[2]], resource[[3]])
  
  query <- "?count=true"
  prefix <- "&"
  if (!is.null(conditions)) {
    query <- paste0(query, prefix, buildQueryConditions(conditions))
  }
  
  q1 <- GET(handle=urlbase, config=aut, path=paste0("api/", resource[[5]], query) )
  dataQ1 <- content(q1, type="application/json")
  return (dataQ1)
}