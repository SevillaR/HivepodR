#' Builds a condition 
#' 
#' Returns a condition clause to be used in queries.
#' 
#' @param variable The variable or column name to filter.
#' @param operator An operator for the filter. Currenty (== equals) an (!= not equals) are supported. (More operators to be implemented in the future).
#' @param value Provides a value to compare with. 
#' 
#' @return Returns the filtering clause. A list of clauses can be composed
#' and passed to \code{query()} or \code{count()} functions via the \code{conditions}
#' parameter to build complex queries. 
#' @export
#' @examples 
#' cnx <- connect("http://jacaton-r.herokuapp.com", "demo", "1234") 
#' off <- resource(cnx, "oficinas") 
#' 
#' count(off)
#' count(off, conditions=buildCondition("nombre", "!=", "Seville")  )
#' count(off, conditions=buildCondition("nombre", "==", "Seville")  )
#' query(off, conditions=buildCondition("nombre", "==", "Seville")  )

buildCondition <- function(variable, operator, value) {
  if (operator == "==") {
    cond <- paste0("\"", variable, "\":", urlEncode(value))
  }
  if (operator == "!=") {
    cond <- paste0("\"", variable, "\":{\"$not\":{\"$eq\":", urlEncode(value), "}}")
  }
  return (cond)
}