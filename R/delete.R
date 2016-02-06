#' Deletes data in the Hivepod backend.
#' 
#' Deletes data in the cloud backend by id, ids or rows.
#' 
#' @param resource The resource to work with. Returned by the \code{resource()} function.
#' @param conditions Conditions to delete all data matching the query.
#' @param id If of the data row to delete.
#' @param ids List of ids to remove.
#' @param rows Rows to remove.
#' 
#' @return The deleted data. Use only one criteria for deletion: conditions or id or ids or rows.  
#' @export
#' @examples 
#'

delete <- function(resource, conditions=NULL, id=NULL, ids=NULL, rows=NULL) {
  if (is.null(resource)) {
    message("delete() needs a resource parameter.")
    return(NULL)
  }
  if (!is.null(id)) {
    return (deleteById(resource, id))
  }
  else if (!is.null(ids)) {
    return (deleteByIds(resource, ids))
  }
  else if (!is.null(rows)) {
    return (deleteRows(resource, rows))
  }
  else if (!is.null(conditions)) {
    return (deleteByQuery(resource, conditions))
  }
  message("delete() needs one of the following parameters: id, ids or conditions.")
  return (NULL)
}