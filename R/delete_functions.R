deleteByQuery <- function(resource, conditions=NULL) {
  urlbase <- httr::handle(resource[[1]])
  aut <- httr::authenticate(resource[[2]], resource[[3]])
  
  query <- ""
  prefix <- "?"
  if (!is.null(conditions)) {
    query <- paste0(query, prefix, buildQueryConditions(conditions))
    prefix <- "&"
  } else {
    message("deleteByQuery() needs a non NULL conditions parameter.")
    return(NULL)
  }
  
  httpResponse <- httr::DELETE(handle=urlbase, config=aut, path=paste0("api/", resource[[5]], query) )
  deletedItems <- httr::content(httpResponse, type="application/json")
  df <- to_dataframe(deletedItems)
  return (df)
}

deleteRows <- function(resource, rows) {
  ids <- as.list(rows["_id"])
  ## todo remove empty values in ids
  
  return (deleteByIds(resource, ids))
}

deleteByIds <- function(resource, ids) {
  results <- list()
  if (is.null(ids)) {
    message("deleteByIds() needs a non empty list of ids.")
    return(NULL)
  }
  for(i in 1:length(ids)) {
    results <- c(results, deleteById(resource, ids[i]))
  }
  return (results)
}

deleteById <- function(resource, id) {
  urlbase <- httr::handle(resource[[1]])
  aut <- httr::authenticate(resource[[2]], resource[[3]])
  
  if (is.null(resource)) {
    message("deleteById() needs a resource parameter.")
    return(NULL)
  }
  if (is.null(id)) {
    message("deleteById() needs a id parameter.")
    return(NULL)
  }
  
  query <- paste0("api/", resource[[5]], "/", id)
  
  httpResponse <- httr::DELETE(handle=urlbase, config=aut, path=query )
  deletedItem <- httr::content(httpResponse, type="application/json")
  df <- to_dataframe(deletedItem)
  return (df)
}
