
count <- function(res, conditions=NULL, distinct=NULL) {
  
  urlbase <- handle(res[[1]])
  aut <- authenticate(res[[2]], res[[3]])
  
  query <- "?count=true"
  prefix <- "&"
  if (!is.null(conditions)) {
    query <- paste0(query, prefix, buildQueryConditions(conditions))
  }
  
  q1 <- GET(handle=urlbase, config=aut, path=paste0("api/", res[[5]], query) )
  dataQ1 <- content(q1, type="application/json")
  return (dataQ1)
}
