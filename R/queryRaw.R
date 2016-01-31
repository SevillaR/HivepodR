queryRaw <- function(resource, query) {
  urlbase <- handle(resource[[1]])
  aut <- authenticate(resource[[2]], resource[[3]])
  q1 <- GET(handle=urlbase, config=aut, path=paste("api/", resource[[5]], query, sep="") )
  dataQ1 <- content(q1, type="application/json")
  df <- to_dataframe(dataQ1)
  return (df)
}