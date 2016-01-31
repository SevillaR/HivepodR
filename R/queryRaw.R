queryRaw <- function(res, query) {
  urlbase <- handle(res[[1]])
  aut <- authenticate(res[[2]], res[[3]])
  q1 <- GET(handle=urlbase, config=aut, path=paste("api/", res[[5]], query, sep="") )
  dataQ1 <- content(q1, type="application/json")
  df <- to_dataframe(dataQ1)
  return (df)
}