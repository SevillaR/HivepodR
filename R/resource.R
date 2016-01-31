resource <-  function(connection, resourceName) {
  res <- list(connection[[1]], connection[[2]], connection[[3]],connection[[4]], resourceName)
  return (res)
}