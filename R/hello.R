#' lo que hace la funcion
#' 
#' en detalle
#' 
#' @param parametro 1 \code{}
#' 
#' @param param 2
#' 
#' @return Ea0 returns a data.frame of class renyi with the selected indices. 
#' @return EinvD returns a vector with EinvD. 
#' @return EJ returns a a vector with Pielou's index. 
#' @return Evar returns a vector with Evar. 
#'
#'
#' @export
#' 
#' @examples 
#' Ea0(A = dummy$abun) 
#' Ea0(A = dummy$abun, scales = c(0.25,0.5,1,2,4,8,Inf)) 
#' EinvD(A = dummy$abun)
#' EJ(A = dummy$abun)
#' Evar(A = dummy$abun)
#' #calculate all of them
#' eve1 <- Eve(A = dummy$abun)
#' eve2 <- Eve(A = dummy$abun, scales = c(0.25,0.5,1,2,4,8,Inf))
#' pairs(eve1)
#' pairs(eve2)

hello <- function() {
  print("Hello, world!")
}

library("httr")

connect <- function(url, user, pass) {
  urlbase <- handle(url)
  rping <- GET(handle=urlbase, path="ping")
  
  dataPing <- content(rping, type="application/json")
  status <- GET(handle=urlbase,  config=authenticate(user, pass), path="api/status")
  dataQ1 <- content(status, type="application/json")

  con <- list(url, user, pass, status)
  return (con)
}

resource <-  function(con, resourceName) {
  con2 <- list(con[[1]], con[[2]], con[[3]],con[[4]], resourceName)
  return (con2)
}

queryRaw <- function(res, query) {
  urlbase <- handle(res[[1]])
  aut <- authenticate(res[[2]], res[[3]])
  q1 <- GET(handle=urlbase, config=aut, path=paste("api/", res[[5]], query, sep="") )
  dataQ1 <- content(q1, type="application/json")
  return (dataQ1)
}


query <- function(res, limit, skip, conditions, sort, selectFields, distinct) {
  urlbase <- handle(res[[1]])
  aut <- authenticate(res[[2]], res[[3]])
  q1 <- GET(handle=urlbase, config=aut, path=paste("api/", res[[5]], query, sep="") )
  dataQ1 <- content(q1, type="application/json")
  return (dataQ1)
}

count <- function(res, conditions, distinct) {
  urlbase <- handle(res[[1]])
  aut <- authenticate(res[[2]], res[[3]])
  q1 <- GET(handle=urlbase, config=aut, path=paste("api/", res[[5]], "?count=true", sep="") )
  dataQ1 <- content(q1, type="application/json")
  return (dataQ1)
}


# Usage:
# cnx <- connect("http://jacaton-r.herokuapp.com", "user", "pass")
# oficinas <- resource(cnx, "oficinas")
# q <- queryRaw(oficinas, "?limit=2")
# q <- query(oficinas, limit=2, skip=1, order="-name +apellido", conditions=list("nombre" "barcelona, )  )

# cond = buildCondition("nombre", "==", "Barcelona")
# cond = andCondition(cond, "x", ">", 4)
# cond = andCondition(cond, "x", ">", 4)
# cond = andCondition(cond, "x", ">", 4)
# cond = andCondition(cond, "x", ">", 4)



