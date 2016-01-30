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

require("httr")

connect <- function(url, user, pass) {
  urlbase <- handle(url)
  rping <- GET(handle=urlbase, path="ping")
  
  dataPing <- content(rping, type="application/json")
  
  aut <- authenticate(user, pass)
  status <- GET(handle=urlbase,  config=list(aut), path="api/status")
  con <- list(urlbase, user, pass)
  return (con)
}

query <- function(con, resource, ...) {
  aut <- authenticate(con[[2]], con[[3]])
  q1 <- GET(handle=con[[1]], config=list(), path=paste("api/", resource, sep="") )
  dataQ1 <- content(q1, type="application/json")
  return (dataQ1)
}

