library("httr")


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




query <- function(res, limit=-1, skip=-1, conditions=NULL, sort, selectFields, distinct) {
  urlbase <- handle(res[[1]])
  aut <- authenticate(res[[2]], res[[3]])
  
  query <- ""
  prefix <- "?"
  if (limit!=-1) {
    query <- paste0(prefix, "limit=", limit)
    prefix <- "&"
  }
  if (skip!=-1) {
    query <- paste0(query, prefix, "skip=", skip)
    prefix <- "&"
  }
  if (!is.null(conditions)) {
    query <- paste0(query, prefix, buildQueryConditions(conditions))
    prefix <- "&"
  }
  
  
  q1 <- GET(handle=urlbase, config=aut, path=paste("api/", res[[5]], query, sep="") )
  dataQ1 <- content(q1, type="application/json")
  df <- to_dataframe(dataQ1)
  return (df)
}



# Usage:
# cnx <- connect("http://jacaton-r.herokuapp.com", "admin", "icinetic")
# oficinas <- resource(cnx, "oficinas")
# q <- queryRaw(oficinas, "?limit=2")
# q <- query(oficinas, limit=2, skip=0, conditions=buildCondition("nombre", "==", "Barcelona")  )
#
# exo <- resource(cnx, "exoplanets")
# count(exo)
# queryRaw(exo, "?limit=20")


# https://jacaton-r.herokuapp.com/api/oficinas?conditions={%22nombre%22:%22Barcelona%22}

# cond1 = buildCondition("nombre", "==", "Barcelona")
# cond2 = buildCondition(cond, "x", ">", 4)
