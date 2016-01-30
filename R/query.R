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

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}


library("httr")


urlEncode <- function(value) {
  if (mode(value) == "numeric") {
    return (value)   
  }
  if (mode(value) == "character") {
    return  (paste0("\"", value, "\"")) 
  }
  if (mode(value) == "logical") {
    if (value==TRUE) {
      return ("true")      
    } else {
      return ("false")      
    }
  }
  return (value)
}

buildQueryConditions <- function(conditionList=NULL) {
  if (is.null(conditionList)) {
    return(NA)
  }  
  res <- "conditions={"
  prefix <-""
  
  for(i in 1:length(conditionList)) {
    res <- paste0(res, prefix, conditionList[i])
    prefix=","    
  }
  
  res <- paste0(res, "}")
  return(res)
}

to_dataframe <- function(response, keep_metadata = "clean"){
  if(!keep_metadata %in% c("clean", "raw", "raw_links")){
    message("keep_metadata has to be clean, raw or raw_links")
  }else{
    responseNA <- lapply(response, nullToNA)
    dat <- unlist(responseNA)
    nvariales <- length(unlist(responseNA[[1]]))
    ncases <- length(responseNA)
    datamatrix <- matrix(data = dat, nrow = ncases, ncol = nvariales, byrow = TRUE) 
    colnames(datamatrix) <- names(unlist(responseNA[[1]]))
    datamatrix <- as.data.frame(datamatrix)
    head(datamatrix)
    idnum <- grep("_id", colnames(datamatrix), fixed = TRUE)
    links <- grep("_links.", colnames(datamatrix), fixed = TRUE)
    optionalsid <- grep("_", colnames(datamatrix), fixed = TRUE)
    optionals <- optionalsid[-which(optionalsid == idnum)]
    if(keep_metadata == "clean"){
      clean <- datamatrix[,-optionals]
      return(clean)
    }
    if(keep_metadata == "raw"){
      raw <- datamatrix[,-links]
      return(raw)
    }
    if(keep_metadata == "raw_links"){
      raw_links <- datamatrix
      return(raw_links)
    }
  }
}

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
  df <- to_dataframe(dataQ1)
  return (df)
}



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

buildCondition <- function(variable, operator, value) {
  if (operator == "==") {
    cond <- paste0("\"", variable, "\":", urlEncode(value))
  }
  if (operator == "!=") {
    cond <- paste0("\"", variable, "\":{\"$not\":{\"$eq\":", urlEncode(value), "}}")
  }
  return (cond)
}

#"nombre":{"$not":{"$eq":"Barcelona"}}


# Usage:
# cnx <- connect("http://jacaton-r.herokuapp.com", "admin", "icinetic")
# oficinas <- resource(cnx, "oficinas")
# q <- queryRaw(oficinas, "?limit=2")
# q <- query(oficinas, limit=2, skip=0, conditions=buildCondition("nombre", "==", "Barcelona")  )
#
# exo <- resource(cnx, "exoplanets")
# q <- count(exo)
# queryRaw(exo, "?limit=20")


# https://jacaton-r.herokuapp.com/api/oficinas?conditions={%22nombre%22:%22Barcelona%22}

# cond1 = buildCondition("nombre", "==", "Barcelona")
# cond2 = buildCondition(cond, "x", ">", 4)
# conds = list(cond1, cond2)