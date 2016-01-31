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
