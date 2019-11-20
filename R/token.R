determine_token <- function(token, query) {
  ## if next_token already exists
  if ("next_token" %in% names(token)) {
    currentoken      <- token$token
    token$token      <- token$next_token
    token$next_token <- currentoken
    return(token)
  }
  ## if not bearable, then just use the one
  if (!"bearer" %in% names(token)) {
    token$token      <- "user"
    token$next_token <- "user"
    return(token)
  }
  ## determine next token
  rlu <- rate_limit2(token = token$user, query = query)
  rlb <- rate_limit2(token = token$bearer, query = query)
  if (isTRUE(rlb$remaining > rlu$remaining)) {
    token$token      <- "bearer"
    token$next_token <- "user"
  } else if (isTRUE(rlu$remaining > rlb$remaining)) {
    token$token      <- "user"
    token$next_token <- "bearer"
  } else if (isTRUE(rlu$reset_at <= rlb$reset_at)) {
    token$token      <- "user"
    token$next_token <- "bearer"
  } else {
    token$token      <- "bearer"
    token$next_token <- "user"
  }
  token
}
this_token <- function(token) token[[token[["token"]]]]

next_token <- function(token) token[[token[["next_token"]]]]

determine_token_ <- function(token, query) {
  ## if next_token already exists
  if ("next_token" %in% names(token)) {
    if (token[["next_token"]] == "user" && "bearer" %in% names(token)) {
      token[["next_token"]] <- "bearer"
      token[["token"]] <- "user"
      return(token)
    }
    if (token[["next_token"]] == "bearer") {
      token[["next_token"]] <- "bearer"
      return(token)
    }
    return(token)
  }
  ## if not bearable, then just use the one
  if (!"bearer" %in% names(token)) {
    token[["token"]]      <- token[["user"]]
    token[["next_token"]] <- token[["user"]]
    return(token)
  }
  ## determine next token
  rlu <- rate_limit2(token = token[["user"]], query = query)
  rlb <- rate_limit2(token = token[["bearer"]], query = query)
  if (isTRUE(rlb[["remaining"]] > rlu[["remaining"]])) {
    token[["token"]]      <- token[["bearer"]]
    token[["next_token"]] <- token[["user"]]
  } else if (isTRUE(rlu[["remaining"]] > rlb[["remaining"]])) {
    token[["token"]]      <- token[["user"]]
    token[["next_token"]] <- token[["bearer"]]
  } else if (isTRUE(rlu[["reset"]] <= rlb[["reset"]])) {
    token[["token"]]      <- token[["user"]]
    token[["next_token"]] <- token[["bearer"]]
  } else {
    token[["token"]]      <- token[["bearer"]]
    token[["next_token"]] <- token[["user"]]
  }
  token
}

prep_tokens <- function(token) {
  token <- add_bearable_attr(token)
  if (!is_bearable(token)) {
    return(list(user = token))
  }
  list(user = token, bearer = rtweet::bearer_token(token))
}

has_bearable_attr <- function(x) isTRUE("is_bearable" %in% names(attributes(x)))

add_bearable_attr <- function(token) {
  if (has_bearable_attr(token)) {
    return(token)
  }
  attr(token, "is_bearable") <- isTRUE(grepl("read-write", rtweet:::api_access_level(token)))
  token
}


is_bearable <- function(token = NULL) {
  isTRUE(attr(token, "is_bearable"))
}

is_bearer <- function(x) inherits(x, "bearer")
