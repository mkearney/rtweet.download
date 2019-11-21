
determine_token <- function(token, query) {
  ## if next_token already exists
  if ("next_token" %in% names(token)) {
    if (token[["next_token"]] == "user" && "bearer" %in% names(token)) {
      token[["next_token"]] <- "bearer"
      token[["token"]] <- "user"
      return(token)
    }
    if (token[["next_token"]] == "bearer") {
      token[["token"]] <- "bearer"
      token[["next_token"]] <- "user"
      return(token)
    }
    ## if only user token, then don't need to change anything
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

prep_tokens <- function(token = NULL) {
  token <- token %||% rtweet::get_token()
  if (!is_bearable(token)) {
    return(list(user = token))
  }
  list(user = token, bearer = rtweet::bearer_token(token))
}

