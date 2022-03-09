check_len_num  <-  function(x, len) {
  return (!is.null(x) && (length(x) == len && is.numeric(x)))
}
check_num  <-  function(x) {
  return (check_len_num(x, 1))
}

check_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  return(min(abs(c(x%%1, x%%1-1))) < tol && !is.na(x))
}

check_char <-  function(x) {
  return (!is.null(x) && (length(x) == 1 && is.character(x)))
}

check_log <-  function(x) {
  return (!is.null(x) && (length(x) == 1 && is.logical(x)))
}

check_path <-  function(x) {
  return (!missing(x) && (length(x) == 1 && is.character(x)))
}

