# asNumericIfRequired ----------------------------------------------------------
asNumericIfRequired <- function(x, name = deparse(substitute(x)), dbg = TRUE)
{
  if (is.numeric(x)) {
    return(x)
  }  

  kwb.utils::catAndRun(
    messageText = sprintf("Converting '%s' to numeric", name),
    expr = as.numeric(x),
    dbg = dbg
  )
}
