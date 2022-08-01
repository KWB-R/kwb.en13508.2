# asNumericIfRequired ----------------------------------------------------------
asNumericIfRequired <- function(x, name = deparse(substitute(x)), dbg = TRUE)
{
  if (! is.numeric(x)) {
    
    kwb.utils::catAndRun(
      dbg = dbg, sprintf("Converting '%s' to numeric", name),
      x <- as.numeric(x)  
    )
  }
  
  x
}
