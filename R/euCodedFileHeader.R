# euCodedFileHeader ------------------------------------------------------------

#' Generate List With EU Header Information
#' 
#' @param separator default: ";"
#' @param decimal default: "."
#' @param quote default: '"'
#' @param encoding default: "ISO-8859-1"
#' @param language default: "en"
#' @param year default: 2010
#' 
#' @return list with elements \code{separator}, \code{decimal}, \code{quote},
#'   \code{encoding}, \code{language}, \code{year}
#'   
#' @export
#' 
euCodedFileHeader <- function(
    separator = ";", 
    decimal = ".", 
    quote = '"', 
    encoding = "ISO-8859-1",
    language = "en", 
    year = 2010L
)
{
  list(
    separator = separator, 
    decimal = decimal, 
    quote = quote, 
    encoding = encoding, 
    language = language, 
    year = year
  )
}
