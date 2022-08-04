getExampleData <- function(dbg = FALSE)
{
  readEuCodedFile(input.file = getExampleFile(), dbg = dbg)
}

getExampleFile <- function()
{
  extdataFile("example_13508_2.txt")
}

extdataFile <- kwb.utils::createFunctionExtdataFile("kwb.en13508.2")
