## kwb.en13508.2 v0.3.0 (2024-03-28)

### General

* use GitHub Actions instead of Travis and AppVeyor
* rewrite README.md (where has the original content gone?)
* add tests
* move function definitions between files

### Modifications to data files in the package

* add material codes to eucodes.csv and save as UTF-8
* add eucodes_full_de.csv file with main_code/char_1/char_2 combinations
* in eucodes_de.csv, specify quantification 1 as numeric

### Modifications to (exported or private) functions (in alphabetical order)

* extractInspectionBlocks
  - rename argument "quoteCharacter" to "quote"
* get_code_meanings()
  - rename to getCodeMeanings() 
* getCodes()
  - allow "table" to be a vector of table names
  - stop if table name does not exist
  - reset the row names
* getInspectionHeaderInfo()
  - rename to getInspectionHeaderInfo_v1()
* getInspectionsFromEuLines()
  - rename to getInspectionRecords_v1()
  - add argument "dbg" to control whether to show a message or not
* getInspectionsFromEuLines.new() 
  - rename to getInspectionRecords_v2()
* getObservationsFromEuLines()
  - order columns by name, put column “inspno” first
* mergeInspectionData()
  - add arguments "warn", "naToEmpty"
* readAndMergeEuCodedFiles()
  - add arguments "add.inspid", "error.file" to optionally add globally unique
    inspection IDs and to specify where to store related error messages
  - add arguments "project", "default.time", "name.convention"
  - fix typo in warning
  - clean code
* readEuCodedFile()
  - add arguments "name.convention", "file.encoding", "check.encoding"
  - change default value of argument "encoding" from "latin1" to "unknown" 
  - use new function getObservationRecordsFromEuLines()
  - pass "dbg" through to removeEmptyLines(), getInspectionRecordsFromEuLines() 
  - refactor
* readEuCodedFiles()
  - let "file" be the first column
  - in the error message, report on files that failed to be imported
* removeEmptyLines()
  - clean code
* textblockToDataframe()
  - rename argument "quoteCharacter" to "quote"
* warnOnDifferingHeaders()
  - improve output

### Add exported functions

* getLineDamageInfo()
  - moved from kwb.rerau
  - allow for C-codes (change in continuous defect) in this function

### Add private functions

* createHashFromColumns()
* extdataFile()
* extractObservationBlocks()
  - add a last "to" value if the last EU-line is not "#Z"
* extractObservationData()
  - remove empty records from table of observations 
  - run readObservationsFromCsvText() with "colClasses" being set to the 
    expected column types or to "character" if as.text is TRUE
* getExampleData()
* getExampleFile()
* getFileHeaderFromEuLines()
* getHeaderInfo()
* getInspectionHeaderInfo_v2()
* getInspectionRecordsFromEuLines()
* getObservationRecordsFromEuLines()
  - call extractObservationData() if calling getObservationsFromEuLines() fails
* readFileEncodingFromHeader()
* readObservationsFromCsvText()
  - reduce "colClasses" to columns that actually occur
* setFilename()
* setGlobalInspectionID()
* toEuFormat()
  
### Remove private functions

* getHeaderLinesFromEuCodedLines()
* getHeaderInfoFromHeaderLines()
* order_by()

## kwb.en13508.2 v0.2.0.9000 (2019-09-09)

* Add a NEWS.md file to track changes to the package.  
  See http://style.tidyverse.org/news.html for writing a good NEWS.md
* Harmonise with [kwb.pkgbuild](https://kwb-r.github.io/kwb.pkgbuild)
