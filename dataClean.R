# Load necessary packages
if("car" %in% rownames(installed.packages()) == FALSE){
  install.packages("car") 
}
library(car)
pathToRootDirectory <- "~/Documents/HTRC Research/Data Analysis/"

# Non-destructively recode publication codes to Publication strings
# Usage: data <- replacePublicationCodes(data)
replacePublicationCodes <- function(dataFrame){
  countryCodes <- read.csv(paste(pathToRootDirectory, "countries.csv", sep="")) # Read in country codes from data file
  countryCodes <- countryCodes[c("name","MARC")] # Grab necessary columns
  strings <- apply(countryCodes, 1, function(row) { # For every row odes data in Country
    string <- paste("'",row["MARC"], "'='", row["name"], "';", sep="") # Create recode strings
    return(string)
  })
  dataFrame$`Publication Place` <- recode(dataFrame$`Publication Place`, paste(strings, collapse="")) # Recode acronyms to full strings
  return(dataFrame)
}

# Usage: data <- replaceBiliographicCodes(data)
replaceBibliographicCodes <- function(dataFrame){
  bibFormat <- read.csv(paste(pathToRootDirectory, "bibFormats.csv", sep=""))
  strings <- apply(bibFormat, 1, function(row){
    string <- paste("'",row["MARC"],"'='",row["Name"],"';", sep="")
    return(string)
  })
  dataFrame$`Bibliographic Format` <- recode(dataFrame$`Bibliographic Format`, paste(strings, collapse=""))
  return(dataFrame)
}

# Usage: data <- replaceLanguageCodes(data)
replaceLanguageCodes <- function(dataFrame){
  languageCodes <- read.csv(paste(pathToRootDirectory, "languages.csv", sep=""))
  strings <- apply(languageCodes, 1, function(row){
    string <- paste("'",row["MARC"],"'='",row["Name"],"';", sep="")
    return(string)
  })
  dataFrame$Language <- recode(dataFrame$Language, paste(strings, collapse=""))
  return(dataFrame)
}

# Usage: data <- removeNonsenseDates(data)
removeNonsenseDates <- function(dataFrame){
  dataFrame <- subset(dataFrame, `Publication Date` != 9999)
  return(dataFrame)
}

# Usage: data <- removeDuplicatedEntries(data)
removeDuplicatedEntries <- function(dataFrame){
  dataFrame <- dataFrame[!duplicated(dataFrame["OCLC Numbers"]),]
  return(dataFrame)
}

# Usage: data <- removeIncompleteRows(data)
removeIncompleteRows <- function(dataFrame){
  dataFrame <- complete.cases(dataFrame)
  print(dataFrame)
}

getUnrecognizedRows <- function(dataFrame){
  rows <- apply(dataFrame, 1, function(row){
    if(nchar(row["Language"]) <= 3){
      return(row)
    }
  })
  return(rows)
}

# Usage: data <- getData()
# Optional: data <- getData(c("VolumeIdentifier", "Access", ...))

# This function loads data from file (directory set by global variable) then runs cleaning methods on relevant columns.
getData <- function(columns = NULL){
  data <- read.delim(paste(pathToRootDirectory, "hathi_full_20150901.txt", sep=""), header=FALSE)
  #data <- read.csv(paste(pathToRootDirectory, "200 records.csv", sep=""), stringsAsFactors = FALSE)
  # Replace column names
  column_names <- c("Volume Identifier", "Access", "Rights", "HathiTrust Record Number", "Enumeration/Chronology", "Source", "Source Institution Record", "OCLC Numbers", "ISBNs", "ISSNs", "LCCNs", "Title", "Imprint", "Rights Determination Reason", "Date of Last Update", "Government Document", "Publication Date", "Publication Place", "Language", "Bibliographic Format")
  #column_names <- c("#", "Volume Identifier", "Access", "Rights", "HathiTrust Record Number", "Enumeration/Chronology", "Source", "Source Institution Record", "OCLC Numbers", "ISBNs", "ISSNs", "LCCNs", "Title", "Imprint", "Rights Determination Reason", "Date of Last Update", "Government Document", "Publication Date", "Publication Place", "Language", "Bibliographic Format")
  colnames(data) <- column_names
  data <- removeDuplicatedEntries(data)
  if(!is.null(columns)){
    data <- data[columns] 
  }
  if("Publication Place" %in% colnames(data)){
    data <- replacePublicationCodes(data)
  }
  if("Language" %in% colnames(data)){
    data <- replaceLanguageCodes(data)
  }
  if("Bibliographic Format" %in% colnames(data)){
    data <- replaceBibliographicCodes(data)
  }
  if("Publication Date" %in% colnames(data)){
    data <- removeNonsenseDates(data)
  }
  return(data)
}

