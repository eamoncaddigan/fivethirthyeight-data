# Contains the function lookupGenderForNames(), which returns a data.frame of 
# gender info from genderize.io given a character vector of first names.

require(jsonlite)
require(httr)

# Helper function: returns NA instead of NULL when a missing list element is
# requested, otherwise returns the element itself.
getListElement <- function(listName, elementName) {
  listElement <- NA
  if (!is.null(listName[[elementName]])) {
    listElement <- listName[[elementName]]
  }
  return(listElement)
}

# Queries genderize.io for names. 
lookupGenderForNames10 <- function(nameVector, countryCode = NA, apiKey = NA) {
  if (length(nameVector) > 10) {
    stop("This only accepts 10 or fewer names")
  }
  
  # Construct the query
  query <- paste("name[", seq_along(nameVector), "]=", nameVector, 
                 sep="", 
                 collapse="&")
  if (!is.na(countryCode) & (countryCode != "none")) {
    query <- paste(query, "&country_id=", countryCode, sep="")
  }
  if (!is.na(apiKey)) {
    query <- paste(query, "&apikey=", apiKey, sep="")
  }
  
  # Run it!
  queryResult <- GET("https://api.genderize.io", query = query)
  if (status_code(queryResult) == 200) {
    responseDF <- fromJSON(content(queryResult, as="text"))
    # Make sure this is a data.frame with the correct columns. I bet fromJSON 
    # can do this for me but I don't know how. This code works whether fromJSON 
    # returned a list (the response to one name) or a data.frame (the response
    # to several).
    responseDF <- data.frame(name = getListElement(responseDF, "name"),
                             gender = getListElement(responseDF, "gender"),
                             country_id = getListElement(responseDF, "country_id"),
                             probability = getListElement(responseDF, "probability"),
                             count = getListElement(responseDF, "count"),
                             stringsAsFactors = FALSE)
    responseDF <- mutate(responseDF, 
                         country_id = ifelse(is.na(country_id), "none", country_id))
    
  } else {
    cat(paste("\n!!!! http returned status code:",
              status_code(queryResult),
              "!!!! message:",
              http_status(queryResult)$message,
              "!!!! error:",
              content(queryResult)$error,
              sep="\n"))
    if (status_code(queryResult) == 429){
      cat('\n!!!! number of available requests exhaused')
    }
    responseDF <- NULL
  }
  return(responseDF)
}

lookupGenderForNames <- function(nameVector, countryCode = NA, apiKey = NA) {
  # Build the list of queries
  queryList <- list()
  while(length(nameVector) > 10) {
    queryList[[length(queryList)+1]] <- nameVector[1:10]
    nameVector <- nameVector[11:length(nameVector)]
  }
  queryList[[length(queryList)+1]] <- nameVector
  
  # Run the queries
  responseList <- list()
  for (i in seq_along(queryList)) {
    responseDF <- lookupGenderForNames10(queryList[[i]], countryCode, apiKey)
    if (is.null(responseDF)) {
      break
    } else {
      responseList[[length(responseList)+1]] <- responseDF
    }
  }
  
  return(do.call(rbind, responseList))
}
