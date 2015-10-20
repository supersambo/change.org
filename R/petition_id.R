#' Get petition_id for given url
#'
#' This function returns the petition id for a given 
#' petition url. This id is needed to gather further 
#' information about a petition.
#' @param petition_url character vector of the URL of a petition.
#' @param api_key character vector of a valid API key.
#' @return The id of the petition.



petition_id <- function(petition_url, api_key){
    require(rjson)
    require(RCurl)
    requestURL <- paste("https://api.change.org/v1/petitions/get_id?api_key=", api_key, "&petition_url=", curlEscape(petition_url), sep="")

    output <- fromJSON(getURL(requestURL), method='C')
    if(output$result=="success"){return(output$petition_id)}
    else{warning(output$messages)}
}

