#' Get organization_id for given organization profile url
#'
#' This function returns the organization_id for a given 
#' organization url. This id is needed to gather further 
#' information about a petition.
#' @param organization_url character vector of the URL of organization_profile.
#' @param api_key character vector of a valid API key.
#' @return The id of the organization.



organization_id <- function(organization_url, api_key){
    require(rjson)
    require(RCurl)

    organization_url <- gsub("/u/", "/organizations/", organization_url)
    requestURL <- paste("https://api.change.org/v1/organizations/get_id?organization_url=", curlEscape(organization_url),"&api_key=", api_key, sep="")

    output <- fromJSON(getURL(requestURL), method='C')
    if(output$result=="success"){return(output$organization_id)}
    if(output$result=="failure"){warning(output$errors)}
}

