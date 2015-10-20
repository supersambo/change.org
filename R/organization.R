#' Get Information about a given organization
#'
#' This function returns organization information for a given organization_id
#' @param organization_id character vector of the organization_id. This id can be found in the URL of a organization profile.
#' @param api_key character vector of a valid API key.
#' @return a data.frame containing basic information



organization <- function(organization_id, api_key){
    require(rjson)
    require(RCurl)
    requestURL <- paste("https://api.change.org/v1/organizations/",organization_id,"?api_key=", api_key , sep="")

    output <- fromJSON(getURL(requestURL), method='C')

    if("organization_id" %in% names(output)){
        output[sapply(output, is.null)] <- NA
        return(as.data.frame(output))}
    if("errors" %in% names(output)){warning(output$errors)}
}

