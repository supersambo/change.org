#' Get petitions created by a given organization
#'
#' This function returns the petitions a organization created
#' @param organization_id id as returned by \code{\link{organization_id}} or taken from organizations profile url
#' @param api_key character vector of a valid API key.
#' @return A data.frame containing petitions including details.
#' @seealso  \code{\link{organization_id}}



organization_petitions <- function(petition_id, api_key){
    require(rjson)
    require(RCurl)
    require(plyr)
    requestURL <- paste("https://api.change.org/v1/organizations/",organization_id,"/petitions?api_key=",api_key, sep="")

    output <- fromJSON(getURL(requestURL), method='C')
    result <- as.data.frame(do.call("rbind.fill", lapply(output$petitions, function(x){x[sapply(x, is.null)] <- NA;return(as.data.frame(x[names(x)!="targets"]))})), stringsAsFactors=TRUE)
    return(result)
}




