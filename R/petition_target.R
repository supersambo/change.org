#' Get petition targets for given petition id
#'
#' This function returns the target for a given petition id.
#' @param petition_id petition id as returned by \code{\link{petition_id}}:w
#' @param api_key character vector of a valid API key.
#' @return A data.frame containing targets including details.
#' @seealso  \code{\link{petition_id}}



petition_targets <- function(petition_id, api_key){
    require(rjson)
    require(RCurl)
    require(plyr)
    requestURL <- paste("https://api.change.org/v1/petitions/",petition_id,"/targets?api_key=",api_key, sep="")

    output <- fromJSON(getURL(requestURL), method='C')
    result <- as.data.frame(do.call("rbind.fill", lapply(output, as.data.frame)), stringsAsFactors=TRUE)
    return(result)
}




