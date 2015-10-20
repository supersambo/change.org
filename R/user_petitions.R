#' Get petitions created by a given user
#'
#' This function returns the petitions a user created
#' @param user_id id as returned by \code{\link{user_id}} or taken from userprofile url
#' @param api_key character vector of a valid API key.
#' @return A data.frame containing petitions including details.
#' @seealso  \code{\link{user_id}}



user_petitions <- function(petition_id, api_key){
    require(rjson)
    require(RCurl)
    require(plyr)
    requestURL <- paste("https://api.change.org/v1/users/",user_id,"/petitions?api_key=",api_key, sep="")

    output <- fromJSON(getURL(requestURL), method='C')
    result <- as.data.frame(do.call("rbind.fill", lapply(output$petitions, function(x){x[sapply(x, is.null)] <- NA;return(as.data.frame(x[names(x)!="targets"]))})), stringsAsFactors=TRUE)
    return(result)
}




