#' Get Information about a given user
#'
#' This function returns user information for a given user_id
#' @param user_id character vector of the user_id. This id can be found in the URL of a user profile.
#' @param api_key character vector of a valid API key.
#' @return a data.frame containing basic information



user <- function(user_id, api_key){
    require(rjson)
    require(RCurl)
    requestURL <- paste("https://api.change.org/v1/users/",user_id,"?api_key=", api_key , sep="")

    output <- fromJSON(getURL(requestURL), method='C')
    if("user_id" %in% names(output)){
        output[sapply(output, is.null)] <- NA
        return(as.data.frame(output))}
    if("errors" %in% names(output)){warning(output$errors)}
}

