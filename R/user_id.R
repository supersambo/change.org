#' Get user_id for given userpfrofile url
#'
#' This function returns the user_id for a given 
#' petition url. This id is needed to gather further 
#' information about a petition.
#' @param user_url character vector of the URL of user_profile.
#' @param api_key character vector of a valid API key.
#' @return The id of the user.



user_id <- function(user_url, api_key){
    require(rjson)
    require(RCurl)

    user_url <- gsub("/u/", "/users/", user_url)
    requestURL <- paste("https://api.change.org/v1/users/get_id?user_url=", curlEscape(user_url),"&api_key=", api_key, sep="")

    output <- fromJSON(getURL(requestURL), method='C')
    if(output$result=="success"){return(output$user_id)}
    if(output$result=="failure"){warning(output$errors)}
}

