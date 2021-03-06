#' Get signature reasons for given petition id
#'
#' This function returns the reasons for signing a given petition id.  
#'
#' @param petition_id petition id as returned by \code{\link{petition_id}}
#' @param nr number of reasons to be returned. Can be \code{numeric} or \code{"all"} to retrieve all.
#' @param sorting  The order by which reasons will be returned. Accepted values are \code{"popularity"}, \code{"time_asc"}, and \code{"time_desc"}. Defaults to popularity (most number of likes)
#' @param verbose \code{logical} if console reporting is wanted. Defaults to \code{TRUE} 
#' @param api_key character vector of a valid API key.
#' @return A data.frame containing reasons including details.
#' @seealso  \code{\link{petition_id}}


petition_reasons <- function(petition_id,nr="all", sorting="popularity", verbose=TRUE, api_key){
    require(rjson)
    require(RCurl)
    requestURL <- paste("https://api.change.org/v1/petitions/",petition_id,"/reasons?sort=",sorting,"&api_key=",api_key, sep="")
    pg <- 1
    total_nr <- nr
    output <- fromJSON(getURL(requestURL), method='C')
    if(nr=="all"){
        if(verbose){cat(paste("Attempting to retrieve all", output$total_pages, "pages..."), fill=TRUE)}
    }

    if(verbose){cat("Retrieving Page ", pg, ": ✓",sep="", fill=TRUE)}
    x <- lapply(output$reasons, function(x) {x[sapply(x, is.null)] <- NA; unlist(x) })
    result <- as.data.frame(do.call("rbind", x),stringsAsFactors=FALSE)
    pg=pg+1


    if(total_nr>nrow(result)){
        while(!is.null(output$next_page_endpoint)){
            requestURL <- paste(output$next_page_endpoint,"&api_key=",api_key, sep="")
            if(verbose){cat("Retrieving Page ", pg , ":",sep="",  fill=FALSE)}
            output <- fromJSON(getURL(requestURL), method='C')
            x <- lapply(output$reasons, function(x) {x[sapply(x, is.null)] <- NA; unlist(x) })
            result <- rbind(result, as.data.frame(do.call("rbind", x),stringsAsFactors=FALSE))
            if(verbose){cat(" ✓", fill=TRUE)}
            pg=pg+1
            if(nrow(result)>=total_nr){break}
        }
    }
    if(total_nr<nrow(result)){result <- result[1:total_nr,]}
    if(verbose){cat("\nRetrieved ", nrow(result), " reasons.\n", sep="",  fill=TRUE)}
    return(result)
}



