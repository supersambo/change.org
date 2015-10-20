#' Get petition signatures for given petition id
#'
#' This function returns the signatures for a given petition id.  Note that retrieving all signatures may take quite some time since only 500 signatures can be gathered per request. Also signature count apparently does not necessarily match the number of signatures made availible via the API. 

#' @param petition_id petition id as returned by \code{\link{petition_id}}
#' @param nr number of signatures to be returned. Can be \code{numeric} or \code{"all"} to retrieve all.
#' @param sorting  The order by which signatures will be returned. Accepted values are \code{"time_asc"} and \code{"time_desc"}. If omitted, defaults to time_asc.
#' @param verbose \code{logical} if console reporting is wanted. Defaults to \code{TRUE} 
#' @param api_key character vector of a valid API key.
#' @return A data.frame containing signatures including details.
#' @seealso  \code{\link{petition_id}}


petition_signatures <- function(petition_id,nr="all", sorting="time_desc", verbose=TRUE, api_key){
    require(rjson)
    require(RCurl)
    requestURL <- paste("https://api.change.org/v1/petitions/",petition_id,"/signatures?sort=",sorting,"&api_key=",api_key, sep="")
    total_nr <- nr
    pg <- 1
    output <- fromJSON(getURL(requestURL), method='C')
    if(nr=="all"){
        total_nr <- output$signature_count
        if(verbose){cat(paste("Attempting to retrieve all", output$signature_count, "signatures..."), fill=TRUE)}
    }

    if(verbose){cat("Retrieving Page ", pg, ": ✓",sep="", fill=TRUE)}
    x <- lapply(output$signatures, function(x) {x[sapply(x, is.null)] <- NA; unlist(x) })
    result <- as.data.frame(do.call("rbind", x),stringsAsFactors=FALSE)
    pg=pg+1

    if(total_nr>nrow(result)){
        while(!is.null(output$next_page_endpoint)){
            requestURL <- paste(output$next_page_endpoint,"&api_key=",api_key, sep="")
            if(verbose){cat("Retrieving Page ", pg , ":",sep="",  fill=FALSE)}
            output <- fromJSON(getURL(requestURL), method='C')
            x <- lapply(output$signatures, function(x) {x[sapply(x, is.null)] <- NA; unlist(x) })
            result <- rbind(result, as.data.frame(do.call("rbind", x),stringsAsFactors=FALSE))
            if(verbose){cat(" ✓", fill=TRUE)}
            pg=pg+1
            if(nrow(result)>=total_nr){break}
        }
    }

    if(nr!="all"){result <- result[1:total_nr,]}
    if(verbose){cat("\nRetrieved ", nrow(result), " of a total of (officially) ", output$signature_count, " signatures.\n", sep="",  fill=TRUE)}
    return(result)
}



