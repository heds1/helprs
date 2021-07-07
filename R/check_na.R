#' Sum NA values in a dataframe by column
#' @param x dataframe to interrogate.
#' @param empty_str logical, should empty strings be counted as well?
#' Defaults to false.
#' @return a named int, where the names correspond to the original variable names
#' and the values correspond to the number of NAs found for that variable.
#' @examples 
#' df <- data.frame(x = c('a','b',NA,''), y = c(1,NA,2,NA))
#' check_na(df)
#' check_na(df, empty_str = TRUE)

check_na <- function(x, empty_str = FALSE) {
    results <- list()
    for (name in names(x)) {
        results[[name]] <- sum(is.na(x[,name]))
        if (empty_str) results[[name]] <- results[[name]] + length(which(x[,name] == ""))
    }
    return (unlist(results))
}