#' Drop a given variable from a given dataframe
#'
#' @return Returns the dataframe with the variable removed
#' @examples
#' names(drop_var(mtcars, "mpg"))

drop_var <- function(x, var) {

    if (var %in% names(x)) {
        return (x[, names(x) != var])
    } else {
        warning("Variable not in dataframe")
    }
    
}