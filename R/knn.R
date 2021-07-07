#' k-nearest neighbours imputation for continuous variables, based on a single vector
#'
#' @param x data frame of data containing continuous variable to be imputed. Should be
#' arranged, as nearest neighbours are based on relative indices.
#' @param k number of nearest neighbours. Defaults to 3.
#' @param value name of variable containing missing values
#' @param group optional name of variable containing classification groups to split on.
#' This is to ensure that nearest-neighbours are sourced from the same classification
#' group, not an adjacent one.
#' @details Yeah we're a little light on these at the mo. This is just a really simple
#' implementation of k-nearest neighbours that only takes into account the variable that
#' is being imputed (i.e., does not find nearest neighbours based on other grouping
#' variables) and assumes the data frame is sorted such that relative indices are used
#' to approximate nearest neighbours. Data should be in a long
#' format. If there are two equal-distance nearest neighbours but only one can be taken through,
#' a random one is chosen.
#' @return data frame with imputed values.
#' Question: Does a second imputed
#' value depend on a first? That is, if a value is imputed, can it be used to impute
#' another value?
#' @examples
#' # make a random 10% of iris Sepal.Length observations NA
#' iris_nas <- iris
#' iris_nas[sample(nrow(iris), size = 0.1 * nrow(iris)), "Sepal.Length"] <- NA
#' # impute with 3-nearest neighbours
#' basic_knn(iris_nas, k = 3, value = "Sepal.Length", group = "Species")
#' @export
basic_knn <- function(x, k = 3, value, group) {

    decimalplaces <- function(x) {
        if (abs(x - round(x)) > .Machine$double.eps^0.5) {
            nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
        } else {
            return(0)
        }
    } # credit daroczig https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r

    # get highest precision for later rounding. just using first value.
    signifs <- decimalplaces(x[, value][1])

    # specify groups of data
    # TODO handle if there are none
    groups <- unique(x[, group])

    for (i in groups) {

        values <- x[x[, group] == i, value]
        na_indices <- setNames(which(is.na(values)), rep(0, length(which(is.na(values)))))
        # values <- c(4, 5, NA, 6, 7)

        # loop over each NA values
        for (j in na_indices) {

            # vector to store the values of neighbours
            neighbours <- rep(NA, k)
            distance <- 1

            # keep trying to find neighbours until we've got three non-NAs
            while (any(is.na(neighbours))) {

            # get both equally distant candidates at a particular vector distance
            equal_dist_candidates <- c(values[j + distance], values[j - distance])
            equal_dist_candidates <- equal_dist_candidates[!is.na(equal_dist_candidates)]

            # if there's enough space for all candidates, and at least one is non-NA, then chuck them in
            if (sum(is.na(neighbours)) >= length(equal_dist_candidates) & length(equal_dist_candidates)) {
                neighbours[which(is.na(neighbours))][1:length(equal_dist_candidates)] <- equal_dist_candidates
              # otherwise, take one at random
            } else {
                neighbours[which(is.na(neighbours))] <- equal_dist_candidates[sample(2, 1)]
            }

            distance <- distance + 1
            }
            # impute based on mean k-nn
            names(na_indices)[na_indices == j] <- round(mean(neighbours), signifs)
        }

        # add back in to original vector
        values[na_indices] <- as.numeric(names(na_indices))
        x[x[, group] == i, value] <- values
    }
    x
}


