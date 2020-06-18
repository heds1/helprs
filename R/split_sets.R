#' Split a dataframe into training, test and (if required) validation sets
#' @param x A dataframe to be split into training and testing (and, if required,
#' validation) sets. Must have nrow >= 20.
#' @param props Numeric vector of proportions to split the dataframe by. Must be
#' length 2 or 3. The first
#' element is the proportion of observations to be assigned to the training set;
#' the second element is the proportion of observations to be assigned to the
#' testing set; and the optional third element is the proportion of observations
#' to be assigned to the validation set. Defaults to `c(0.7, 0.3)`, where
#' approximately 70% of observations are assigned to the training set, and 30%
#' of observations are assigned to the testing set.
#' @return A list of dataframes of length 2 or 3, depending on whether `validation` is set to
#' FALSE or TRUE. 
#' @examples 
#' split_dfs <- split(mtcars, props = c(0.6, 0.4))

split_sets <- function(x, props = c(0.7,0.3), validation = FALSE) {

    # validate arguments
    if (nrow(x) < 20) {
        stop("Dataframe must have at least 20 observations.")
    }
    if (!(length(props) %in% c(2, 3))) {
        stop("The props argument needs to be a numeric vector of length 2 or 3.")
    }
    if (class(props) != "numeric") {
        stop("The props argument needs to be a numeric vector of length 2 or 3.")
    }

    if (validation == TRUE) {

        # if no props argument is provided, default to 6:2:2
        if (length(props)==2) props = c(0.6,0.2,0.2)

        # create vector of assignments
        assignments <- sample(
            x = c("train","test","validation"),
            size = nrow(x),
            replace = TRUE,
            prob = props)


        x$AssignedSet <- assignments
        TrainingData <- subset(x, AssignedSet == 'train')
        TestData <- subset(x, AssignedSet == 'test')
        ValidationData <- subset(x, AssignedSet == 'validation')
        TrainingData <- drop_var(TrainingData, "AssignedSet")
        TestData <- drop_var(TestData, "AssignedSet")
        ValidationData <- drop_var(ValidationData, "AssignedSet")

        return (list(train=TrainingData, test=TestData, validation=ValidationData))

    } else if (validation == FALSE) {

        if (length(props) == 3) {
            stop("You have provided three elements in the props argument, but have set validation = FALSE. Did you mean to set validation = TRUE?")
        }

        assignments <- sample(c(TRUE,FALSE),nrow(x),replace=TRUE,prob=props)

        x$IsTraining <- assignments

        TrainingData <- subset(x, IsTraining)
        TrainingData <- TrainingData[,names(TrainingData)!="IsTraining"]

        TestData <- subset(x, IsTraining == FALSE)
        TestData <- TestData[,names(TestData)!="IsTraining"]

        return (list(train=TrainingData, test=TestData))
    }
}
