#' Convert backslashes to forward slashes directly from the clipboard
#'
#' @return The string copied to the clipboard is returned with backslashes
#replaced with forward slashes.


format_clip <- function() {
    gsub("\\\\", "/", readClipboard())
}
