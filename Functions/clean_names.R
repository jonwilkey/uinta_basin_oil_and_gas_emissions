##' Multiple gsub() from a table of pattern-replacement values
##'
##' This function applies gsub() repeatedly and where pattern and substitution values are pulled from a table. Calling this 'clean_names' but it's a bit more general.
##' @title Replacement of multiple patterns in character vectors
##' @param text A vector of character strings; the elements of the vector are the names which are to be modified
##' @param table a two-column character matrix; the first column holds the patterns and the second holds the replacements
##' @return A vector of character strings; the elements of the vector are the names as modified
##' @author Michael Hogue
##' @note none
##' @examples
##'

clean_names <- function(text,table) {
    if (!is.matrix(table)) stop("Input table is not a matrix")
    text <- tolower(text)
    for (row in 1:nrow(table)) {
        text <- gsub(pattern = table[row,1], replacement = table[row,2], x = text)
    }
  return(text)
}
 
