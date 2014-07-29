# Replacement of multiple patterns in character vectors

# Inputs ------------------------------------------------------------------

# text - A vector of character strings; the elements of the vector are the names
# which are to be modified

# table -  A two-column character matrix; the first column holds the patterns
# and the second holds the replacements


# Outputs -----------------------------------------------------------------

# text - A vector of character strings; the elements of the vector are the names
# as modified


# Description -------------------------------------------------------------

# This function applies gsub() repeatedly and where pattern and substitution
# values are pulled from a table. Calling this 'clean_names' but it's a bit more
# general.


# Function ----------------------------------------------------------------
clean_names <- function(text,table) {
    if (!is.matrix(table)) stop("Input table is not a matrix")
    text <- tolower(text)
    for (row in 1:nrow(table)) {
        text <- gsub(pattern = table[row,1], replacement = table[row,2], x = text)
    }
  return(text)
}