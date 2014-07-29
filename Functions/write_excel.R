### Copy/Paste function ###

# Inputs ------------------------------------------------------------------

# x - any value, vector, matrix, or data.frame


# Outputs -----------------------------------------------------------------

# write.table - a tab-separated version of x which is copied to the clipboard


# Description -------------------------------------------------------------

# This function copies the input object "x" to clipboard so that it can be
# pasted to Excel (or any other spreadsheet program).


# Function ----------------------------------------------------------------
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}