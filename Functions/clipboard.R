### Copy/Paste function ###

# Inputs ------------------------------------------------------------------

# x - any value, vector, matrix, or data.frame

# OS - Operating system (each uses a different clipboard), valid options are
#      unix (Unix/Linux), win (Windows), and mac (Mac OSX). Current default is
#      unix.


# Outputs -----------------------------------------------------------------

# write.table - a tab-separated version of x which is copied to the clipboard


# Description -------------------------------------------------------------

# This function copies the input object "x" to clipboard so that it can be
# pasted to Excel (or any other spreadsheet program).


# Function ----------------------------------------------------------------
clipboard <- function(x, sep="\t", row.names = FALSE, col.names = TRUE, OS = "win") {
  switch(OS,
         unix = {con <- pipe("xclip -selection clipboard -i", open="w");
                 write.table(x, con, sep=sep, row.names=row.names, col.names=col.names);
                 close(con)},
         win = {write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names)},
         mac = {con <- pipe("pbcopy", open="w");
                write.table(x, con, sep=sep, row.names=row.names, col.names=col.names);
                close(con)}
         )
}