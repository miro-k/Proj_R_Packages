
rm(list = ls())

lapply(c("mcf", "magrittr", "tidyverse"), library, character.only = TRUE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# tbl_freq(df$x, include_NA = "ifany", sort_by = NULL)
# tbl_freq(df$x, include_NA = "ifany", sort_by = "Count")
# tbl_freq(df$x, include_NA = "ifany", sort_by = "Categ")

df <- data.frame( x = c("a", NA, NA,"b","b","c"),
                  y = c("d","e","d", NA,"d","e"),
                  z = c("f","f", NA, NA,"g","g") )

# When input is a single vector, output is a data frame
mcf::tbl_freq(df$x, include_NA = "no")
mcf::tbl_freq(df$x, include_NA = "ifany", sort_by = "Count")

# When input is a data frame, the function outputs a list of sub-lists of data frames
tlist <- lapply(df, tbl_freq, include_NA = "ifany")
str(tlist [1])  # This is a sub-list
str(tlist[[1]]) # This is a data frame
sapply(tlist, print) # Print all 3 distributions on screen
DT::datatable(tlist[[1]]) # Use DT package to create an interactive table
