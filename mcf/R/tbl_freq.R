#' @title Distribution of a categorical variable
#'
#' @description Table of frequency distribution (counts and proportions) of categories/levels
#'
#' @details Given a categorical variable (vector), \code{tbl_freq} returns the variable's
#' distribution in the form of both, the category counts as well as proportions.
#' As an R object, the table itself is a data frame, and can be manipulated as such.
#'
#' Note that the argument can be a vector of any type that could conceivably represent
#' a categorical variable; in R these could be character strings, integers or the vector
#' could be directly defined as a factor (categorical variable).
#'
#' Using \code{lapply} with \code{tbl_freq}, a data frame of categorical variables can be
#' used as an argument; see an example of such an application below.
#'
#' @param x Factor/character/integer vector
#' @param include_na Include/exclude NA as a category; one of three choices: "no", "ifany",
#' "always"; default: use_na = "no".
#' @param sort_by By which column should the table be sorted ("Categ", "Count")
#'
#' @return NA
#'
#'#' @examples
#' # When input is a single vector, output is a data frame
#' tbl_freq(df$x, include_NA = "ifany")
#' tbl_freq(df$x, include_NA = "ifany", sort_by = "Count")
#'
#' # When input is a data frame, the function outputs a list of sub-lists of data frames
#' tlist <- lapply(df, tbl_freq, include_NA = "ifany")
#' str(tlist [1])  # This is a sub-list
#' str(tlist[[1]]) # This is a data frame
#' sapply(tlist, print) # Print all 3 distributions on screen
#' DT::datatable(tlist[[1]]) # Use DT package to create an interactive table
#'
#' @export
#'
tbl_freq <- function(x, include_NA = "no", sort_by = NULL) {

    tab <- data.frame(table(x, useNA = include_NA), row.names = NULL)
    names(tab) <- c("Categ", "Count")

    tab$Prop <- round(tab$Count/sum(tab$Count), 3)

    tab$Cum_Count <- cumsum(tab$Count)
    tab$Cum_Prop <- round(cumsum(tab$Prop), 3)

    if (!is.null(sort_by)) {
        tab <- tab[order(tab[sort_by], decreasing = TRUE),]
    }

    return(tab)
}

