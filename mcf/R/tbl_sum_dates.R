# table_sum_dates() ===========================================================================

# Return(s): a table of selected summary statistics for date variables; output table is
# an object of type data frame.

# Argument(s): 'x' = must be a data frame (if an atomic vector, it must be coerced into
# data frame first), and the data frame must contain only date variables (otherwise
# an error message is produced), 'round_to' = number of digits to round the proportions
# to, 'ordered' if TRUE, then the table will be ordered alphabetically by the variable
# (column) names.


#' @title Summary table for class date variables
#'
#' @description Table of selected summary statistics for date variables
#'
#' @details The argument must be a data frame (if it's an atomic vector, coerce it into
#' data frame first), and it must contain only date variables. If non-date variables are
#' present, an error message 'All variables must be class date' will show.
#'
#' @param x Data frame containing only date variables
#' @param sorted TRUE if the table should be sorted by the variable names, FALSE if
#' otherwise (the default).
#'
#' @return A data frame consisting of statistics typically used to describe date
#' variables.
#'
#' @examples
#' df <- data.frame( x1 = c(1:9)*0.1,
#'                   x2 = as.integer(c(1:9)*5),
#'                   x3 = c(1:9)*10,
#'                   x4 = seq(from = as.Date("1970-01-01"), by = "day", length.out = 9),
#'                   x5 = letters[1:9],
#'                   x6 = as.factor(sample(c("f1", "f2", "f3"), 9, replace = TRUE)),
#'                   x7 = seq(from = as.Date("2018-05-15"), by = "5 days", length.out = 9),
#'                   stringsAsFactors = FALSE )
#'
#' df[2:3, 1] <- NA; df[3:5, 2] <- NA; df[4:5, 3] <- NA; df[c(3,5), 4] <- NA
#'
#' date_vars <- unlist(lapply(df,class)) == "Date" # Identify date variables
#' df_dates <- df[, date_vars] # Data frame of only date variables
#'
#' tbl_sum_dates(df) # Error is returned because of non-date variables
#' tbl_sum_dates(df_dates)
#' tbl_sum_dates(df_dates, sorted = TRUE)
#'
#' @export
#'
tbl_sum_dates <- function( x, sorted = FALSE ) {

   only_date_vars <- prod(
      unlist(
         lapply( x, function(arg) { ifelse( class(arg) == "Date", 1, 0 ) } )
      )
   )

   if (only_date_vars == 0) {

      stop("All variables must be class 'Date'!")

   } else {

   N <- unlist(lapply(x, function(arg) { sum(!is.na(arg)) }))
   N_miss <- unlist(lapply(x, function(arg) { sum(is.na(arg)) }))
   mean <- as.Date( unlist( lapply( x, mean, na.rm = TRUE ) ), origin = "1970-01-01" )
   sd   <- round( unlist( lapply( x, sd, na.rm = TRUE ) ), 2 )
   min  <- as.Date(unlist( lapply( x, min, na.rm = TRUE )), origin = "1970-01-01")
   max  <- as.Date(unlist( lapply( x, max, na.rm = TRUE )), origin = "1970-01-01")

   out <- cbind.data.frame(N, N_miss, mean, sd, min, max)

   out <- cbind.data.frame(colname = rownames(out),
                           out,
                           row.names = NULL,
                           stringsAsFactors = FALSE)

   out <- if( sorted ) { out[order(out$colname), ] } else { out }

   }

   # return(out)
   options(width = 10000); print.data.frame(out, row.names = FALSE)
   invisible(out)
}






