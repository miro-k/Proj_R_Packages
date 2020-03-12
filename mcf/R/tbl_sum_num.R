#' @title Summary table for numeric variables
#'
#' @description Table of selected summary statistics for numeric variables
#'
#' @details The argument must be a data frame (if it's an atomic vector, coerce it into
#' data frame first), and it must contain only numeric variables (doubles or integers).
#' If non-numeric variables are present, an error message 'All variables must be numeric'
#' will show. Note that while dates and times are, technically, numeric, they need to be
#' treated differently when it comes to summaries and descriptions (see
#' \code{tbl_sum_dates() in the \code{mcf} package}).
#'
#' @param x Data frame containing only numeric variables (double, integer, but no date/time)
#' @param round_to An integer representing the number of decimal places to which to round
#' the summary statistics in the resulting table; default: round_to = 2.
#' @param sorted TRUE if the table should sorted by the variable names, FALSE if
#' otherwise (the default).
#'
#' @return A data frame consisting of statistics typically uses to describe numeric
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
#' df[2:3, 1] <- NA
#' df[3:5, 2] <- NA
#' df[4:5, 3] <- NA
#' df[c(3,5), 4] <- NA
#'
#' num_vars <- unlist(lapply(df,class)) %in% c("numeric", "integer")
#' df_num <- df[, num_vars]
#'
#' str(df)
#' str(df_num)
#'
#' tbl_sum_num(df) # This will result in error because of non-numeric variables present
#' tbl_sum_num(df_num) # Default option values
#' tbl_sum_num(df_num, round_to = 1, sorted = TRUE) # Option values specified
#'
#' @export
#'
tbl_sum_num <- function( x, round_to = 2, sorted = FALSE ) {

   only_numeric_vars <- prod(
      unlist(
         lapply( x, function(arg) { ifelse( class(arg) == "numeric" |
                                            class(arg) == "integer", 1, 0 ) } )
      )
   )

   if (only_numeric_vars == 0) {

      stop("All variables must be numeric!")

   } else {

   N <- unlist(lapply(x, function(arg) { sum(!is.na(arg)) }))
   N_miss = unlist(lapply(x, function(arg) { sum(is.na(arg)) }))
   mean <- round( unlist( lapply( x, mean, na.rm = TRUE ) ), round_to)
   sd   <- round( unlist( lapply( x, sd, na.rm = TRUE ) ), round_to)
   min  <- round( unlist( lapply( x, min, na.rm = TRUE ) ), round_to)
   max  <- round( unlist( lapply( x, max, na.rm = TRUE ) ), round_to)
   p25  <- round( unlist( lapply( x, quantile, 0.25, na.rm = TRUE ) ), round_to)
   p50  <- round( unlist( lapply( x, quantile, 0.50, na.rm = TRUE ) ), round_to)
   p75  <- round( unlist( lapply( x, quantile, 0.75, na.rm = TRUE ) ), round_to)

   out <- cbind.data.frame(N, N_miss, mean, sd, min, max, p25, p50, p75)

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


