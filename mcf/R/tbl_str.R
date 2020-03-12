#' @title Data frame descriptives
#' 
#' @description A table of basic descriptive statistics for a given data frame.
#' 
#' @details \code{tbl_str} returns a table of basic descriptive statistics for a given
#' data frame. In terms of R objects, the returned table is itself a data frame and can
#' be manipulated as such. Most of the table's columns are self-explanatory, 
#' \code{n} is the observation count, i.e., the number of non-missing observations,
#' \code{n_na} is the number of missing observations, and \code{n_unique} is the number
# of unique values within the variable (note that NA counts as a category).
#' 
#' @param x Data frame
#' 
#' @param ordered Set to TRUE if the table is to be ordered alphabetically by the
#' variable names
#' 
#' @param n_examples Number of examples of the values of each variable to show in
#' the table; the default value is 3 examples.
#' 
#' @param truncate_examples_at Number of characters after which the examples of each
#' variable are cut off. This is to prevent examples that are long strings to mess
#' us displaying the table; by default, all examples are truncated at 50 characters.
#' 
#' @return A data frame containing basic descriptive statistics
#' 
#' @examples
#' # Generate a data frame with different types of variables; also introduce a handful
#' of missing values
#' 
#' model <- rownames(mtcars)
#' mpg   <- as.double(mtcars$mpg)
#' cyl   <- as.integer(mtcars$cyl)
#' am    <- as.logical(mtcars$am)
#' gear  <- as.ordered(mtcars$gear)
#' 
#' df <- cbind.data.frame(model, mpg, cyl, am, gear, row.names = NULL,
#'       stringsAsFactors = FALSE);
#'       df[1:5, 2] <- NA; df[11:20, 3] <- NA
#' 
#' tbl_str(df, ordered = TRUE, n_examples = 3)
#' 
#' @export
#' 
tbl_str <- function( x,
                     ordered = FALSE,
                     n_examples = 3,
                     truncate_examples_at = 50) {
   
   variable  <- names(x)
   
   class <- cbind.data.frame(
      variable,
      # Note that the 'class()' function can return a character vector with more
      # than one element (e.g., c("ordered", "factor"))
      class = unlist( lapply( lapply(x, class), paste, collapse = " " ) ),
      row.names = NULL,
      stringsAsFactors = FALSE )
   
   n <- cbind.data.frame(
      variable,
      n = unlist(lapply(x, function(arg) { sum(!is.na(arg)) })),
      row.names = NULL,
      stringsAsFactors = FALSE )

   n_na <- cbind.data.frame(
      variable,
      n_na = unlist(lapply(x, function(arg) { sum(is.na(arg)) })),
      row.names = NULL,
      stringsAsFactors = FALSE )

   n_unique <- cbind.data.frame(
      variable,
      n_unique = unlist(lapply(x, function(arg) { length(unique(arg)) })),
      row.names = NULL,
      stringsAsFactors = FALSE )

   examples <- cbind.data.frame(
      variable,
      examples = apply( t( head(x, n_examples) ),
                        FUN = function(arg) { paste(arg, collapse = ", ") },
                       MARGIN = 1 ),
      row.names = NULL,
      stringsAsFactors = FALSE )
   
   examples$examples <- substr(examples$examples, start = 1, stop = truncate_examples_at)
   
   out <- merge(class, n, by = "variable", all = TRUE, sort = ordered)
   out <- merge(out, n_na, by = "variable", all = TRUE, sort = ordered)
   out <- merge(out, n_unique, by = "variable", all = TRUE, sort = ordered)
   out <- merge(out, examples, by = "variable", all = TRUE, sort = ordered)
   out <- cbind.data.frame(column = 1:nrow(out), out)
   
   options(width = 10000); print.data.frame(out, row.names = FALSE)
   invisible(out)
}




