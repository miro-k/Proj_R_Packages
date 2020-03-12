#' @title Duration of executing an R code
#'
#' @description Time it takes to execute a given code.
#'
#' @details Returns how many seconds it took to exectue a given
#' line/block of code. Note that if the code extends over multiple lines,
#' it has to be enclosed in curly brackets.
#'
#' @param ... All R code to be executed
#'
#' @return Duration of code execution in seconds
#'
#' @examples
#' x <- matrix(runif(1e7), ncol = 1e5)
#'
#' time_to_execute( unlist(lapply(list(x), colMeans)) )
#'
#' # Curly brackets for multiple lines of code:
#' time_to_execute( which_time = "elapsed",
#'                  { out_loop <- c()
#'                   for (i in seq_len(ncol(x))) { out_loop[[i]] <- mean(x[ , i]) }
#'                  } )
#'
#' @export
#'
time_to_execute <- function( ..., which_time = "elapsed" ) {

   duration <- system.time( eval( ... ) )

   if (which_time == "elapsed") { out <- duration["elapsed"] }
   if (which_time == "user") { out <- duration["user.self"] }
   if (which_time == "system") { out <- duration["sys.self"] }

   return(out)
}
