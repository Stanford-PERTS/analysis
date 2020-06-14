ok_comma <- function(FUN) {
  # Permits a dangling comma in the argument list, which is often an eaiser way
  # to write high-touch code whose lines are rearranged frequently.
  #
  # Returns a wrapped version of the input function which throws away the last
  # argument if that argument is missing.
  #
  # Example:
  #
  # my_vec <- ok_comma(c)(
  #   1,
  #   2,  # note the final comma, yet no error
  # )
  function(...) {
    arg.list <- as.list(sys.call())[-1L]
    len <- length(arg.list)
    if (len > 1L) {
      last <- arg.list[[len]]
      if (missing(last)) {
        arg.list <- arg.list[-len]
      }
    }
    do.call(FUN, arg.list)
  }
}
