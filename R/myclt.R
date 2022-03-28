#' @title Central Limit Theorem function from Lab 8
#'
#' @param n size
#' @param iter iterations
#'
#' @return plot
#' @export
#' @importFrom graphics hist
#' @importFrom stats runif
#'
#' @examples
#' \dontrun{w = myclt(n = 10, iter = 10000)}
myclt = function(n, iter)
{
  # Creates a sample of size n*iter from a uniform distribution
  # that has lower limit 0 and upper limit 5.
  y = runif(n*iter, 0, 5)

  # Creates a matrix with the sample from line A that has n rows
  # and iter columns arranged row by row.
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Applies the sum function to the columns of data from the
  # matrix previously created.
  sm = apply(data, 2, sum)

  # Creates a histogram from the results above
  hist(sm)
  sm
}
