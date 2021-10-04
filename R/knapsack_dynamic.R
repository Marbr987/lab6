#' knapsack_dynamic
#' @description A faster algorithm to get the optimal solution to the knapsack problem through iteration.
#' @param x dataframe with columns w and v (weight and value) representing the items
#' @param W double representing the weight limit of the knapsack
#' @return list with the value of the items in the knapsack and the elements contained in the knapsack.
#' @export
knapsack_dynamic <- function(x, W){
  # Check if input x is correct
  if(!identical(names(x), c("w", "v")) || ncol(x) != 2 || typeof(x) != "list"){stop("x must be a dataframe with column names w and v")}
  if(W < 0){stop("W must not be negative")}
  
  # Calculate the max value
  m <- matrix(nrow = nrow(x) + 1, ncol = W + 1, dimnames = list(0:nrow(x), 0:W))
  m[0+1,] <- 0
  m[,0+1] <- 0
  for (i in 1:nrow(x)) {
    for (j in 0:W){
      if (x$w[i] > j) {
        m[i+1, j+1] <- m[i, j+1]
      }
      else {
        m[i+1, j+1] <- max(m[i, j+1], m[i, j-x$w[i]+1] + x$v[i])
      }
    }
  }
  
  # Get the elements which are in the knapsack for max value
  get_items <- function(i, j){
    if (i == 0){
      return(c())
    }
    else if (m[i+1, j+1] > m[i, j+1]){
      return(append(get_items(i-1, j-x$w[i]), i))
    }
    else {
      return(get_items(i-1, j))
    }
  }
  elements <- get_items(nrow(x), W)
  
  # Return result
  result <- list(value=round(m[nrow(x)+1,W]), elements=elements)
  return(result)
}
