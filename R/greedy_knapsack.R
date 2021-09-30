#' greedy_knapsack
#' @description A greedy heuristic for solving the knapsack problem. 
#' The solution does not represent the optimal solution, but is computationally cheap.
#' @param x dataframe with columns w and v (weight and value) representing the items
#' @param W double representing the weight limit of the knapsack
#' @return list with the value of the items in the knapsack and the elements contained in the knapsack.
#' @export
greedy_knapsack <- function(x, W){
  # Check if input x is correct
  if(names(x) != c("w", "v") || ncol(x) != 2 || typeof(x) != "list"){stop("x must be a dataframe with column names w and v")}
  # some code
  result <- list(value="some number", elements=c("some element1", "some element2"))
  return(result)
}