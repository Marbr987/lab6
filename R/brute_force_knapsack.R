#' brute_force_knapsack
#' @description An algorithm that tries every possible alternatives of items in the knapsack and returns the optimal solution.
#' @param x dataframe with columns w and v (weight and value) representing the items
#' @param W double representing the weight limit of the knapsack
#' @return list with the value of the items in the knapsack and the elements contained in the knapsack.
#' @export
brute_force_knapsack <- function(x, W){
  # some code
  result <- list(value="some number", elements=c("some element1", "some element2"))
  return(result)
}