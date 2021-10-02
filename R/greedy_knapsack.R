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
  
  perm = order(x$v/x$w, decreasing = TRUE)
  x <- x[perm,]
  i <- 1
  sum <- 0
  while(sum <= W) {
    sum <- sum(x$w[1:i])
    i = i + 1
  }
  total_value <- sum(x$v[1:(i-2)])
  elements <- as.numeric(rownames(x[1:(i-2),]))
  
  result <- list(value = round(total_value), elements = elements)
  return(result)
}

RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
