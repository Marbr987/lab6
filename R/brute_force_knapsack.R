#' brute_force_knapsack
#' @description An algorithm that tries every possible alternatives of items in the knapsack and returns the optimal solution.
#' @param x dataframe with columns w and v (weight and value) representing the items
#' @param W double representing the weight limit of the knapsack
#' @return list with the value of the items in the knapsack and the elements contained in the knapsack.
#' @importFrom parallel mclapply
#' @export

brute_force_knapsack <- function(x, W, parallel=FALSE){
  if(!identical(names(x), c("w", "v")) || ncol(x) != 2 || typeof(x) != "list"){stop("x must be a dataframe with column names w and v")}
  if(W < 0){stop("W must not be negative")}
  
  values <- c()
  weights <- c()
  counter <- 1:2^nrow(x)
  vec_length <- 1:nrow(x)
  combinations <- lapply(counter-1, intToBits)
  combinations <- lapply(combinations, function(x){as.numeric(x)})
  combinations <- lapply(combinations, function(x){x[vec_length]})
  if(parallel){
    temp_dfs <- parallel::mclapply(combinations, function(y){y * x})
    values <- sapply(temp_dfs, function(x){sum(x$v)})
    weights <- sapply(temp_dfs, function(x){sum(x$w)})
  }
  else{
    for (i in counter) {
      temp_df <- combinations[[i]] * x
      values[i] <- sum(temp_df$v)
      weights[i] <- sum(temp_df$w)
    }
  }
  relevant_indices <- which(weights <= W)
  max_value <- max(values[relevant_indices])
  result_index <- which(values == max_value)
  elements <- which(combinations[[result_index]] == 1)
  
  
  return(list(value = round(max_value), elements = elements))
}
