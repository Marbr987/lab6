#' brute_force_knapsack
#' @description An algorithm that tries every possible alternatives of items in the knapsack and returns the optimal solution.
#' @param x dataframe with columns w and v (weight and value) representing the items
#' @param W double representing the weight limit of the knapsack
#' @return list with the value of the items in the knapsack and the elements contained in the knapsack.
#' @export

brute_force_knapsack <- function(x, W){
  if(names(x) != c("w", "v") || ncol(x) != 2 || typeof(x) != "list"){stop("x must be a dataframe with column names w and v")}
  if(W < 0){stop("W must not be negative")}
  value = c()  
  elements=c()
  weight=c()
  value=c()
  #Going through rows until we reach the end.
  i=1
  while(i <= nrow(x)){ 
  w = as.data.frame(combn(x[,1], i)) 
  v = as.data.frame(combn(x[,2], i)) 
  sum_w = colSums(w)
  sum_v = colSums(v)
  #sum the largest from whole weight combination.
  weight = which(sum_w<= W) 
  #item with largest value for whole weight.
  if(length(weight) != 0)
  { 
  value = sum_v[weight]
  value =max(value)
  A= which((value) == value)
  maxValWghtIdx =weight[A]
  maxValWght = w[,maxValWghtIdx]
  #add number of chosen items with max value to some_element 1.
  j = 1
  while (j<=i){
  elements[j] =which(x[,1] == maxValWght[j])
  j=j+1
  }
  }
  i=i+1
  }
  return(list(value = round(value), elements = elements))
  return(result)
}
