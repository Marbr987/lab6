brute_force_knapsack <- function(x, W){
some_number = c()  
some_element1=c()
some_element2= c()
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
some_number =max(value)
A= which((value) == some_number)
maxValWghtIdx =weight[A]
maxValWght = w[,maxValWghtIdx]
#add number of chosen items with max value to some_element 1.
j = 1
while (j<=i){
some_element1[j] =which(x[,1] == maxValWght[j])
j=j+1
}
}
i=i+1
}
return(list(value = round(some_number), elements = some_element1 , some_element2))
}


##Print
{
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000))
}
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)