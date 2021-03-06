% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brute_force_knapsack.R
\name{brute_force_knapsack}
\alias{brute_force_knapsack}
\title{brute_force_knapsack}
\usage{
brute_force_knapsack(x, W, parallel = FALSE)
}
\arguments{
\item{x}{dataframe with columns w and v (weight and value) representing the items}

\item{W}{double representing the weight limit of the knapsack}

\item{parallel}{boolean whether the code should be run in parallel or not. Default is to FALSE.}
}
\value{
list with the value of the items in the knapsack and the elements contained in the knapsack.
}
\description{
An algorithm that tries every possible alternatives of items in the knapsack and returns the optimal solution.
}
