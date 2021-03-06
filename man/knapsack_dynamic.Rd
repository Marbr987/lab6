% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_dynamic.R
\name{knapsack_dynamic}
\alias{knapsack_dynamic}
\title{knapsack_dynamic}
\usage{
knapsack_dynamic(x, W)
}
\arguments{
\item{x}{dataframe with columns w and v (weight and value) representing the items}

\item{W}{double representing the weight limit of the knapsack}
}
\value{
list with the value of the items in the knapsack and the elements contained in the knapsack.
}
\description{
A faster algorithm to get the optimal solution to the knapsack problem through iteration.
}
