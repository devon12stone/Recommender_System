% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_user_user_based.R
\name{new_user_user_based}
\alias{new_user_user_based}
\title{New User for User Based Recommender}
\usage{
new_user_user_based(user, ratings, items, item_col, rating_col)
}
\arguments{
\item{user}{A dataframe in the form of items and ratings.}

\item{ratings}{Spread matrix in the form of users (rows), items (columns) and ratings (values).}

\item{items}{Dataframe of items and information on the items.}

\item{item_col}{The column name of the items column in the datafram containing the ratings.}

\item{rating_col}{The column name of the ratings column in the datafram containing the ratings}
}
\value{
Returns a sorted dataframe of the top 10 rated items, the score of the item and information on each item.
}
\description{
This function adds a new user to the spread matrix of the user based recommender system.
Then the funciton returns the top 10 rated user rated items for the user.
}
\examples{
new_user <- data.frame(ISBN= c('0440234743', '0971880107', '0345417623'), Book.Rating = c(2, 5, 3))
new_user_user_based(new_user, book_ratings_matrix, book_info,'ISBN','Book.Rating')
}
