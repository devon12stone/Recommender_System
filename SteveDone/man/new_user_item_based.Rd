% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_user_item_based.R
\name{new_user_item_based}
\alias{new_user_item_based}
\title{New User Item Based Rating}
\usage{
new_user_item_based(user, ratings, items, similarities, item_col,
  rating_col)
}
\arguments{
\item{user}{A dataframe in the form of items and ratings.}

\item{ratings}{Spread matrix in the form of users (rows), items (columns) and ratings (values).}

\item{items}{Dataframe of items and information on the items.}

\item{similarities}{Similarity matrix of items in the system.}

\item{item_col}{The column name of the items column in the datafram containing the ratings.}

\item{rating_col}{The column name of the ratings column in the datafram containing the ratings}
}
\value{
Returns a sorted dataframe of the top 10 rated items, the score of the item and information on each item.
}
\description{
This function adds a new user to the spread matrix of the item based recommender system.
Then the funciton returns the top 10 rated item rated items for the user.
}
\examples{
new_user <- data.frame(ISBN= c('0440234743', '0971880107', '0345417623'), Book.Rating = c(2, 5, 3))
new_user_item_based(new_user, book_ratings_matrix, book_info, book_similarities, 'ISBN', 'Book.Rating')
}
