% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_based_rating.R
\name{user_based_rating}
\alias{user_based_rating}
\title{User Based Rating}
\usage{
user_based_rating(user_id, ratings, items)
}
\arguments{
\item{user_id}{The ID of the user.}

\item{ratings}{Spread matrix in the form of users (rows), items (columns) and ratings (values).}

\item{items}{Dataframe of items and information on the items.}
}
\value{
Returns a sorted dataframe of the top 10 rated items, the score of the item and information on each item.
}
\description{
This funciton returns the top 10 rated user rated items of any user in already in the dataset used to create the recommender system.
}
\examples{
user_based_rating(277042, book_ratings_matrix, book_info)
}
