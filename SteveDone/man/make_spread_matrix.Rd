% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_spread_matrix.R
\name{make_spread_matrix}
\alias{make_spread_matrix}
\title{Make Spread Matrix}
\usage{
make_spread_matrix(frame, user, item, rating)
}
\arguments{
\item{frame}{Dataframe of users, items and ratings.}

\item{user}{Column name of users column.}

\item{item}{Column name of items column.}

\item{rating}{Column name of items column.}
}
\value{
Returns a spread matrix.
}
\description{
Makes a spread matrix from a data frame of users, items and ratings.
The rows of the spread matrix represent users, the columns represent items and the values represent ratings.
}
\examples{
 make_spread_matrix(book_ratings,'User.ID', 'ISBN', 'Book.Rating')
}
