% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facto_based_rating.R
\name{facto_based_rating}
\alias{facto_based_rating}
\title{Matrix Factorization Based Rating}
\usage{
facto_based_rating(user, ratings, changed, k_in, user_col, item_col,
  rating_col, a, b, items)
}
\arguments{
\item{user}{User ID.}

\item{ratings}{A dataset of users, items and ratings that has been removed of zero ratings.}

\item{changed}{A matrix in the form of the User ID, index of the user's first rating that was set to NA,
index of the user's second rating that was set to NA, original value of the user's first rating
and original value of the user's second rating.}

\item{user_col}{The name of the user ID column in the ratings dataframe.}

\item{item_col}{The name of the item ID column in the ratings dataframe.}

\item{rating_col}{The name of the ratings column in the ratings dataframe.}

\item{a}{The L2 regularisation of W latent factors for the matrix decomposition.}

\item{b}{The L2 regularisation of H latent factors for the matrix decomposition}

\item{items}{Dataframe of items and information on the items.}

\item{k}{The integer of decomposition rank of the decomposition matrix.}
}
\value{
Returns a sorted dataframe of the top 10 rated items, the score of the item and information on each item.
}
\description{
Matrix Factorization Based Rating
}
\examples{
facto_based_rating(277042 ,book_ratings, changed_inds,5,'User.ID', 'ISBN', 'Book.Rating', 0.2,0.2, book_info)
}
