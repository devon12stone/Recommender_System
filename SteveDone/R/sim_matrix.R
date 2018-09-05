#' Item Similarity Matrix
#'
#' This function returns a similarity matrix for items in an item based remommender system.
#' @param ma The transpose of the spread matrix created for the recommender system. The rows of the spread matrix represent users, the columns represent items and the values represent ratings.
#' @param mb The same parameter as above.
#'
#' @return Similarity matrix between the items in a an item based remommender system.
#' @export
#'
#' @examples sim_matrix(t(book_ratings_matrix), t(book_ratings_matrix))
sim_matrix =function(ma, mb){
  mat=tcrossprod(ma, mb)
  t1=sqrt(apply(ma, 1, crossprod))
  t2=sqrt(apply(mb, 1, crossprod))
  mat / outer(t1,t2)
}
