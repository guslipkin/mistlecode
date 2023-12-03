#' A Pipeable Wrapper For `dbscan`
#'
#' @details
#' Adds a new column named `clust` to the `data` with the cluster results
#'
#' @inheritParams dbscan::dbscan
#' @param data A data.frame.
#' @inherit dbscan::dbscan return
#' @export
dbscan <- function(data, eps, minPts, borderPoints = TRUE) {
  data$clust <- dbscan::dbscan(data, eps, minPts, borderPoints)$cluster
  return(data)
}
