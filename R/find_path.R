find_path <- function(.data, start, end, type) {
  if (type == "mincost") {
    return(.min_cost(.data, end[1], end[2]))
  }
}

.min_cost <- function(.data, row, col) {
  if (row < 1 | col < 1) { return(Inf) }
  else if (row == 1 & col == 1) { return(.data[row,col]) }
  else {
    cost <- .data[row, col] + min(
      .min_cost(.data, row - 1, col - 1),
      .min_cost(.data, row - 1, col),
      .min_cost(.data, row, col - 1)
    )
    return(cost)
  }
}

.lee <- function(.data, row, col) {

}
