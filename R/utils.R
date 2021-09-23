#' Print commit
#'
#' Prints the latest commit SHA
#'
#' @return "Commit: <last commit SHA>"
#' @export
print_commit = function() {
  if (git2r::in_repository()) {
    paste("<span class=commit-label>Commit:", substr(git2r::last_commit()[["sha"]], 1, 7), "</span>")
  }
}
