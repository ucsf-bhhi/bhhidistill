#' Add code version
#'
#' Prints the latest commit SHA and links to GitHub if available
#'
#' @return "Commit: <last commit SHA>"
#' @export
add_version = function() {
  if (git2r::in_repository()) {
    if (length(git2r::remotes()) > 0) {
      commit_string = commit_link()
    } else {
      commit_string = commit_id()
    }
    paste0(
      "## Version {.appendix}\n",
      "Code: ", commit_string
    )
  }
}

commit_link = function() {
  paste0("[", commit_id(), "](", commit_url(), ")")
}

commit_id = function() {
  substr(git2r::last_commit()[["sha"]], 1, 7)
}

commit_url = function() {
  paste0(repo_url(), "/blob/", commit_id(), file_path())
}

repo_url = function() {
  {
    remote_url = git2r::remote_url()
    if (substr(remote_url, 1, 4) == "http") return(substr(remote_url, 1, nchar(remote_url) - 4))

    if (substr(remote_url, 1, 3) == "git") {
      repo_name = substr(remote_url, 16, nchar(remote_url) - 4)
      return(paste0("https://github.com/", repo_name))
    }
  }
}

file_path = function() {
  abs_path = knitr::current_input(dir = TRUE)
  project_root = rprojroot::find_root(rprojroot::is_git_root)
  gsub(project_root, "", abs_path)
}

