#' Create BHHI project site
#'
#' @param dir Path to the directory holding the site
#' @param title Title of the site
#' @param gh_pages Will the site be hosted on GitHub pages
#' @param open Open a new RStudio session in the site directory
#'
#' @export
create_bhhi_site = function(dir, title, gh_pages = TRUE, open = TRUE) {
  # create the distill site
  distill::create_blog(dir, title, gh_pages, edit = FALSE)

  # remove posts
  unlink(file.path(dir, "_posts"), recursive = TRUE)
  unlink(file.path(dir, "docs", "posts"), recursive = TRUE)
  # remove about page
  unlink(file.path(dir, "about.Rmd"))
  unlink(file.path(dir, "docs", "about.html"))

  # create _analyses directory
  dir.create(file.path(dir, "_analyses"))

  # read in _site.yaml
  site_yaml = yaml::read_yaml(file.path(dir, "_site.yml"))
  # delete existing _site.yml
  unlink(file.path(dir, "_site.yml"))

  # add the analyses collection
  site_yaml$collections$analyses$categories = TRUE

  # read in index.Rmd
  index_yaml <- readLines(file.path(dir, "index.Rmd"), n = 5)
  # delete existing index.Rmd
  unlink(file.path(dir, "index.Rmd"))
  # swap in listing: analyses
  index_yaml[which(index_yaml == "listing: posts")] = "listing: analyses"
  # write new index.Rmd
  writeLines(index_yaml, file.path(dir, "index.Rmd"))

  # remove the about post from the navbar
  site_yaml$navbar$right = purrr::discard(
    site_yaml$navbar$right,
    ~ .x == "About" || .x == "about.html"
  )

  # disable site search
  site_yaml$navbar$search = FALSE

  # disable citations
  site_yaml$collections$citations = FALSE

  # copy theme css
  file.copy(system.file("templates", "theme.css", package = "bhhidistill"), dir)
  # add theme to _site.yml
  site_yaml$theme = "theme.css"

  # write the new _site.yml
  yaml::write_yaml(site_yaml, file.path(dir, "_site.yml"))

  # re-render the site
  rmarkdown::render_site(dir)

  if (open && dir != ".") {
    usethis::proj_activate(dir)
  }
}
