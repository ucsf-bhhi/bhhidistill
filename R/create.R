#' Create BHHI project site
#'
#' @param dir Path to the directory holding the site
#' @param title Title of the site
#' @param gh_pages Will the site be hosted on GitHub pages
#' @param gh_action Include GitHub action that automatically renders new and updated pages.
#' @param open Open a new RStudio session in the site directory
#'
#' @export
create_bhhi_site = function(dir, title, gh_pages = TRUE, gh_action = TRUE, open = TRUE) {
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

  # setup github actions
  if (gh_action) {
    actions_directory = file.path(dir, ".github", "workflows")
    # if the actions directory doesn't exist, create it
    if (!dir.exists(actions_directory)) dir.create(actions_directory, recursive = TRUE)
    # copy the actions script
    file.copy(
      system.file("templates", "render-site.yml", package = "bhhidistill"),
      actions_directory
    )

    # add docs/ to .gitignore since we're rendering on GitHub
    # create .gitignore if it doesn't exist
    if (!file.exists(file.path(dir, ".gitignore"))) file.create(file.path(dir, ".gitignore"))
    # add docs/
    cat("docs/", file = file.path(dir, ".gitignore"), append = TRUE)
  }

  if (open && dir != ".") {
    usethis::proj_activate(dir)
  }
}

#' Create a Distill themed RMarkdown analysis
#'
#' @param title Title of the analysis.
#' @param ... Arguments passed to [distill::create_post()].
#' @param date_prefix Date prefix for post slug (preserves chronological order
#' for posts within the filesystem). Defaults to NULL for no date prefix.
#' @param open Open the post in an editor after creating it.
#'
#' @export
create_analysis = function(title, ..., date_prefix = NULL, open = interactive()) {
  initial_post = distill::create_post(title, collection = "analyses", date_prefix = date_prefix, edit = FALSE, ...)

  yaml = readLines(initial_post, n = 12)

  con <- file(initial_post, open = "w")

  on.exit(close(con), add = TRUE)

  body <-
"
```{r setup, include=FALSE}
# set default chunk options
knitr::opts_chunk$set(
  # don't show code
  echo = FALSE,
  # don't show warnings
  warning = FALSE,
  # don't show messages
  message = FALSE,
  # use the ragg package to render figures
  dev = 'ragg_png',
  # set default dpi for hi-res screens
  dpi = 144
)

# figures will use colors from document theme
thematic::thematic_rmd(
  font = 'Lato',
  fg = #000000,
  bg = #ffffff,
  accent = #058488,
  sequential = thematic::sequential_gradient(fg_weight = 0, bg_weight = 1, fg_low = FALSE),
  qualitative = c('#058488', '#052049', '#6EA400', '#007CBE', '#F26D04', '#EB093C')
)

# set default ggplot theme
ggplot2::theme_set(
  ggplot2::theme_minimal(base_family = 'Lato') +
    ggplot2::theme(
      # disable in between gridlines
      panel.grid.minor = ggplot2::element_blank(),
      # set color of gridlines
      panel.grid.major = ggplot2::element_line(color = 'grey90')
    )
)
```


`r bhhidistill::add_version()`
"

  writeLines(yaml, con)
  writeLines(body, con)
}
