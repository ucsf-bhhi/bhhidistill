
# bhhidistill

<!-- badges: start -->
<!-- badges: end -->

A [Distill](https://rstudio.github.io/distill) theme and tools for UCSF BHHI.

You can use bhhidistill to create a site for a project and RMarkdown pages for individual analyses.

The analyses come with a template that has the BHHI color palette and includes version control info for the document. 

## Installation

You can install bhhidistill with:

``` r
remotes::install_github("ucsf-bhhi/bhhidistill")
```

## Creating a project site

Here's how to create a project site:

``` r
library(bhhidistill)
create_bhhi_site("~/work/example-site", "Example Site")
```

## Creating an analysis RMarkdown page:

Here's how to create an RMarkdown analysis:

```r
library(bhhidistill)
create_analysis("Example Analysis")
```
