# repository for install.packages()
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Make sure lintr picks this config file
options(lintr.linter_file = ".lintr")

# Tell languageserver to use styler for formatting
options(
  languageserver.formatting_style = function(options) {
    # You can tune styler here; indent_by=2 is common in tidyverse style
    styler::tidyverse_style(indent_by = 2)
  }
)

# (optional) speed up lintr a bit and keep messages tidy
options(lintr.cache = TRUE)

options(
  languageserver.formatting_style = function(options) {
    styler::tidyverse_style(
      indent_by = 2,
      start_comments_with_one_space = TRUE
    )
  }
)