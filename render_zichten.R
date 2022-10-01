library(quarto)

options(gargle_oauth_email = TRUE)
quarto_render(
  input = "zichten.qmd"
  , output_file = "index.html"
)
