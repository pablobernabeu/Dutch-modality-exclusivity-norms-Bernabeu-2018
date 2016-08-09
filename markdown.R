# Pablo Bernabeu (pcbernabeu@gmail.com)

# This script is for the creation of Word and PDF documents via RMarkdown.
# Pandoc and Miktex were previously installed. Where this is a good way to 
# automatically create a readable version of the code with results, the
# purpose is not to create a nice-looking, edited document.

install.packages('rmarkdown')
library(markdown)

# Run code below. If library error, just try running again
rmarkdown::render("norms.R", c("word_document", "pdf_document"))

# rendering takes a few minutes
# at the end, find norms pdf in your working directory

# In the Word document, it is recommended to widen the horizontal margins.