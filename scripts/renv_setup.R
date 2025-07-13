# 2.1 Initialize renv for R Package Management
# Install renv and initialize
install.packages("renv")
renv::init()

# Install all your packages in one go
install.packages(c(
  "tidyverse", "plotly", "DT", "scales", "bslib", 
  "bsicons", "lubridate", "kableExtra", "knitr", 
  "readxl", "dplyr", "tsibble", "ggplot2", 
  "forecast", "htmltools", "fable", "quarto", "rsconnect"
))

# Create .renvignore file
writeLines(c(
  "scripts/",
  "*.Rmd", 
  "*.rmd",
  "temp/",
  "archive/"
), ".renvignore")

# Then run snapshot again
renv::snapshot()


## TRY MINIMAL VERSION OF RENV ## 
# Create .renvignore first
writeLines(c("scripts/", "data/", "archive/", "temp/"), ".renvignore")

# Use explicit package list instead of discovery
renv::init(bare = TRUE)  # Skip automatic discovery

# Install only what you need
renv::install(c(
  "tidyverse", "plotly", "DT", "scales", "bslib", 
  "bsicons", "lubridate", "kableExtra", "knitr", 
  "readxl", "tsibble", "forecast", "htmltools", "fable"
))

renv::snapshot()


install.packages(c(
  "tidyverse", "plotly", "DT", "scales", "bslib", 
  "bsicons", "lubridate", "kableExtra", "knitr", 
  "readxl", "tsibble", "forecast", "htmltools", "fable"
))

# Part 3: Prepare for Posit Connect Deployment
# Install deployment packages
install.packages(c("rsconnect", "connectapi"))

# Update renv snapshot
renv::snapshot()



3.2 Modify Dashboard for Posit Connect



# Then test the dashboard
quarto::quarto_preview("your-dashboard.qmd")