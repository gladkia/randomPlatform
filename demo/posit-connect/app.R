# randomPlatform demo on Posit Connect Cloud.
#
# Runs the installed package in hosted mode: no browsing of the server's disk,
# every session writes to its own temporary directory, and a banner says the
# app is for example data only. The randomPlatform commit pinned in
# manifest.json decides which version is served, not the branch this file is on.

options(randomPlatform.hosted = TRUE, randomPlatform.language = "pl-PL")
library(randomPlatform)

shiny::shinyApp(
  ui = randomPlatform:::rp_app_ui(),
  server = randomPlatform:::rp_app_server
)
