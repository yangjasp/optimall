context("test-optimall_shiny")

library(optimall)
library(shinytest)
library(globals)
library(webshot)

# Sometimes it may be useful to skip if not interactive
# skip_if_not(interactive())

# Set up app

# phantomJS is required to run this test. This function will install it
# if possible, but it doesn't work on Fedora.
skip_on_os("linux")

if (!dependenciesInstalled()){
  shinytest::installDependencies()
}

# If it fails for some reason, just skip the test.
skip_if_not(webshot::is_phantomjs_installed())

app <- shinytest::ShinyDriver$new(shiny::shinyApp(ui = shiny_ui,
                                                  server = shiny_server),
                                  loadTimeout = 1000000)

test_that("App runs", {

   # accepts data
  #app$uploadFile(data = file.path(system.file("data", package = "optimall"),
                                  #"MatWgt_Sim.rda"))
  app$uploadFile(data = system.file("extdata", "MatWgt_Sim.csv",
                                    package = "optimall"))

  # set inputs
  app$setInputs(strata =  "race", split_at = 0.5, split_var = "mat_weight_est",
                y = "mat_weight_est", strata_to_split = "White",
                type = "local quantile", nsample =  100)
  app$setInputs(strata_to_split = "White")
  app$click("confirm")

  values <- app$getAllValues()

  # expect_equal(nchar(values$output$list)[1], 141). But nchar is 138 on M1mac
  expect_gt(nchar(values$output$list)[1], 137)
  expect_equal(length(values$output$list), 1)
})

# stop app
app$stop()

