context("test-optimall_shiny")

library(optimall)
library(shinytest)

# Set up app
app <- shinytest::ShinyDriver$new(shiny::shinyApp(ui = shiny_ui,
                                                  server = shiny_server))

test_that("App runs", {

   # accepts data
  app$uploadFile(data = system.file("data","MatWgt_Sim.rda", package = "optimall"))

  # set inputs
  app$setInputs(strata =  "race", split_at = 0.5, split_var = "mat_weight_est",
                y = "mat_weight_est", strata_to_split = "White",
                type = "local quantile", nsample =  100)
  app$setInputs(strata_to_split = "White")
  app$click("confirm")

  output_text <- app$getValue("list")

  expect_equal(length(output_text), 1)
})

# stop app
app$stop()

