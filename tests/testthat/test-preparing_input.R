test_that("preparing_input works", {

  path <- "C:\\Users\\mela\\OneDrive - RSE S.p.A\\R\\miei_pacchetti\\inputoutputR\\data-raw\\input.xlsx"

  out <- preparing_input(
    input_path = path
  )

  expect_true(is.list(out))


})
