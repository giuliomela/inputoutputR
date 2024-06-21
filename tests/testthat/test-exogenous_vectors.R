test_that("exogenous_vectors works", {

  input_vector <- 1:62

  input_value <- 100

  names(input_vector) <- legend_oecd$sectors$code_it[1:62]

  out <- exogenous_vectors(
    input_vector = input_vector,
    input_value = input_value
  )

  expect_error(
    exogenous_vectors(
      input_vector = 1:63,
      input_value = input_value
    )
  )

  expect_equal(
    length(out), length(input_vector) + 1
  )

  expect_equal(
    sum(out),
    1953
  )


})
