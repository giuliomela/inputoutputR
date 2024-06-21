test_that("prepare_tables_new_sector works", {

  path = "C:\\Users\\mela\\OneDrive - RSE S.p.A\\R\\miei_pacchetti\\inputoutputR\\data-raw\\input.xlsx"

  out <- prepare_tables_new_sector(
    input_path = path
  )

  expect_true(is.list(out))

  expect_true(is.list(out$tables_sim))

  expect_true(is.list(out$input))

  purrr::map(
    out$tables_sim[c("supply", "use_pb", "use_t_pb", "taxes")],
    \(x) {

      expect_equal(nrow(x), ncol(x))

      expect_equal(nrow(x), 63)

    }
  )

  purrr::map(
    out$tables_sim[grepl("final", names(out$tables_sim))],
    \(x) {

      expect_equal(nrow(x), 63)

      expect_equal(ncol(x), 7)

    }
  )

  expect_true(is.vector(out$tables_sim$employed))

  expect_equal(length(out$tables_sim$employed), 63)

  purrr::map(
    out$tables_sim$emissions,
    \(x) {
      purrr::map(
        x,
        \(y) {

          expect_true(is.vector(y))

          expect_equal(length(y), 63)

        }
      )

    }
  )

  purrr::map(
    out$input[grepl("final", names(out$input))],
    \(x){

      expect_equal(nrow(x), 63)

      expect_equal(ncol(x), 7)

    }
  )

  purrr::map(
    out$input[grepl(c("emissions"), names(out$input))],
    \(x) {
      purrr::map(
        x,
        \(y) {

          expect_true(is.vector(y))


        }
      )

    }
    )


}
)
