test_that("rearrange_tables works", {

  out <- rearrange_tables()

  sectors <- legend_oecd$order$sectors

  products <- legend_oecd$order$products

  ### checking matrix dimensions

  # intermediate tables

  purrr::map(
    c("supply", "use_pa", "use_t_pa", "use_pb", "use_t_pb", "margins", "taxes"),
    \(x){

      rows <- nrow(out[[x]])

      cols <- ncol(out[[x]])

      expect_equal(rows + cols, 62*2)


    }
  )

  # final tables

  purrr::map(
    names(out[stringr::str_detect(names(out), "final")]),
    \(x){

      rows <- nrow(out[[x]])

      cols <- ncol(out[[x]])

      expect_equal(rows + cols, 62 + 7)

    }
  )

  # value added table

  expect_equal(
    nrow(out[["va"]]) + ncol(out[["va"]]), 62 + 3
  )


})
