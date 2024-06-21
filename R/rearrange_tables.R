#' Rearranging national accounting tables
#'
#' This function takes OECD national accounting matrices and rearranges them by adding up sectors/products chosen by the user to other
#' sectors/products chosen by the user. The output is a list of matrices with one dimension less than the original matrices in order to
#' "make room" for a new sector and a new product. In this way it is possible to evaluate the socio-economic impact of the new product/sector
#' on the economy.
#'
#' @param sector_to_join,product_to_join ISTAT codes of the sector and product to be aggregated to other sectors/products. The full list
#'     of products/sectors is available in `inputoutputR::legend_oecd`. Default values are: `VT` and `RT` respectively.
#' @param destination_sector,destination_product ISTAT codes of the sector and products to which `sector_to_join` and `product_to join`
#'     must be aggregated to. Default values are: `V96` and `R96` respectively.
#' @param type A lstring indicating what type of tables must be rearranged. Three values possible: `sut`, `employment` and `emissions`.
#' @return A list of matrices, of the same length of `inputoutputR::tables`.
#' @examples
#' # rearrange_tables()
#'
rearrange_tables <- function(
    sector_to_join = "VT",
    product_to_join = "RT",
    destination_sector = "V96",
    destination_product = "R96",
    type = "sut"
    ){

  if (!is.element(type, c("sut", "employment", "emissions")))
    stop("Please provide a valid value for paramter 'type'")

  if (type == "sut"){

  tables_names <- names(inputoutputR::tables)

  tables_rearranged <- purrr::map(
    tables_names,
    \(x){

      out <- inputoutputR::tables[[x]]

      if (x == "va") {

        out[, destination_sector] <- out[, destination_sector] + out[, sector_to_join]

        out[, c(-ncol(out))]


      } else if (stringr::str_detect(x, "final")){

        out[destination_product, ] <- out[destination_product, ] + out[product_to_join, ]

        out[c(-nrow(out)), ]

      } else {

        out[, destination_sector] <- out[, destination_sector] + out[, sector_to_join]


        out[destination_product, ] <- out[destination_product, ] + out[product_to_join, ]


        out[c(-nrow(out)), c(-ncol(out))]

      }

    }
  )

  names(tables_rearranged) <- tables_names

  tables_rearranged

  } else if (type == "employment") {

    employed <- inputoutputR::employment$PS

    employed[destination_sector] <- employed[destination_sector] + employed[sector_to_join]

    employed <- employed[-length(employed)]

    employed

  } else if (type == "emissions"){

    emissions <- inputoutputR::emissions

    purrr::map(
      emissions,
      \(x) {

        out <- x

        purrr::map(
          out,
          \(y) {
            vec <- y

            vec[destination_sector] <- vec[destination_sector] + vec[sector_to_join]

            vec[-length(vec)]
          }
        )

      }
    )

  }

}
