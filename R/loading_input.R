#' Loading the input file
#'
#' This function loads an excel file containing input data for the SUT model. The excel file must follow a given template.
#' The function returns a list of matrices containing all the necessary inputs to carry out the impact analysis based on the
#' supply and use tables.
#'
#' @param input_path The path to the .xlsx file to be loaded
#' @param new_sector A logical value. If set to `TRUE` the impact assessment is carried out introducing a new sector into the economy.
#'     If it is set to `FALSE` the simulation refer to a change in final demand and considers existing sectors only.
#' @param new_sector_name A string. The name of the new sector/product to be created in order to assess its impacts.
#' @return A list of matrices and vectors with input data to be used to build the SUT model
#'
loading_input <- function(
  input_path,
  new_sector = TRUE,
  new_sector_name = "New Sector"
  ){

  if(!is.element(new_sector, c(FALSE, TRUE)))
    stop("The argiment 'new_sector' can only be either TRUE or FALSE")

  if(isTRUE(new_sector)){

  input_names <- readxl::excel_sheets(
    input_path
  )

  input <- purrr::map(
    input_names[input_names != "legend"],
    \(x) {

      out <- readxl::read_excel(
        input_path,
        sheet = x
      )

      if (!x %in% c("final_use_pa", "final_use_t_pa", "final_export", "employed", "emissions")){

        out_final <- out[["value"]]

        names(out_final) <- out[["code"]]

        out_final

        # out |>
        #   dplyr::pull(
        #     value, code
        #   )

      } else if (x %in% c("final_use_pa", "final_use_t_pa", "final_export")){

        product_names <- out[["code"]]

        #product_names <- dplyr::pull(out, code)

        product_names[length(product_names)] <- new_sector_name

        out[, "code"] <- NULL

        out <- as.matrix(out)

        rownames(out) <- product_names

        out

      } else if (x == "employed"){

        out_final <- out[["value"]]

        names(out_final) <- new_sector_name

        out_final

      } else if (x == "emissions") {

        out <- split(out, out[["type"]])

        out <- purrr::map(
          out,
          \(y) {

            vec <- y[["value"]]

            names(vec) <- y[["pollutant"]]

              #dplyr::pull(y, value, name = pollutant)

            vec

          }
          )


      }

    }
  )

  names(input) <- input_names[input_names != "legend"]

  input

  } else if (isFALSE(new_sector)){

    input <- purrr::map(
      c("final_use_pa", "final_use_t_pa"),
      \(x) {

        out <- readxl::read_excel(
          input_path,
          sheet = x
        )

        product_names <- out[["code"]]

        #product_names <- dplyr::pull(out, code)

        out[, "code"] <- NULL

        out <- as.matrix(out)

        rownames(out) <- product_names

        out

      }

    ) |> purrr::set_names(c("final_use_pa", "final_use_t_pa"))

    input

  }


}
