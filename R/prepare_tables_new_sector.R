#' Preparing Supply and Use tables including a new sector
#'
#' This function takes the original S&U tables and rearranges them to make room for a new sector/product in order to be able to
#' perform an impact analysis.
#'
#' @inheritParams rearrange_tables
#' @inheritParams preparing_input
#' @returns A list containing rearranged tables (with a new sector/product) and exogenous inputs.
prepare_tables_new_sector <- function(
    input_path,
    new_sector_name = "New Sector Name",
    sector = "V27",
    transport_distribution_services = c("RD", "R45", "R46", "R47", "R49", "R50", "R51"),
    sector_to_join = "VT",
    product_to_join = "RT",
    destination_sector = "V96",
    destination_product = "R96"
){

  # rearranging tables and vectors to make room for the new sector

  tables_rearranged <- purrr::map(
    c("sut", "employment", "emissions"),
    \(x) rearrange_tables(
      sector_to_join = {{sector_to_join}},
      product_to_join = {{product_to_join}},
      destination_sector = {{destination_sector}},
      destination_product = {{destination_product}},
      type = x
    )
  ) |> purrr::set_names(c("sut", "employment", "emissions"))

  # loading input data (exogenous demand variation)

  input <- preparing_input(
    input_path = {{input_path}},
    sector = {{sector}},
    transport_distribution_services = {{transport_distribution_services}},
    new_sector_name = {{new_sector_name}}
  )

  ### Adding the new sector/product to the rearranged tables

  table_sim_names <- setdiff(
    names(tables_rearranged$sut),
    c("margins", "final_taxes", "use_pa", "use_t_pa")
  )

  tables_sim <- purrr::map(
    table_sim_names,
    \(x){

      if (!stringr::str_detect(x, "final_use")){

        out <- cbind(tables_rearranged[["sut"]][[x]], input[[x]])

        colnames(out)[ncol(out)] <- new_sector_name

        new_sector_output <- input$va[["P1"]]

        additional_row <- dplyr::case_when(
          x == "use_pb" ~ c(input[["output_to_sectors"]], 0),
          x == "use_t_pb" ~ rep(0, ncol(out)),
          x == "supply" ~ c(rep(0, ncol(out) - 1), new_sector_output),
          x == "taxes" ~ rep(0, ncol(out))
        )

        if(x != "va"){

          names(additional_row) <- c(rownames(tables_rearranged[["sut"]][[x]]), new_sector_name)

          out <- rbind(out, additional_row)

          rownames(out) <- c(rownames(tables_rearranged[["sut"]][[x]]), new_sector_name)

        }

        if(x != "supply"){

          out[, sector] <- out[, sector] - out[, new_sector_name] # substracting new sector from the one it belogns in the real world

        }

        out

      } else {

        out <- tables_rearranged[["sut"]][[x]]

        if(stringr::str_detect(x, "_t_")){

          additional_row <- input[["final_use_t_pa"]][nrow(input[["final_use_t_pa"]]), ]

        } else {

          additional_row <- input[["final_use_pa"]][nrow(input[["final_use_pa"]]), ]

        }

        out <- rbind(out, additional_row)

        rownames(out)[nrow(out)] <- new_sector_name

        out

      }
    }
  ) |> purrr::set_names(table_sim_names)


  # rearranging employment vector

  employed <- exogenous_vectors(
    input_vector = tables_rearranged$employment,
    new_sector_name = {{new_sector_name}},
    sector = {{sector}},
    input_value = input$employed
  )

  tables_sim[["employed"]] <- employed

  # rearranging emissions vectors

  emissions <- rearrange_tables(
    sector_to_join = {{sector_to_join}},
    product_to_join = {{product_to_join}},
    destination_sector = {{destination_sector}},
    destination_product = {{destination_product}},
    type = "emissions"
  )

  emissions_l <-  purrr::map(
    names(emissions),
    \(x) {

      out_l <- emissions[[x]]

      purrr::map(
        names(out_l),
        \(y) {

          input_value <- input[["emissions"]][[x]][[y]]

          out_v <- exogenous_vectors(
            input_vector = tables_rearranged$employment,
            new_sector_name = {{new_sector_name}},
            sector = {{sector}},
            input_value = input_value
          )

        }
      ) |> purrr::set_names(names(out_l))
    }
  ) |> purrr::set_names(names(emissions))


  tables_sim[["emissions"]] <- emissions_l

  return(
    list(
      tables_sim = tables_sim,
      input = input
    )
  )



}
