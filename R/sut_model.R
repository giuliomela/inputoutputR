sut_model <- function(
    new_sector = TRUE,
    new_sector_name = "New Sector Name",
    sector = "V27",
    transport_distribution_services = c("RD", "R45", "R46", "R47", "R49", "R50", "R51"),
    sector_to_join = "VT",
    product_to_join = "RT",
    destination_sector = "V96",
    destination_product = "R96"
){

  if(!is.element(new_sector, c(FALSE, TRUE)))
    stop("The argiment 'new_sector' can only be either TRUE or FALSE")

  # identifying input data path

  path <- file.choose()

  if(isTRUE(new_sector)){

    tables_input <- prepare_tables_new_sector(
      input_path = path,
      new_sector_name = {{new_sector_name}},
      sector = {{sector}},
      transport_distribution_services = {{transport_distribution_services}},
      sector_to_join = {{sector_to_join}},
      product_to_join = {{product_to_join}},
      destination_sector = {{destination_sector}},
      destination_product = {{destination_product}}
    )

    tables_sim <- tables_input[["tables_sim"]]

    input <- tables_input[["input"]]

  } else if (isFALSE(new_sector)){

    tables_sim <- inputoutputR::tables

    input <- loading_input(
      input_path = path,
      new_sector = FALSE
      )

    tables_sim[["employed"]] <- inputoutputR::employment$PS

    tables_sim[["emissions"]] <- inputoutputR::emissions

  }

  ### Defining working matrices to run the simulation

  sectors <- colnames(tables_sim$use_pb)

  products <- rownames(tables_sim$use_pb)

  # make matrix

  make <- t(tables_sim$supply)

  # B matrices

  B_matrix <- tables_sim$use_pb %*% diag(tables_sim$va["P1", ]^-1)

  colnames(B_matrix) <- colnames(tables_sim$use_pb)

  # T matrix (element by element ratio between use and use_import matrices)

  T_matrix <- tables_sim$use_t_pb / tables_sim$use_pb

  T_matrix[is.na(T_matrix)] <- 0

  T_matrix_final <- tables_sim$final_use_t_pb / tables_sim$final_use_pb

  T_matrix_final[is.na(T_matrix_final)] <- 0

  # Final domestic demand (final demand minus imported items satisfying final demand)

  final_demand_domestic <- input$final_use_pa

  # D matrix (market shares)

  D_matrix <- make %*% diag(colSums(make)^-1)

  colnames(D_matrix) <- colnames(make)

  # L inverse matrix

  I <- diag(rep(1, 63*2))

  R_matrix <- B_matrix * (1 - T_matrix) # matrice data da 1 - coefficienti di importazione. Moltiplicazione elemento per elemento

  R_matrix_final <- 1 - T_matrix_final

  zero_matrix <- diag(rep(0, 63))

  L_matrix <- solve(I - cbind(
    rbind(zero_matrix, D_matrix),
    rbind(R_matrix, zero_matrix)
  )
  )

  rownames(L_matrix) <- c(
    rownames(R_matrix),
    rownames(D_matrix)
  )

  colnames(L_matrix) <- c(
    colnames(D_matrix),
    colnames(R_matrix)
  )

  ### Computing the new output and the other variables of interest

  # computing total taxes (to be added to the output value of the new sector)

  if(isTRUE(new_sector)){

  total_tax <- input$taxes |> sum()

  final_demand_domestic[new_sector_name, "P51G"] <- final_demand_domestic[new_sector_name, "P51G"] + total_tax

  }

  # Total output

  output <- L_matrix %*% rbind(final_demand_domestic * R_matrix_final,
                               matrix(rep(0, ncol(final_demand_domestic)*nrow(final_demand_domestic)), ncol = ncol(final_demand_domestic)))

  # computing value added changes

  va_ratio <- tables_sim$va["B1G", ] / tables_sim$va["P1", ]

  va <- rowSums(output[sectors, ]) * va_ratio

  # computing intermediate imports

  intermediate_imports <- (B_matrix * T_matrix) %*% rowSums(output[products, ])

  # Computing indirect taxes

  taxes_share <- colSums(tables_sim$taxes) / tables_sim$va["P1", ]

  indirect_taxes <- rowSums(output)[sectors] * t(taxes_share)

  # Employment

  employed_over_output <- tables_sim$employed / tables_sim$va["P1", ]

  employment <- rowSums(output)[sectors] * employed_over_output

  # Emissions

  emissions <- purrr::map(
    names(tables_sim$emissions),
    \(x){

      emi <- tables_sim$emissions[[x]]

      purrr::map(
        names(emi),
        \(y) {

          ratio <- emi[[y]] / tables_sim$va["P1", ]

          out <- rowSums(output)[sectors] * ratio

          out

        }
      ) |> purrr::set_names(names(emi))
    }
  ) |> purrr::set_names(names(tables_sim$emissions))

  # Final imports

  final_import <- final_demand_domestic * T_matrix_final

  ## Multipliers

  # Output

  output_multipliers <- L_matrix[sectors, sectors] %*% diag(rep(1, nrow(L_matrix[sectors, sectors])))

  colnames(output_multipliers) <- colnames(L_matrix[sectors, sectors])

  # Employment

  employment_multipliers <- diag(employed_over_output) %*% L_matrix[sectors, sectors]

  rownames(employment_multipliers) <- rownames(L_matrix[sectors, sectors])

  # Value added

  va_multipliers <- diag(va_ratio) %*% L_matrix[sectors, sectors]

  rownames(va_multipliers) <- rownames(L_matrix[sectors, sectors])

  # emissions

  emissions_multipliers <- purrr::map(
    names(tables_sim$emissions),
    \(x){

      emi <- tables_sim$emissions[[x]]

      purrr::map(
        names(emi),
        \(y) {

          ratio <- emi[[y]] / tables_sim$va["P1", ]

          multi <- diag(ratio) %*% L_matrix[sectors, sectors]

          rownames(multi) <- rownames(L_matrix[sectors, sectors])

          multi
        }
      ) |> purrr::set_names(names(emi))
    }
  )|> purrr::set_names(names(tables_sim$emissions))

  ### Recap table



  sum_emissions <- purrr::map(
    names(emissions),
    \(x){

      out <- emissions[[x]]

      purrr::map_vec(
        names(out),
        \(y){

          sum(out[[y]])

        }
      ) |> purrr::set_names(names(out))
    }
  ) |> purrr::set_names(names(emissions))

  recap <- c(
    gdp = sum(va) + sum(indirect_taxes),
    indirect_taxes = sum(indirect_taxes),
    intermediate_imports = sum(intermediate_imports),
    final_imports = sum(final_import),
    output = sum(rowSums(output)[sectors]),
    employed = sum(employment),
    emissions = sum_emissions
  )


  results <- list(
    output = output,
    value_added = va,
    intermediate_imports = intermediate_imports,
    indirect_taxes = indirect_taxes,
    employment_persons = employment,
    emissions = emissions,
    recap = recap,
    multipliers = list(
      output = output_multipliers,
      employment = employment_multipliers,
      va = va_multipliers,
      emissions = emissions_multipliers
    )
  )

  results

}
