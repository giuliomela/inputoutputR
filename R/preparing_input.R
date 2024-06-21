#' Preparing input matirces
#'
#' This function loads input data on the new sector/product to be assessed and computes the taxes vector and the
#' use and inport use vectors at base prices.
#'
#' @param sector The ISTAT code of the sector from which the "New" sector must be substracted from to void double counting
#' @param transport_distribution_services Transport and distribution services (those to which distribution and transport margins flow into,
#'     when passing from purchasers' to base prices)
#' @inheritParams loading_input
preparing_input <- function(
  input_path,
  sector = "V27",
  transport_distribution_services = c("RD", "R45", "R46", "R47", "R49", "R50", "R51"),
  new_sector_name = "New Sector"
  ){

  input <- loading_input(
    input_path = input_path,
    new_sector = TRUE,
    new_sector_name = new_sector_name
  )

  # Rearranging tables to make room for the new sector

  tables62 <- rearrange_tables()

  # Extracting tax and margin data of the sector of interest

  use_pa_sector <- tables62[["use_pa"]][, sector]

  tax_sector <- tables62[["taxes"]][, sector]

  margin_sector <- tables62[["margins"]][, sector]

  # Computing tax and margin shares

  tax_share <- ifelse(
    use_pa_sector == 0,
    0,
    tax_sector /
      use_pa_sector[names(tax_sector)]
  )

  margin_share <- ifelse(
    use_pa_sector == 0,
    0,
    margin_sector /
      use_pa_sector[names(margin_sector)]
  )

  tax_input <- input$use_pa[names(tax_share)] * tax_share

  margin_input <- input$use_pa[names(margin_share)] * margin_share

  # Substracting taxes and margins from most products except for those produced by intermediary sectors

  total_margins <- sum(margin_input[setdiff(names(margin_input), transport_distribution_services)]) # total margins minus margins of the transport and intermediary products

  total_margins_trans_int <- total_margins *
    margin_sector[setdiff(transport_distribution_services, "RD")] /
    sum(margin_sector[setdiff(transport_distribution_services, "RD")])

  margin_input["RD"] <- margin_input["RB"] * - 1 # Positive margin of the products of extraction sector equals the negative margin of the energy products

  margin_input[setdiff(transport_distribution_services, "RD")] <- total_margins_trans_int * -1


  input[["use_pb"]] <- input[["use_pa"]] - tax_input - margin_input

  input[["use_t_pb"]] <- input[["use_pb"]] * input[["use_t"]]

  input[["taxes"]] <- tax_input

  input

}
