#' Preparing exogenous vectors for multipliers analysis
#'
#' This function adds the value corresponding to a new sector to a rearranged exogenous vector (i.e. jobs vector, or emission vectors).
#' At the same time the value of the new sector is substracted from the (user-defined) sector of provenance.
#'
#' @param input_vector A rearranged exogenous vector.
#' @param new_sector_name The name of the new sector
#' @param sector The name of the sector from which the new sector value must be subtracted from
#' @param input_value The value to be added to the vector
exogenous_vectors <- function(
  input_vector,
  new_sector_name = "New Sector",
  sector = "V27",
  input_value
){

  if(length(input_vector) != 62)
    stop("The length of the input vector is not exactly 62")

   if(sum(is.element(names(input_vector), inputoutputR::legend_oecd$sectors$code_it)) != 62)
     stop("Names of the input vector do not correspond to official ISTAT sector names")

  out <- input_vector

  out[new_sector_name] <- input_value

  out[sector] <- out[sector] - out[new_sector_name]

  out

}

