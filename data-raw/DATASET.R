## code to prepare `DATASET` dataset goes here

# info: https://www.gov.scot/publications/about-supply-use-input-output-tables/pages/introduction/

library(here)
library(tidyverse)
library(readxl)
library(tidyxl)
library(jsonlite)
library(httr)
library(OECD)
library(readsdmx)

# Tables are downloaded from the OECD database through their web API


# https://www.oecd.org/sdd/na/supply-and-use-tables-database.htm

ref_yr <- 2019

legend_oecd <- map(
  c("products", "sectors", "order", "final_demand", "emissions"),
  \(x){
    out <-   read_excel(
      here("data-raw", "legend_oecd.xlsx"),
      sheet = x
    )
  }
) |>
  set_names(c("products", "sectors", "order", "final_demand", "emissions"))

# Codes of final demand variables

final_demand_codes <- c(
  legend_oecd$final_demand[legend_oecd$final_demand$hierarchy == 3, ]$code_oecd,
  "P6"
)

### INTERMEDIATE TABLES (base prices)

tables_codes <- c(
  supply = "DF_SUPPLY_T1500,2.0", # supply table
  use_pa = "DF_USEPP_T1600", #use at purchasers' prices
  use_t_pa = "DF_USEPP_T1602", #use import at purchasers' prices
  use_pb = "DF_USEBP_T1610", # use at base prices
  use_t_pb = "DF_USEBP_T1612,2.0", # use import at base prices
  margins = "DF_VALUATION_T1620,", # trade and transport margins
  taxes_less_subs = "DF_VALUATION_T1630," # taxes less subsidies on products
)

tables_raw <- map(
  tables_codes,
  \(x){

    read_sdmx(
      paste0(
        "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NASU@",
        x,
        "/A.ITA.......?startPeriod=",
        ref_yr,
       "&endPeriod=",
        ref_yr,
        "&dimensionAtObservation=AllDimensions"
      )
    )
  }
)

names(tables_raw) <- names(tables_codes)

intermediate_tables <- map(
  tables_raw,
  \(x){

    out <- x |>
      filter(
        PRODUCT %in% legend_oecd$products$code_oecd,
        ACTIVITY %in% legend_oecd$sectors$code_oecd,
        PRICE_BASE == "V") |> #current prices
      mutate(ObsValue = as.double(ObsValue)) |>
      select(PRODUCT, ACTIVITY, ObsValue) |>
      pivot_wider(names_from = ACTIVITY, values_from = ObsValue)

    prod_names <- out$PRODUCT

    out <- out |>
      select(!PRODUCT) |>
      as.matrix()

    rownames(out) <- prod_names

    colnames(out) = legend_oecd$sectors$code_it[match(colnames(out), legend_oecd$sectors$code_oecd)]

    rownames(out) = legend_oecd$products$code_it[match(rownames(out), legend_oecd$products$code_oecd)]

    # Summing up rows and columns with the same name

    out <- rowsum(out, row.names(out))

    out <- t(rowsum(t(out), group = colnames(out), na.rm = T))

    # Reordering the matrix

    out <- out[legend_oecd$order$products, legend_oecd$order$sectors]

    out


  }
)

### FINAL DEMAND TABLES

final_tables_raw <- tables_raw[names(tables_raw) %in% c("supply", "margins", "taxes_less_subsidies") == FALSE]


final_tables <- map(
  final_tables_raw,
  \(x){

    out <- x |>
      filter(
        TRANSACTION %in% final_demand_codes,
        PRODUCT %in% legend_oecd$products$code_oecd,
        PRICE_BASE == "V"
      ) |>
      mutate(ObsValue = as.double(ObsValue)) |>
      select(PRODUCT, TRANSACTION, ObsValue) |>
      pivot_wider(names_from = TRANSACTION, values_from = ObsValue)

    prod_names <- out$PRODUCT

    out <- out |>
      select(!PRODUCT) |>
      as.matrix()

    rownames(out) <- prod_names

    #colnames(out) = c("P3S15", "P53", "P6", "P51G", "P52", "P3S13", "P3S14DC")

    rownames(out) = legend_oecd$products$code_it[match(rownames(out), legend_oecd$products$code_oecd)]


      # Summing up rows and columns with the same name

    out <- rowsum(out, row.names(out))

    #out <- t(rowsum(t(out), group = colnames(out), na.rm = T))



    # Reordering the matrix

    out <- out[legend_oecd$order$products, c("P3S14DC",	"P3S15",	"P3S13",	"P51G",	"P53",	"P52",	"P6")]

    out


  }
)

### VALUE ADDED

value_added_raw <- read_sdmx(
  paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NASU@DF_USEVA_T1600,/A.ITA.P51C+B2A3N+B3G+B2A3G+D29X39+D11+D1+P1+B1G+P2.S96+S95+S94+R93+R90T92+Q87_88+Q86+N80T82+N79+N78+N77+M74_75+M73+M72+M71+M69_70+L68B+L68A+K66+K65+K64+J62_63+J61+J59_60+J58+H53+H52+H51+H50+H49+G47+G46+G45+E37T39+E36+C33+C31_32+C30+C29+C28+C27+C26+C25+C24+C23+C22+C21+C20+C19+C17+C18+C16+C13T15+C10T12+A03+A02+A01+B+D+F+I+O+P+T+U...O+B.V.",
    "?startPeriod=",
    ref_yr,
    "&endPeriod=",
    ref_yr,
    "&dimensionAtObservation=AllDimensions"
  )
)


va_legend <- read_sdmx(
  "https://sdmx.oecd.org/public/rest/dataflow/OECD.SDD.NAD/DSD_NASU@DF_USEVA_T1600/?references=all"
) |>
  select(TRANSACTION = id_description, en_description)


va_table <- value_added_raw |>
  left_join(va_legend) |>
  filter(ACTIVITY %in% legend_oecd$sectors$code_oecd) |>
  mutate(ObsValue = as.double(ObsValue)) |>
  select(TRANSACTION, ACTIVITY, ObsValue) |>
  pivot_wider(names_from = ACTIVITY, values_from = ObsValue)

trans_codes <- va_table$TRANSACTION

va_table <- va_table |>
  select(!TRANSACTION) |>
  as.matrix()

rownames(va_table) <- trans_codes

colnames(va_table) = legend_oecd$sectors$code_it[match(colnames(va_table), legend_oecd$sectors$code_oecd)]

#rownames(va_table) = corr_table_codes$code_it[match(rownames(va_table), corr_table_codes$code_oecd)]

# Summing up columns with the same name

va_table <- t(rowsum(t(va_table), group = colnames(va_table), na.rm = T))

# Reordering the matrix

va_table <- va_table[c("P2", "B1G", "P1"), legend_oecd$order$sectors]

# LABOUR TABLES

employment_raw <- read_sdmx(
  paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE7,1.0/A.ITA...EMP....H+JB+PS...?startPeriod=",
  ref_yr,
  "&endPeriod=",
  ref_yr,
  "&dimensionAtObservation=AllDimensions")
)

employment_tidy <- employment_raw |>
  left_join(legend_oecd$sectors, by = c("ACTIVITY" = "code_oecd")) |>
  filter(ACTIVITY %in% legend_oecd$sectors$code_oecd) |>
  mutate(ObsValue = as.double(ObsValue)) |>
  select(ACTIVITY, code_it, UNIT_MEASURE, UNIT_MULT, ObsValue) |>
  mutate(value = ObsValue * 10^as.numeric(UNIT_MULT), # values in units
         .keep = "unused")

employment_l <- split(employment_tidy, employment_tidy$UNIT_MEASURE)

employment <- map(
  employment_l,
  \(x) {
    out <- pull(x, value, code_it)

    out[legend_oecd$order$sectors]

  }
  )

### Environmental accounts

# Downloading emissions by economic sector

# emissions_raw <- read_sdmx(paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD.SEEA,DSD_AEA@DF_AEA,1.0/ITA.A..T_CO2E......?startPeriod=",
#                                    ref_yr,
#                                    "&endPeriod=",
#                                    ref_yr,
#                                    "&dimensionAtObservation=AllDimensions")
# )
#
# emissions_tidy <- emissions_raw |>
#   filter(METHODOLOGY == "EMISSIONS_SEEA",
#          ACTIVITY %in% legend_oecd$sectors$code_oecd) |> # residency principle (the onlt available at industry level)
#   left_join(legend_oecd$sectors, by = c("ACTIVITY" = "code_oecd")) |>
#   mutate(value = as.double(ObsValue)) |>
#   select(ACTIVITY, code_it, UNIT_MEASURE, POLLUTANT, value)
#
# emissions_l <- split(emissions_tidy, emissions_tidy$POLLUTANT)
#
# emissions <- map(
#   emissions_l,
#   \(x) {
#
#     out <- tibble(
#       code_it = unique(legend_oecd$order$sectors)
#     )
#
#
#     out <- out |>
#       left_join(x) |>
#       mutate(value = if_else(
#         is.na(value),
#         0,
#         value
#       )) |>
#       group_by(code_it) |>
#       summarise(value = sum(value)) |>
#       ungroup()
#
#     out <- pull(out, value, code_it)
#
#     out[legend_oecd$order$sectors]
#
#   }
# ) #emissions by sector in t of CO2-eq


### Emissions ISTAT

# https://ondata.github.io/guida-api-istat/
# https://developers.italia.it/it/api/istat-sdmx-rest.html
# https://duet.to/sites/datalab/2018/06/16/2018-06-16-statistica-pubblica-e-r-parte-2/

dataset_istat <- rsdmx::readSDMX(providerId = "ISTAT", resource = "dataflow") |>
  as_tibble()

emissions_raw <- rsdmx::readSDMX(
  providerId = "ISTAT",
  resource = "data",
  flowRef = "97_187",
  start = ref_yr,
  end = ref_yr,
  dsd = TRUE
)

emissions_dfr <- as.data.frame(emissions_raw)

emissions_tidy <- emissions_dfr |>
  filter(EDI == max(EDI)) |>
  mutate(code_oecd = case_when(
    ATECO_2007 == "J59T60" ~ "J59_60",
    ATECO_2007 == "L" ~ "L68A",
    TRUE ~ ATECO_2007
  )) |>
  left_join(legend_oecd$sectors) |>
  filter(code_it != "<NA>") |>
  group_by(code_it, INQUINANTI_TA, TIP_AGGR) |>
  summarise(value = sum(obsValue)) |>
  ungroup()

emissions_tidy <- emissions_tidy |>
  filter(TIP_AGGR %in% c("AE_T", "AEHM_K", "AE_T_CO2EQ")) |>
  mutate(value = if_else(
    TIP_AGGR == "AEHM_K",
    value / 1000, # converting all data into tons
    value
  ),
  type = case_when(
    TIP_AGGR == "AE_T" ~ "Air pollutants",
    TIP_AGGR == "AEHM_K" ~ "Heavy metals",
    TRUE ~ "GHGs"
  ),
  .keep = "unused") |>
  rename(
    pollutant = INQUINANTI_TA
  )

emissions_l <- split(emissions_tidy, emissions_tidy$type)

emissions <- map(
  emissions_l,
  \(x) {

    out <- split(x, x[["pollutant"]])

    map(
      out,
      \(y) {

        vct <- pull(y, value, name = code_it)

        vct[legend_oecd$order$sectors]

      }
    )


  }
)

tables <- list(
  supply = intermediate_tables$supply, # supply ai prezzi di base
  use_pa = intermediate_tables$use_pa, # use ai prezzi di acquisto
  use_t_pa = intermediate_tables$use_t_pa, # use-import ai prezzi di acquisto
  use_pb = intermediate_tables$use_pb, # use ai prezzi di base
  use_t_pb = intermediate_tables$use_t_pb, # use import ai prezzi di base
  margins = intermediate_tables$margins, # matrice dei margini
  taxes = intermediate_tables$taxes_less_subs, # tasse meno sussidi
  final_use_pa = final_tables$use_pa, # final demand PA
  final_use_pb = final_tables$use_pb, # final demand PB
  final_use_t_pa = final_tables$use_t_pa, # Final use import pa
  final_use_t_pb  = final_tables$use_t_pb, # final use import pb
  final_taxes = final_tables$taxes_less_subs, # final taxes
  va = va_table # valore aggiunto
)

usethis::use_data(tables, employment, emissions, legend_oecd, overwrite = TRUE)

#usethis::use_data(emissions, overwrite = T)

#usethis::use_data(simapro_template, simapro_codes, overwrite = TRUE, internal = TRUE)














