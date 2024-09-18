# Faxina de dados

# Quimica - Macrófitas ----------------------------------------------------
sheets <- readxl::excel_sheets("data-raw/dadoscpfl.xlsx")
df <- readxl::read_xlsx("data-raw/dadoscpfl.xlsx",
                        sheet = sheets[1],
                        skip = 1) |>
  dplyr::rename(data = "...1") |>
  janitor::clean_names() |>
  dplyr::mutate(
    amostra = stringr::str_to_lower(amostra),
    amostra = ifelse(amostra == "pisst","piist",amostra),
    cd = as.numeric(ifelse(cd == "<1",0.5,cd)),
    cr = as.numeric(ifelse(cr == "<1",0.5,cr)),
    pb = as.numeric(ifelse(pb == "<1",0.5,pb))
  )
dplyr::glimpse(df)
skimr::skim(df)
readr::write_rds(df,"data/quimica-macrofita")


# Experimento - Solo ------------------------------------------------------
dff <- readxl::read_xlsx("data-raw/dadoscpfl.xlsx",
                         sheet = sheets[3],
                         skip = 1) |>
  janitor::clean_names() |>
  dplyr::mutate(
    macrofita = stringr::str_to_lower(macrofita),
    cr = ifelse(is.na(cr),0,cr),
    cd = ifelse(is.na(cd),0,cd)
  )
glimpse(dff)
skimr::skim(dff)
readr::write_rds(dff,"data/solo-macrofita")

# Crescimento - Braquiária ------------------------------------------------
dfff <- readxl::read_xlsx("data-raw/dadoscpfl.xlsx",
                          sheet = sheets[2]) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dose = ifelse(dose == "20/ t/ha", "20 t/ha",ifelse(dose == "22 t/ha", "20 t/ha",dose))
  )
glimpse(dfff)
skimr::skim(dff)

## Salvar o df em rds
readr::write_rds(dfff,"data/crescimento-braquiaria")
