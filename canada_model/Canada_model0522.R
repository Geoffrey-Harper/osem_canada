# install.packages("devtools")
#devtools::install_github("moritzpschwarz/osem@extensions")
library(osem)
library(tidyverse)
library(statcanR)

# Source the dictionary builder
source("Canada_model_dictionary0522.R")

dictionary <- prepare_canada_dictionary()

#  spec <- dplyr::tibble(
#    type = c(
#      "d",
#      "d",
#      "n"
#    ),
#    dependent = c(
#      "ExportsTotal",
#      "ExportsWithoutOil",
#      "ExportsOil"
#    ),
#    independent = c(
#      "ExportsWithoutOil + ExportsOil",
#      "Exports - ExportsOil",
#      "GasPrice"
#
#    )
#  )
#
# spec <- dplyr::tibble(
#   type = c(
#     "n",
#     "n",
#     "n",
#     "n"
#     #"d",
#     #"d",
#     #"d"
#   ),
#   dependent = c(
#     "VAMiningAndOil",
#     "Qoil",
#     "REFoil",
#     "Qgas"
#     #"CO2IntensityOil",
#     #"CO2IntensityGas",
#     #"CO2IntensityREF"
#   ),
#   independent = c(
#     "Qoil+Qgas+REFoil",
#     "OilNetBack + Koil + Uwar",
#     "CUref + CrackSpread",
#     "NetBackGas+Kgas+StorageGas"
#     #"Qoil*OilIntensity",
#     #"Qgas*GasIntensity",
#     #"REFoil*REFIntensity"
#   )
# )



#run base line forecast then modify baselines for experiments
# model_result <- run_model(specification = spec, dictionary = dictionary, save_to_disk = "inputdata/test.csv")

#model_result <- run_model(specification = spec, dictionary = dictionary,
#                          input = c("inputdata/test.csv", "inputdata/oil.csv"), #time,na_item,value mandatory , "inputdata/new_data.csv"
#                          constrain.to.minimum.sample = FALSE,
#                          primary_source= "local")

#model_forecast <- forecast_model(model_result, n.ahead = 5, exog_fill_method = "auto")

# new_exog_data <- model_forecast$exog_data_nowcast %>% mutate(CapexOil = CapexOil * seq(from = 2, to = 10, by = 2))
#
# model_forecast2 <- forecast_model(model_result, n.ahead = 5, exog_predictions = new_exog_data)
#
# browser()

# economic specification
#Supply = demand equation
spec_econ <- tibble(type = c("d", "d"), dependent = c("Supply", "Demand"), independent = c("GDPOutput + ImportsTotal", "GDPExpenditure + ExportsTotal")) %>%
  add_row(type = "d", dependent = "GDPOutput", independent = "VA + TaxesLessSubsidies") %>%
  add_row(type = "d", dependent = "DInventories", independent = "Supply - ConsHH - ConsGov - CapForm - Exports") %>%
  #supply side
  add_row(type = "d", dependent = "VA", independent = "VARealEstate + VAFinance + VAWholesaleTrade + VATransportationandWarehousing + VAInformation + VAAgriculture + VAMiningAndOil + VAUtilities + VAManufacturing + VAWasteManagement + VAConstruction + VAEnergy + VAGov") %>%
  add_row(type = "n", dependent = "VAMiningAndOil", independent = "Qoil + Qgas") %>% #add oil
  add_row(type = "n", dependent = "Qoil", independent = "OilNetBack + Koil + Uwar") %>%
  add_row(type = "n", dependent = "Qgas", independent = "NetBackGas + Kgas + StorageGas" ) %>%
  add_row(type = "n", dependent = "VAUtilities", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "d", dependent = "VAManufacturing", independent = "VAManuwoREF + VAREF") %>%
  add_row(type = "n", dependent = "VAManuwoREF", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAREF", independent = "REFoil") %>%
  add_row(type = "n", dependent = "REFoil", independent = "REFoil_L1 +CUref +CrackSpread") %>%
  add_row(type = "n", dependent = "VAEnergy", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAConstruction", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAFinance", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VARealEstate", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAWholesaleTrade", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VATransportationandWarehousing", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAInformation", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VARetail", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAGov", independent = "ConsHH + ConsGov") %>%
  add_row(type = "d", dependent = "ImportsTotal", independent = "ImportsWithoutOil + ImportsOil") %>%
  add_row(type = "d", dependent = "ImportsWithoutOil", independent = "Imports - ImportsOil") %>%
  #oil supply side
  #add_row(type = "n", dependent = "VA", independent = "Exports") %>%
  #Demand side
  add_row(type = "d", dependent = "GDPExpenditure", independent = "ConsHH + ConsGov + CapForm + Exports - Imports") %>%
  add_row(type = "n", dependent = "CapForm", independent = "CapFormGov + CapFormBusiness") %>%
  add_row(type = "n", dependent = "CapFormGov", independent = "CapFormGovConstruction + CapFormGovResidentialStructures + CapFormGovNonResidentialStructures + CapFormGovMachineryandEquipment + CapFormGovIntellectualProperty + CapFormGovEngineeringStructures") %>%
  add_row(type = "n", dependent = "CapFormBusiness", independent = "CapFormBusinessConstruction + CapFormBusinessResidentialStructures + CapFormBusinessNonResidentialStructures + CapFormBusinessMachineryandEquipment + CapFormBusinessIntellectualProperty + CapFormEngineeringStructures") %>%
  add_row(type = "d", dependent = "ExportsTotal", independent = "ExportsWithoutOil + ExportsOil") %>%
  add_row(type = "d", dependent = "ExportsWithoutOil", independent = "Exports - ExportsOil") %>%


  #simple phillips curve for inflation
  add_row(type = "n", dependent = "Inflation", independent = "Unemployment") %>%

  #Oil exports and disposition identities
  add_row(type = "n", dependent = "ExportsOil", independent = "CapexOil + TotalDispositionOfCrudeOil") %>%
  add_row(type = "d", dependent = "TotalDispositionOfCrudeOil", independent = "DispositionCanadainRefineries + DispositionCrudeOilExports + DispositionCrudeOilInventoryChange")


# add_row(type = "n", dependent = "EmiCO2OilGas", independent = "VAMiningAndOil") %>%
# add_row(type = "n", dependent = "EmiCO2Mining", independent = "VAMiningAndOil") %>%
# add_row(type = "d", dependent = "TotatofOilGasMining", independent = "EmiCO2Mining + EmiCO2OilGas")

# browser()
# #debugonce(osem:::load_or_download_variables)
#
#
model_result <- run_model(specification = spec_econ, dictionary = dictionary,
                          input = c("inputdata/test20260523.csv"),constrain.to.minimum.sample = TRUE)
# model_result_test <- run_model(specification = spec_econ, dictionary = dictionary,
#                           input = c("inputdata/test20260523.csv"),
#                           save_to_disk = "inputdata/test.csv",
#                           constrain.to.minimum.sample = TRUE,
#                           use_logs = "none",
#                           primary_source= "local")

# no log looks better


# model_result <- run_model(
#   specification = spec_econ,
#   dictionary = dictionary,
#   primary_source = "download",
#   trend = TRUE,
#   saturation.tpval = 0.01,
#   gets_selection = TRUE,
#   constrain.to.minimum.sample = FALSE,
#   plot = FALSE
# )
# browser()
# debugonce(osem:::forecast_model)
model_forecast <- forecast_model(model_result, n.ahead = 5, exog_fill_method = "auto")
#
# # inspection: break in forecast period
# model_forecast$exog_data_nowcast %>%
#   select(time, OilNetBack, Koil, Uwar, NetBackGas, Kgas, StorageGas, CUref, CrackSpread) %>%
#   print(n = 20)







# ######################### debug ##########################################
# # 1st bug: corrected load_download_ data function
# # 2nd bug:
# spec_ref <- tibble::tibble(
#   type = "n",
#   dependent = "REFoil",
#   independent = "REFoil_L1 + CUref + CrackSpread"
# )
#
#
# model_ref <- run_model(
#   specification = spec_ref,
#   dictionary = dictionary,
#   input = c("inputdata/test20260523.csv"),
#   constrain.to.minimum.sample = FALSE,
#   primary_source = "local"
# )
#
# # suspecting problem with logged value
# model_ref_ylog <- run_model(
#   specification = spec_ref,
#   dictionary = dictionary,
#   input = c("inputdata/test20260523.csv"),
#   constrain.to.minimum.sample = FALSE,
#   primary_source = "local",
#   use_logs = "y"
# )
#
#
# # forecast issue
# debug(osem:::forecast_identities)
#
# model_forecast <- forecast_model(
#   model_result,
#   n.ahead = 5,
#   exog_fill_method = "auto"
# )
#
# # Koil, uwar, kgas have missing value in prediction period
# exog_fixed <- model_forecast$exog_data_nowcast %>%
#   mutate(
#     Koil = ifelse(is.na(Koil), 36790, Koil),
#     Kgas = ifelse(is.na(Kgas), 28589, Kgas),
#     Uwar = 0
#   )
#
#
# # confirm no missing values
# exog_fixed %>% select(time, Koil, Uwar, Kgas)
#
# #re-run forecast
# model_forecast_fixed <- forecast_model(
#   model_result_test,
#   n.ahead = 5,
#   exog_predictions = exog_fixed
# )
#
# # still have breaks in prediction
# exog_fixed %>%
#   tidyr::pivot_longer(-time, names_to = "var", values_to = "val") %>%
#   filter(is.na(val)) %>%
#   arrange(var, time) %>%
#   print(n = 50)
#
#
# raw_data <- read.csv("inputdata/test20260523.csv") %>%
#   mutate(time = as.Date(time))
#
# raw_q4_2025 <- raw_data %>%
#   filter(time == as.Date("2025-10-01")) %>%
#   select(na_item, values) %>%
#   tidyr::pivot_wider(names_from = na_item, values_from = values)
#
# # fill in NAs with the first period of original data
# vars_na <- c("CapFormBusinessConstruction", "CapFormBusinessIntellectualProperty",
#              "CapFormBusinessMachineryandEquipment", "CapFormBusinessNonResidentialStructures",
#              "CapFormBusinessResidentialStructures", "CapFormEngineeringStructures",
#              "CapFormGovConstruction", "CapFormGovEngineeringStructures",
#              "CapFormGovIntellectualProperty", "CapFormGovMachineryandEquipment",
#              "CapFormGovNonResidentialStructures", "CapFormGovResidentialStructures",
#              "CapexOil", "ConsGov", "ConsHH",
#              "DispositionCanadainRefineries", "DispositionCrudeOilExports",
#              "DispositionCrudeOilInventoryChange", "Exports", "Imports", "ImportsOil",
#              "TaxesLessSubsidies", "Unemployment", "VAAgriculture", "VAWasteManagement")
#
# for (var in vars_na) {
#   if (var %in% names(raw_q4_2025) && !is.na(raw_q4_2025[[var]])) {
#     exog_fixed[exog_fixed$time == as.Date("2025-10-01"), var] <- raw_q4_2025[[var]]
#   }
# }
#
# # REFoil_L1 = REFOil at Q3 2025
# refoil_q3 <- model_result_test$full_data %>%
#   filter(na_item == "REFoil", time == as.Date("2025-07-01")) %>%
#   pull(values)
# exog_fixed[exog_fixed$time == as.Date("2025-10-01"), "REFoil_L1"] <- refoil_q3
#
# # check for NAs
# still_na <- exog_fixed %>%
#   filter(time == as.Date("2025-10-01")) %>%
#   tidyr::pivot_longer(-time) %>%
#   filter(is.na(value)) %>%
#   pull(name)
#
# print(still_na)
#
# #  carry forward with the value from the past one period for vars that're still NA
# if (length(still_na) > 0) {
#   exog_fixed <- exog_fixed %>%
#     arrange(time) %>%
#     tidyr::fill(all_of(still_na), .direction = "down")
# }
#
#
# exog_fixed %>%
#   filter(time == as.Date("2025-10-01")) %>%
#   tidyr::pivot_longer(-time) %>%
#   filter(is.na(value))  # sound be empty
#
# # re-run forecast
# model_forecast_fixed <- forecast_model(
#   model_result_test,
#   n.ahead = 5,
#   exog_predictions = exog_fixed
# )

# failed, drop 2025-10-1 observation of koil, uwar and kgas from test20260522
# re-run forecast
# this is the final version of estimation and forecast results for the conference in May

model_result_test <- run_model(
  specification = spec_econ,
  dictionary = dictionary,
  input = c("inputdata/test20260523.csv"),  # deleted Q4 2025
  #save_to_disk = "inputdata/test.csv",
  constrain.to.minimum.sample = TRUE,
  use_logs = "none",
  primary_source = "local"
)
plot(model_result_test)

model_forecast_fixed <- forecast_model(
  model_result_test,
  n.ahead = 5,
  exog_fill_method = "auto"
)

plot(model_forecast_fixed)


# with carbon cost
exog_scenario <- model_forecast_fixed$exog_data_nowcast %>%
  mutate(OilNetBack = OilNetBack - 24)

model_forecast_scenario <- forecast_model(
  model_result_test,
  n.ahead = 5,
  exog_predictions = exog_scenario
)
plot(model_forecast_scenario)

#################################### not useful, ignore ########################
# baseline <- model_forecast_fixed$forecast %>%
#   filter(dep_var == "Qoil") %>%
#   tidyr::unnest(central.estimate) %>%
#   select(time, Qoil_baseline = Qoil)
#
# scenario <- model_forecast_scenario$forecast %>%
#   filter(dep_var == "Qoil") %>%
#   tidyr::unnest(central.estimate) %>%
#   select(time, Qoil_scenario = Qoil)
#
# left_join(baseline, scenario, by = "time") %>%
#   mutate(diff = Qoil_scenario - Qoil_baseline)
#
#
# # try to fix
# model_forecast_fixed$exog_data_nowcast %>%
#   tidyr::pivot_longer(-time, names_to = "var", values_to = "val") %>%
#   filter(is.na(val)) %>%
#   arrange(var, time) %>%
#   print(n = 50)
# rlang::last_error()
#
#
# last_known <- model_result_test$full_data %>%
#   filter(!is.na(values)) %>%
#   group_by(na_item) %>%
#   filter(time == max(time)) %>%
#   ungroup() %>%
#   dplyr::select(na_item, values) %>%
#   tibble::deframe()
#
#
# last_known["REFoil_L1"] <- model_result_test$full_data %>%
#   filter(na_item == "REFoil", !is.na(values)) %>%
#   filter(time == max(time)) %>%
#   pull(values)
#
#
# exog_to_fix <- model_forecast_fixed$exog_data_nowcast
#
# for (var in names(exog_to_fix)) {
#   if (var == "time") next
#   na_rows <- which(is.na(exog_to_fix[[var]]))
#   if (length(na_rows) > 0 && var %in% names(last_known)) {
#     exog_to_fix[[var]][na_rows] <- last_known[[var]]
#   }
# }
#
#
# exog_to_fix %>%
#   tidyr::pivot_longer(-time) %>%
#   filter(is.na(value))
# # shoudl be 0
#
#
# model_forecast_final <- forecast_model(
#   model_result_test,
#   n.ahead = 5,
#   exog_predictions = exog_to_fix
# )

# plot(model_forecast_final)

# # drop observations for more variables in 2025-10-1
# model_result_fix2 <- run_model(
#   specification = spec_econ,
#   dictionary = dictionary,
#   input = c("inputdata/test20260527.csv"),  # 已删掉 Q4 2025
#   #save_to_disk = "inputdata/test.csv",
#   constrain.to.minimum.sample = TRUE,
#   use_logs = "none",
#   primary_source = "local"
# )
#
# model_forecast_fix2 <- forecast_model(
#   model_result_fix2,
#   n.ahead = 5,
#   exog_fill_method = "auto"
# )

# no working
# model_forecast2 <- forecast_model(model_result_test,
#                                  n.ahead = 5,
#                                 # plot = FALSE,
#                                 uncertainty_sample = 0,
#                                  exog_fill_method = "auto")

