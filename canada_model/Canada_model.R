# install.packages("devtools")
#devtools::install_github("moritzpschwarz/osem@extensions")
library(osem)
library(tidyverse)

# Source the dictionary builder
#source("Canada_model_dictionary.R")

dictionary <- prepare_canada_dictionary()

# spec <- dplyr::tibble(
#   type = c(
#     "d",
#     "d",
#     "n"
#   ),
#   dependent = c(
#     "ExportsTotal",
#     "ExportsWithoutOil",
#     "ExportsOil"
#   ),
#   independent = c(
#     "ExportsWithoutOil + ExportsOil",
#     "Exports - ExportsOil",
#     "GasPrice"
#
#   )
# )

spec <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n",
    "n",
    "d",
    "d",
    "d"
  ),
  dependent = c(
    "VAMiningAndOil",
    "Qoil",
    "REFoil",
    "Qgas",
    "CO2IntensityOil",
    "CO2IntensityGas",
    "CO2IntensityREF"
  ),
  independent = c(
    "Qoil + Qgas + REFoil",
    "OilNetBack + Koil + PIPEoil",
    "CUref + CrackSpread",
    "NetBackGas + Kgas + TransGas + StorageGas",
    "Qoil*OilIntensity",
    "Qgas*GasIntensity",
    "REFoil*REFIntensity"


  )
)


#run base line forecast then modify baselines for experiments
model_result <- run_model(specification = spec, dictionary = dictionary,input=c("inputdata/test20260523.csv"))



model_forecast <- forecast_model(model_result, n.ahead = 5, exog_fill_method = "auto")
#
new_exog_data <- model_forecast$exog_data_nowcast %>% mutate(OilNetBack = OilNetBack - 24)
#
model_forecast2 <- forecast_model(model_result, n.ahead = 5, exog_predictions = new_exog_data)



# browser()
#
# model_result <- run_model(specification = spec, dictionary = dictionary,
#                           input = c("inputdata/test.csv", "inputdata/new_data.csv"), #time,na_item,value mandatory
#                           constrain.to.minimum.sample = FALSE,
#                           primary_source = "local")
#
#
# new_exog_data <- model_forecast$exog_data_nowcast %>% mutate(CapexOil = CapexOil * seq(from = 2, to = 10, by = 2))
#
# model_forecast2 <- forecast_model(model_result, n.ahead = 5, exog_predictions = new_exog_data)

browser()

# economic specification
#Supply = demand equation
spec_econ <- tibble(type = c("d", "d"), dependent = c("Supply", "Demand"), independent = c("GDPOutput + Imports", "GDPExpenditure + Exports")) %>%
  add_row(type = "d", dependent = "GDPOutput", independent = "VA + TaxesLessSubsidies") %>%
  add_row(type = "d", dependent = "DInventories", independent = "Supply - ConsHH - ConsGov - CapForm - Exports") %>%
  #supply side
  add_row(type = "d", dependent = "VA", independent = "VARealEstate + VAFinance + VAWholesaleTrade + VATransportationandWarehousing + VAInformation + VAAgriculture + VAMiningAndOil + VAUtilities + VAManufacturing + VAWasteManagement + VAConstruction + VAEnergy + VAGov") %>%
  add_row(type = "n", dependent = "VAMiningAndOil", independent = "Qoil + Qgas + REFoil") %>% #add oil
  add_row(type = "n", dependent = "VAUtilities", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAManufacturing", independent = "ConsHH + ConsGov + CapForm ") %>%
  add_row(type = "n", dependent = "VAEnergy", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAConstruction", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAFinance", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VARealEstate", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAWholesaleTrade", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VATransportationandWarehousing", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAInformation", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VARetail", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAGov", independent = "ConsHH + ConsGov") %>%
 # add_row(type = "d", dependent = "ImportsTotal", independent = "ImportsWithoutOil + ImportsOil") %>%
#  add_row(type = "d", dependent = "ImportsWithoutOil", independent = "Imports - ImportsOil") %>%

  #oil module
  add_row(type = "n", dependent = "Qoil", independent = "OilNetBack + Koil") %>%
  add_row(type = "n", dependent = "REFoil", independent = "CUref + CrackSpread") %>%
  add_row(type = "n", dependent = "Qgas", independent = "NetBackGas + Kgas + StorageGas") %>%


  #oil supply side
  #add_row(type = "n", dependent = "VA", independent = "Exports") %>%
  #Demand side
  add_row(type = "d", dependent = "GDPExpenditure", independent = "ConsHH + ConsGov + CapForm + Exports - Imports") %>%
  add_row(type = "n", dependent = "CapForm", independent = "CapFormGov + CapFormBusiness") %>%
  add_row(type = "n", dependent = "CapFormGov", independent = "CapFormGovConstruction + CapFormGovResidentialStructures + CapFormGovNonResidentialStructures + CapFormGovMachineryandEquipment + CapFormGovIntellectualProperty + CapFormGovEngineeringStructures") %>%
  add_row(type = "n", dependent = "CapFormBusiness", independent = "CapFormBusinessConstruction + CapFormBusinessResidentialStructures + CapFormBusinessNonResidentialStructures + CapFormBusinessMachineryandEquipment + CapFormBusinessIntellectualProperty + CapFormEngineeringStructures")
  # add_row(type = "d", dependent = "ExportsTotal", independent = "ExportsWithoutOil + ExportsOil") %>%
  # add_row(type = "d", dependent = "ExportsWithoutOil", independent = "Exports - ExportsOil") %>%


  #simple phillips curve for inflation
  #add_row(type = "n", dependent = "Inflation", independent = "Unemployment") %>%

  #Oil exports and disposition identities
  #add_row(type = "n", dependent = "ExportsOil", independent = "CapexOil + TotalDispositionOfCrudeOil") %>%
  #add_row(type = "d", dependent = "TotalDispositionOfCrudeOil", independent = "DispositionCanadainRefineries + DispositionCrudeOilExports + DispositionCrudeOilInventoryChange")

# add_row(type = "n", dependent = "EmiCO2OilGas", independent = "VAMiningAndOil") %>%
# add_row(type = "n", dependent = "EmiCO2Mining", independent = "VAMiningAndOil") %>%
# add_row(type = "d", dependent = "TotatofOilGasMining", independent = "EmiCO2Mining + EmiCO2OilGas")


browser()
model_result <- run_model(specification = spec_econ, dictionary = dictionary, input=c("inputdata/oil.csv","inputdata/test.csv"))
co2module <- model_result$module_collection %>%
  filter(dependent == "Qoil") %>%
  pull(model) %>%
  pluck(1)
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
model_forecast <- forecast_model(model_result, n.ahead = 5,exog_fill_method = "auto")

new_exog_data <- model_forecast$exog_data_nowcast %>% mutate(OilNetBack = OilNetBack - 24)
#
model_forecast2 <- forecast_model(model_result, n.ahead = 5, exog_predictions = new_exog_data)


