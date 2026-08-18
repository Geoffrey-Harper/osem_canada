
prepare_canada_dictionary <- function() {
  library(tidyverse)
  library(readr)
  library(magrittr)
  # Comprehensive eurostat dictionary
  library(dplyr)
  library(tibble)
  library(osem)

  # Define the function to add a dictionary entry
  add_dict_entry <- function(dict, model_varname, full_name, database, variable_code, dataset_id, var_col, freq, GEO, geo = NA,
                             unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
                             `North American Industry Classification System (NAICS)` = NA, Prices = NA,
                             `North American Product Classification System (NAPCS)` = NA, found = NA,
                             nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
                             `Levels of government` = NA, Trade=NA ,Estimates = NA,
                             `Labour force characteristics` = NA, Gender = NA, `Age group` = NA,
                             Statistics = NA, `Data type` = NA, `Alternative measures` = NA,
                             `Type of fuel` = NA, `Quantity` = NA, `Capital expenditures` = NA,
                             `Supply and disposition` = NA, `Units of measure` = NA
                             ) {
    dict %>%
      add_row(model_varname = model_varname, full_name = full_name, database = database, variable_code = variable_code,
              dataset_id = dataset_id, var_col = var_col, freq = freq, GEO = GEO, geo = geo, unit = unit, s_adj = s_adj,
              `Seasonal adjustment` = `Seasonal adjustment`, Prices = Prices,
              `North American Industry Classification System (NAICS)` = `North American Industry Classification System (NAICS)`,
              `North American Product Classification System (NAPCS)` = `North American Product Classification System (NAPCS)`,
              found = found,
              nace_r2 = nace_r2, ipcc_sector = ipcc_sector, cpa2_1 = cpa2_1, siec = siec, sector = sector,
              `Levels of government`=`Levels of government`, Trade = Trade, Estimates=Estimates,
              `Labour force characteristics` =`Labour force characteristics`, Gender = Gender , `Age group` =`Age group`,
              Statistics = Statistics, `Data type` = `Data type`,`Alternative measures` = `Alternative measures`,
              `Type of fuel` = `Type of fuel`, `Quantity` = `Quantity`, `Capital expenditures` = `Capital expenditures`,
              `Supply and disposition` = `Supply and disposition`, `Units of measure` = `Units of measure`) %>%
      return()
  }

  # High level accounting frame works Supply = Demand
  dictionary <- tibble(
    model_varname = "Supply", full_name = "Total Supply", database = NA, variable_code = NA,
    dataset_id = NA, var_col = NA, freq = NA, GEO = NA, geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Trade = NA, Estimates=NA,
    `Labour force characteristics` = NA, Gender = NA , `Age group` = NA,
    Statistics = NA, `Data type` = NA,`Alternative measures` = NA,
    `Type of fuel` = NA, `Quantity` = NA, `Capital expenditures` = NA,
    `Supply and disposition` = NA, `Units of measure` = NA
  )

  # Add another entry to the dictionary
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Demand", full_name = "Total Demand",
    database = NA, variable_code = NA, dataset_id = NA, var_col = NA, freq = NA, GEO = NA, geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )
  #####

  # Supply side macro-level
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "GDPOutput", full_name = "GDP Output Approach",
    database = NA, variable_code = NA, dataset_id = NA, var_col = NA, freq = NA, GEO = NA, geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  # Demand side macro-level
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "GDPExpenditure", full_name = "GDP Expenditure Approach",
    database = NA, variable_code = NA, dataset_id = NA, var_col = NA, freq = NA, GEO = NA, geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  ### variables for modeling

  # Start of VA in chain linked 2017

  # where to find industry classifcations
  #https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610043403&pickMembers%5B0%5D=2.1&pickMembers%5B1%5D=3.1&cubeTimeFrame.startYear=2015&cubeTimeFrame.endYear=2025&referencePeriods=20150101%2C20250101

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAEnergy", full_name = "Energy Sector GDP in basic prices  [T016]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Energy sector", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )
  #Gross domestic product at market prices minus taxes less subsidies on products. Gross domestic product at basic prices is also equal to the traditional value at factor cost plus taxes less subsidies on the factors of production (labour and capital).
  #this is how to get VA


  #VA industry is broken down into its accounting identity according to statcat
  # dictionary <- dictionary %>% add_dict_entry(
  #   model_varname = "VAIndustry", full_name = "Industrial production GDP in basic prices",
  #   database = NA, variable_code = NA, dataset_id = NA, var_col = NA, freq = NA, GEO = NA, geo = NA,
  #   unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
  #   `North American Industry Classification System (NAICS)` = NA, Prices = NA,
  #   `North American Product Classification System (NAPCS)` = NA, found = NA,
  #   nace_r2 = NA
  # )

  #includes Mining, quarrying, and oil and gas extraction, Utilites, Manufactuing, wastemangement
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAIndustry", full_name = "Industrial production GDP in basic prices  [T010]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Industrial production", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAMiningAndOil", full_name = "Mining Oil and natural gas GDP in basic prices [21]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Mining, quarrying, and oil and gas extraction", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAUtilities", full_name = "Utilities GDP in basic prices [22]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Utilities", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAManufacturing", full_name = "Manufacturing GDP in basic prices [31-33]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Manufacturing", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAWasteManagement", full_name = "Waste management serivces GDP in basic prices [562]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Waste management and remediation services", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  ## End of VAIndsutry break down

  ##start of VAGov break down
  #includes public admin, health care, education
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAGov", full_name = "Public Sector GDP in basic prices [T018]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Public sector", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  ## End of VAGov break down

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAConstruction", full_name = "Construction GDP in basic prices [23]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Construction", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )


  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAAgriculture", full_name = "Agrictulture GDP in basic prices [11]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Agriculture, forestry, fishing and hunting", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VARetail", full_name = "Retail GDP in basic prices [44-45]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Retail trade", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAInformation", full_name = "Information and cultural industries GDP in basic prices [51]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Information and cultural industries", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAFinance", full_name = "Finance and Insurance GDP in basic prices [52]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Finance and insurance", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VARealEstate", full_name = "Real estate and rental and leasing GDP in basic prices  [53]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Real estate and rental and leasing", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VAWholesaleTrade", full_name = "Wholesale Trade GDP in basic prices [41]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Wholesale trade", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  ##Start of transportation and warehousing breakdown
  #includes
  #This sector comprises establishments primarily engaged in transporting passengers and goods, warehousing and storing goods,
  #and providing services to these establishments. The modes of transportation are road (trucking, transit and ground passenger),
  #rail, water, air and pipeline. These are further subdivided according to the way in which businesses in each mode organize their establishments.
  #National post office and courier establishments, which also transport goods, are included in this sector.
  #Warehousing and storage establishments are subdivided according to the type of service and facility that is operated.

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "VATransportationandWarehousing", full_name = "Transportation and Warehousing GDP in basic prices  [48-49]",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0449-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Transportation and warehousing", Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA
  )

  ## end of VA

  #taxes less of subisdies (nominal) Quarterly
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "TaxesLessSubsidies", full_name = "Taxes on production, products and imports (net of subsidies)",
    database = "statcan", variable_code = NA, dataset_id = "36-10-0118-01", var_col = "na_item",
    freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, `Levels of government`= "Federal general government",
    Estimates="Taxes on production, products and imports (net of subsidies)"
  )

  ## Start of Consumption chain-linked (2017)
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "ConsGov", full_name = "General governments final consumption expenditure",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0127-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = "Federal general government", Estimates="Final consumption expenditure"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "ConsHH", full_name = "Household final consumption expenditure [C]",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0107-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Household final consumption expenditure"
  )

  ## Captial formation chain-linked (2017)

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapForm", full_name = "Total gross fixed capital Formation",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Total gross fixed capital formation [1]"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormBusiness", full_name = "Total business gross fixed capital formation",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Total business gross fixed capital formation"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormGov", full_name = "Total government gross fixed capital formation",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Total general governments gross fixed capital formation"
  )

  #break down of CapFormGov

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormGovConstruction", full_name = "General governments gross fixed capital formation: construction",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="General governments gross fixed capital formation: construction"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormGovResidentialStructures", full_name = "General governments gross fixed capital formation: residential structures",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="General governments gross fixed capital formation: residential structures"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormGovNonResidentialStructures", full_name = "General governments gross fixed capital formation: non-residential structures",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="General governments gross fixed capital formation: non-residential structures"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormGovMachineryandEquipment", full_name = "General governments gross fixed capital formation: industrial machinery and equipment",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="General governments gross fixed capital formation: industrial machinery and equipment"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormGovIntellectualProperty", full_name = "General governments gross fixed capital formation: intellectual property products",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="General governments gross fixed capital formation: intellectual property products"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormGovEngineeringStructures", full_name = "General governments gross fixed capital formation: engineering structures",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="General governments gross fixed capital formation: engineering structures"
  )

  #break down of Business cap form

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormBusinessConstruction", full_name = "Business gross fixed capital formation: construction",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Business gross fixed capital formation: construction"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormBusinessResidentialStructures", full_name = "Business gross fixed capital formation: residential structures",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Business gross fixed capital formation: residential structures"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormBusinessNonResidentialStructures", full_name = "Business gross fixed capital formation: non-residential structures",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Business gross fixed capital formation: non-residential structures"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormBusinessMachineryandEquipment", full_name = "Business gross fixed capital formation: industrial machinery and equipment",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Business gross fixed capital formation: industrial machinery and equipment"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormBusinessIntellectualProperty", full_name = "Business gross fixed capital formation: intellectual property products",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Business gross fixed capital formation: intellectual property products"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapFormEngineeringStructures", full_name = "Business gross fixed capital formation: engineering structures",
    database = "statcan", variable_code = NA,
    dataset_id = "36-10-0108-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates="Business gross fixed capital formation: engineering structures"
  )

  ## end of captial formation

  ##Imports and exports chain-linked (2017)

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Imports", full_name = "Imports of goods and services",
    database = "statcan", variable_code = NA,
    dataset_id = "12-10-0161-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Trade = "Imports",Estimates="Total goods and services"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "ImportsOil", full_name = "Imports of crude oil and bitumen [C121]",
    database = "statcan", variable_code = NA,
    dataset_id = "12-10-0161-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Trade = "Imports",Estimates="Crude oil and crude bitumen"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Exports", full_name = "Exports of goods and services",
    database = "statcan", variable_code = NA,
    dataset_id = "12-10-0161-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Trade = "Exports",Estimates="Total goods and services"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "ExportsOil", full_name = "Exports of crude oil and bitumen [C121]",
    database = "statcan", variable_code = NA,
    dataset_id = "12-10-0161-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA, Prices = "Chained (2017) dollars",
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Trade = "Exports",Estimates="Crude oil and crude bitumen"
  )


  #Oil production variables

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CapexOil", full_name = "Capital Expenditure for oil and gas extraction in dollars [211]",
    database = "statcan", variable_code = NA,
    dataset_id = "25-10-0054-01", var_col = "na_item", freq = "Q", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = "Oil and gas extraction",
    `Capital expenditures` = "All capital expenditures, seasonally adjusted"
  )

  #As from statscan total disposition = DispositionCanadainRefineries + DispositionCrudeOilExports + DispositionCrudeOilInventoryChange

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "DispositionCanadainRefineries", full_name = "Disposition of crude oil to Canadian refineres in barrels",
    database = "statcan", variable_code = NA,
    dataset_id = "25-10-0063-01", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA,
    `Supply and disposition` = "Input to Canadian refineries", `Units of measure` = "Barrels",
    Quantity = "Y"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "DispositionCrudeOilExports", full_name = "Disposition of crude oil total exports in barrels",
    database = "statcan", variable_code = NA,
    dataset_id = "25-10-0063-01", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA,
    `Supply and disposition` = "Input to Canadian refineries", `Units of measure` = "Barrels",
    Quantity = "Y"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "DispositionCrudeOilInventoryChange", full_name = "Disposition of crude oil inventory changes in barrels",
    database = "statcan", variable_code = NA,
    dataset_id = "25-10-0063-01", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA,
    `Supply and disposition` = "Input to Canadian refineries", `Units of measure` = "Barrels",
    Quantity = "Y"
  )

  #total supply = Crude oil production +  Equivalent products production + oil Imports

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CrudeOilProduction", full_name = "Total production of crude oil in barrels",
    database = "statcan", variable_code = NA,
    dataset_id = "25-10-0063-01", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA,
    `Supply and disposition` = "Crude oil production", `Units of measure` = "Barrels",
    Quantity = "Y"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CrudeOilImports", full_name = "Total imports of crude oil in barrels",
    database = "statcan", variable_code = NA,
    dataset_id = "25-10-0063-01", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA,
    `Supply and disposition` = "Imports", `Units of measure` = "Barrels",
    Quantity = "Y"
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "EquivalentProductsProduction", full_name = "Equivalent products in barrels",
    database = "statcan", variable_code = NA,
    dataset_id = "25-10-0063-01", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = "Seasonally adjusted at annual rates",
    `North American Industry Classification System (NAICS)` = NA,
    `Supply and disposition` = "Equivalent products production", `Units of measure` = "Barrels",
    Quantity = "Y"
  )

  #inflation
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Inflation", full_name = "Measure of core inflation based on a factor model, CPI-common (year-over-year percent change)",
    database = "statcan", variable_code = NA,
    dataset_id = "18-10-0256-01", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `Alternative measures` = "Measure of core inflation based on a factor model, CPI-common (year-over-year percent change)",
    Quantity = "N"
  )

  #unemployment
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Unemployment", full_name = "monthly unemployment %",
    database = "statcan", variable_code = NA,
    dataset_id = "14-10-0287-03", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `Labour force characteristics` = "Unemployment rate", Gender = "Total - Gender", `Age group` = "15 years and over",
    Statistics = "Estimate", `Data type` = "Seasonally adjusted", Quantity = "N"
  )

  #Gas Price
  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "GasPrice", full_name = "Monthly gas price: regular unleaded gasoline at self service filling stations",
    database = "statcan", variable_code = NA,
    dataset_id = "18-10-0001-01", var_col = "na_item", freq = "m", GEO = "Canada", geo = NA,
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA, `Type of fuel` = "Regular unleaded gasoline at self service filling stations",
    Quantity = "N"

  )

  ## Emissions

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "EmiCO2OilGas", full_name = "Oil and Natural Gas IPCC 2006",
    database = "edgar", variable_code = NA,
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022.zip",
    var_col = NA, freq = "m", GEO = NA, geo = "CA",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = "1.B.2", cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Trade = NA,Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "EmiCO2Mining", full_name = "Soild Fuels (mining coal) IPCC 2006",
    database = "edgar", variable_code = NA,
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022.zip",
    var_col = NA, freq = "m", GEO = NA, geo = "CA",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = "1.B.1", cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Trade = NA,Estimates=NA
  )

  ## Local variables

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "OilPrice", full_name = "Brent Crude Oil (liquid gold) in USD", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )


  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "ETSPrice", full_name = "Effective Exchange Rates", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "OilNetBack", full_name = "OilNetBack", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "NetBackGas", full_name = "NetBackGas", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CrackSpread", full_name = "CrackSpread", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Koil", full_name = "Koil", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "PIPEoil", full_name = "PIPEoil", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "REFoil", full_name = "REFoil", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Qoil", full_name = "Qoil", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Kgas", full_name = "Kgas", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "Qgas", full_name = "Qgas", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "TransGas", full_name = "TransGas", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )


  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "CUref", full_name = "CUref", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = "m", GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA, Quantity = "N",
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "StorageGas", full_name = "StorageGas", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "OilIntensity", full_name = "OilIntensity", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "GasIntensity", full_name = "GasIntensity", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )

  dictionary <- dictionary %>% add_dict_entry(
    model_varname = "REFIntensity", full_name = "REFIntensity", database = "local", variable_code = NA,
    dataset_id = NA, var_col = "na_item", freq = NA, GEO = NA, geo = "Can",
    unit = NA, s_adj = NA, `Seasonal adjustment` = NA,
    `North American Industry Classification System (NAICS)` = NA, Prices = NA,
    `North American Product Classification System (NAPCS)` = NA, found = NA,
    nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
    `Levels of government` = NA, Estimates=NA
  )



  # Save the resulting dictionary for use
  dict_statcan <- dictionary

  return(dict_statcan)
}
#usethis::use_data(dict_statcan, overwrite = TRUE)

# spec <- dplyr::tibble(
#   type = c(
#     "d"
#   ),
#   dependent = c(
#     "Demand"
#   ),
#   independent = c(
#     "Unemployment"
#
#   )
# )
#
# dictionary = prepare_canada_dictionary()

# actual_cols = colnames(dictionary)
# # basic functionality
# module_order <- check_config_table(spec)
# to_obtain <- determine_variables(specification = module_order,
#                                  dictionary = dictionary)
#



#data <- osem:::load_or_download_variables(specification = spec,dictionary = dictionary)



