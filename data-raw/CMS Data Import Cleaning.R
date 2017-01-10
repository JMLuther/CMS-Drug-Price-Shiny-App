library(tidyverse)
library(stringi)
library(readr)
#__________________________________________________________________________________________________
# IMPORT FDA DRUG DATA FILES
# http://www.fda.gov/Drugs/InformationOnDrugs/ucm142438.htm
#__________________________________________________________________________________________________
fdaDrugs_Temp <- readr::read_tsv("FDA_Products_tab.txt")
fdaDrugs_Temp$STARTMARKETINGDATE <- lubridate::ymd(fdaDrugs_Temp$STARTMARKETINGDATE) # a few dates are wrong (Clopidogrel 1900-01-01)
fdaDrugs_Temp$ENDMARKETINGDATE <- parse_date(fdaDrugs_Temp$ENDMARKETINGDATE, "%Y%m%d")

drugPackageInfo <- readr::read_tsv("FDA_Package_tab.txt")
fdaDrugs <- left_join(fdaDrugs_Temp, drugPackageInfo, by=c("PRODUCTID", "PRODUCTNDC")) 
rm(fdaDrugs_Temp, drugPackageInfo)

# Convert Case and replace NA with ""
fdaDrugs$PROPRIETARYNAMESUFFIX[is.na(fdaDrugs$PROPRIETARYNAMESUFFIX)] <- ""
fdaDrugs$PRODUCTTYPENAME <- str_to_title(fdaDrugs$PRODUCTTYPENAME)
fdaDrugs$PROPRIETARYNAME <- str_to_title(fdaDrugs$PROPRIETARYNAME)
fdaDrugs$NONPROPRIETARYNAME <- str_to_title(fdaDrugs$NONPROPRIETARYNAME)
fdaDrugs$DOSAGEFORMNAME <- str_to_title(fdaDrugs$DOSAGEFORMNAME)
fdaDrugs$ROUTENAME <- str_to_title(fdaDrugs$ROUTENAME)
fdaDrugs$SUBSTANCENAME <- str_to_title(fdaDrugs$SUBSTANCENAME)

# MAKE NEW VARIABLES
fdaDrugs$brand_name <- fdaDrugs$PROPRIETARYNAME
fdaDrugs$generic_name <- fdaDrugs$NONPROPRIETARYNAME
fdaDrugs$formulation <-  paste(fdaDrugs$PROPRIETARYNAME, fdaDrugs$PROPRIETARYNAMESUFFIX, sep=" ")

fdaDrugs$PRODUCTTYPENAME <- 
  factor(fdaDrugs$PRODUCTTYPENAME,
         levels=c("Human Prescription Drug", "Human Otc Drug", "Non-Standardized Allergenic", "Plasma Derivative",
                  "Vaccine", "Standardized Allergenic", "Cellular Therapy"))

# SAVE 
saveRDS(fdaDrugs, "fdaDrugs.rds")

#__________________________________________________________________________________________________
# IMPORT PART D CMS DATA 2011-2015
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Information-on-Prescription-Drugs/2015MedicareData.html
#__________________________________________________________________________________________________

drugDataRAW <-  # read the unchanged .xlsx file from the website
  readxl::read_excel("Medicare_Drug_Spending_PartD_All_Drugs_YTD_2015_12_06_2016.xlsx",
                     sheet="Data", skip=2)
saveRDS(drugDataRAW, "partDData_Original.rds")
names(drugDataRAW) <- tolower(names(drugDataRAW))        # make var names all lower case
names(drugDataRAW) <- gsub(",", "", names(drugDataRAW))  # remove ,
names(drugDataRAW) <- gsub(" ", "_", names(drugDataRAW)) # remove space in var name
names(drugDataRAW) <- gsub("_\\(weighted\\)_", "_", names(drugDataRAW))
drugDataRAW$generic_name <- stringr::str_trim(drugDataRAW$generic_name) # remove white space
drugDataRAW$brand_name <- stringr::str_trim(drugDataRAW$brand_name)     # remove white space
drugDataRAW <- drugDataRAW[-which(is.na(drugDataRAW$generic_name)==T), ]# delete empty rows
#varsToDelete <- grep("_lis|annual_change_in", names(drugDataRAW))     # delete lis fields
#drugDataRAW <- drugDataRAW[, -varsToDelete]
drugDataRAW %>%  gather(var, value, -brand_name, -generic_name) %>%     # Convert to long format
  extract(var, c("var", "year"), "([a-z_]*)_(20[0-9]*)") %>% 
  spread(key=var, value=value)  -> drugData

# I've found that sometimes the brand_name is not unique, and can be associated with
# multiple formulations (e.g., "salex" = "salicylic acid" OR "salicylic acid/ceramide cmb#1")
# create formulation- because we need a unique brand+generic name combination:
drugData$formulation <- paste0(drugData$generic_name, " -as- ", drugData$brand_name) 
rm(drugDataRAW, varsToDelete)

# Create ranking and cost change by year
drugData %>% group_by(formulation) %>% 
  arrange(formulation, year) %>% 
  filter(!is.na(total_spending))-> drugData
drugData %>% ungroup %>%  
  group_by(year) %>% arrange(year) %>% 
  mutate(pctile_cost_per_unit = percent_rank(average_cost_per_unit),
         pctile_total_cost = percent_rank(total_spending),
         nDrugs = sum(!is.na(total_spending))) %>% ungroup() -> drugData
drugData %>% group_by(formulation) %>% arrange(year) %>% 
  mutate(annual_change_per_unit = average_cost_per_unit - lag(average_cost_per_unit),
         annual_change_per_unit_pct = annual_change_per_unit/lag(average_cost_per_unit)*100,
         annual_change_total = total_spending - lag(total_spending),
         annual_change_total_pct = annual_change_total/lag(total_spending)*100,
         pctile_differential =  pctile_total_cost - pctile_cost_per_unit) -> drugData
drugData %>% group_by(year) %>% arrange(desc(total_spending)) %>% 
  mutate(rank_total=row_number()) -> drugData
drugData %>% group_by(year) %>% arrange(desc(average_cost_per_unit)) %>% 
  mutate(rank_avg_cost=row_number()) -> drugData

# Create tooltip with html code
drugData$tip <-
  paste0("<h3><strong><span style=\"color: gold\"}>", stri_trans_totitle(drugData$brand_name),"</span></strong><br></h3>",
         "<h4><strong><span style=\"color: gold\"}>(",stri_trans_totitle(drugData$generic_name), ")</span></strong></h4>",
         "<h4><strong><span style=\"color: gold\"}>Medicaid Part D</span></strong></h4>",
         "<hr />", 
         "Total Cost(millions): $", round(drugData$total_spending/1000000, 2),"<br>",
         "Total Cost (%ile): ", round(drugData$pctile_total_cost, 2),  "<br>",
         "Total Cost (Rank): #", drugData$rank_total, " of ", drugData$nDrugs, "<br>",
         "<hr />", 
         "Unit Cost: $", round(drugData$average_cost_per_unit, 2),"<br>",
         "Unit Cost (%ile): ", round(drugData$pctile_cost_per_unit, 2), "<br>",
         "Unit Cost (Rank): #", drugData$rank_avg_cost, " of ", drugData$nDrugs, "<br>",
         "Price change($): $", ifelse(is.na(drugData$annual_change_per_unit), "", ifelse(drugData$annual_change_per_unit>0, "+", "")),
         ifelse(is.na(drugData$annual_change_per_unit)," --- ",round(drugData$annual_change_per_unit, 2)),  "<br>",
         "Price change(%): ",ifelse(is.na(drugData$annual_change_per_unit_pct), "", ifelse(drugData$annual_change_per_unit_pct>0, "+", "")),
         ifelse(is.na(drugData$annual_change_per_unit_pct)," --- ",round(drugData$annual_change_per_unit_pct, 2)), "%<br>")

# final steps
drugData$generic_name <- stringi::stri_trans_totitle(drugData$generic_name)
drugData$brand_name <- stringi::stri_trans_totitle(drugData$brand_name)

#  drugData$pctile_differential <-  drugData$pctile_total_cost - drugData$pctile_cost_per_unit
saveRDS(drugData, "partDData.rds") 
rm(drugData)
#____________________________________________________________________________________________________________________________________________
# Part B Data
#____________________________________________________________________________________________________________________________________________
library(stringr)
library(stringi)
partBDataRAW <-  # read the unchanged .xlsx file from the website
  readxl::read_excel("Medicare_Drug_Spending_PartB_All_Drugs_YTD_2015_12_06_2016.xlsx",
                     sheet="Data", skip=2)
saveRDS(partBDataRAW, "partBData_Original.rds")
names(partBDataRAW) <- tolower(names(partBDataRAW))        # make var names all lower case
names(partBDataRAW) <- gsub(",", "", names(partBDataRAW))  # remove ,
names(partBDataRAW) <- gsub(" ", "_", names(partBDataRAW)) # remove space in var name
partBDataRAW <- 
  partBDataRAW %>%  filter(!is.na(hcpcs_description))

# Recode into brand_name, route, ,
# routes of administration: injection, infusion, parenteral, injectable, for injection, injection into muscle, administered into muscle to ind,
# intra-articular,  injection into urinary bladder,
# oral, inhalation, per i.u., topical, intravitreal, implant (sc), for nasal administration, for injection beneath skin, injection into skin
# parenteral, enteral
# make a DRUG NAME variable to whittle down to the basic name
partBDataRAW$brand_name <- partBDataRAW$hcpcs_description
#partBDataRAW$brand_name <- stri_trans_totitle(partBDataRAW$brand_name)

# remove route indicator
partBDataRAW$brand_name <- gsub("(for injection into muscle|injection into muscle|for intramuscular use|administered into muscle)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for injection into skin|for injection beneath skin|injection beneath skin|injection into skin)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for injection beneath the skin or into muscle)", "", partBDataRAW$brand_name, ignore.case = T)

partBDataRAW$brand_name <- gsub("(for nasal administration)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(administered as inhalation solution through a nebulizer|administered through an inhaler)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for subcutaneous or intravenous injection)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(inhalation solution)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(injection into urinary bladder|intravesical)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for injection beneath the skin and/or into muscle )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for topical administration)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(intramuscular or intravenous)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for intramuscular)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(for intra-articular injection)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for intrathecal trial)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(injection, sterile)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for intravenous use|for intravenous infusion)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(injection, |infusion, |iv, )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(for therapeutic use)", ", therapeutic", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(for diagnostic use)", ", diagnostic", partBDataRAW$brand_name)

partBDataRAW$brand_name <- gsub("(, unit dose,)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(oral)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(parenteral)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(intravenous)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(intramuscular)", "", partBDataRAW$brand_name, ignore.case = T)


# remove dose indicator
partBDataRAW$brand_name <- gsub("(1 gram/0.125 grams \\(1.125 grams\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(sterile \\(500 ml=1 unit\\))", "", partBDataRAW$brand_name)

partBDataRAW$brand_name <- gsub("([0-9]+-[0-9]+ mg\\/ml iodine concentration)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+ i.u. vwf:rco |, per [0-9]+ iu vwf:rco|per iu vwf:rco)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per [0-9]+.[0-9]+ mg|per [0-9]+.[0-9]+ gm)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per [0-9]+ mg|per [0-9]+ milligrams|per [0-9]+mg|per [0-9]+ gm)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per 10 square centimeters)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per factor viii i.u.)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per mcg|per [0-9]+ mcg|per [0-9]+ microgram)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per gram|per gm|per milligram)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per square centimeter )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per microcurie )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per [0-9]+ ml|per [0-9]+ milliliters)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per ml)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per [0-9]+ meq)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per [0-9]+ iu)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per i.u.|per iu|per [0-9]+ units)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per 1,000 usp units|per 1000 usp units)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(per [0-9]+ usp unit \\(up to [0-9]+ usp units\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(per dose)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(, per single dose vial|per vial|per single use)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(per instillation)", "", partBDataRAW$brand_name, ignore.case = T)

partBDataRAW$brand_name <- gsub("([0-9]+ usp unit)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(up to 149 mg/ml iodine concentration)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(up to [0-9]+ mg|up to [0-9]+.[0-9]+ mg)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(up to [0-9]+ g|up to [0-9]+ gm)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(up to [0-9]+ ml)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(up to [0-9]+ cc)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(up to [0-9]+ mcg|up to [0-9]+ micrograms|up  to [0-9]+ mcg)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(up to [0-9,]+ units )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(over [0-9]+cc|over [0-9]+ cc)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+\\% in [0-9]+ ml)", "", partBDataRAW$brand_name)

partBDataRAW$brand_name <- gsub("([0-9]+.[0-9]+ mg|[0-9]+.[0-9]+mg)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("([0-9]+ mg|[0-9]+mg|[0-9]+  mg)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+ gram|[0-9]+ gm)", "", partBDataRAW$brand_name)

partBDataRAW$brand_name <- gsub("([0-9]+ microgram|[0-9]+ micrograms)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+.[0-9]+ mcg)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+ mcg)", "", partBDataRAW$brand_name, ignore.case = T)

partBDataRAW$brand_name <- gsub("([0-9]+.[0-9]+ ml|[0-9]+.[0-9]+ml)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+ ml|, [0-9.]+ml)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9,]+ units|, [0-9.]+units|[0-9,]+ unit)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+cc|[0-9]+ cc)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+,[0-9]+ iu |[0-9]+,[0-9]+ i.u.)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+ iu |[0-9]+ i.u.)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(\\([0-9]+ i.u.\\))", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(adult dosage)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(, full dose|, minidose)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(single unit dosage form)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(, 1 million units)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("([0-9]+.[0-9]+\\%)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("([0-9]+\\%)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(\\([0-9]+/[0-9]+\\))", "", partBDataRAW$brand_name)

# remove age indicator
partBDataRAW$brand_name <- gsub("(split virus, when administered to individuals 3 years of age and older)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(patient 3 years and older | to individuals 3 years of age and older )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(patient 6-35 months of age )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(to children 6-35 months of age)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(patient 7 years or older)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(patient 2 years or older )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(pediatric and adolescent patients |, adolescent patient )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(dialysis or immunosuppressed patient )", "", partBDataRAW$brand_name, ignore.case = T)

# remove various extraneous information
partBDataRAW$brand_name <- gsub("(\\(not to be used to report any adenosine phosphate compounds, instead use a9270\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(\\(not to be used to report any adenosine phosphate compounds; instead use a9270\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(\\(not to be used to report any adenosine phosphate compounds))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(preservative and antibiotic fre )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(, preservative free|, preservative-free for epidural or intrathecal use)", "", partBDataRAW$brand_name, ignore.case = T)

partBDataRAW$brand_name <- gsub("(\\(antihemophilic factor, recombinant\\))", ", recombinant", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(, fc fusion protein \\(recombinant\\))", ", recombinant", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(\\(antihemophilic factor, human\\))", ", human", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(\\(antihemophilic factor, purified, non-recombinant\\))", ", non-recombinant", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(antihemophilic factor)", "factor", partBDataRAW$brand_name, ignore.case = T)


partBDataRAW$brand_name <- gsub("(fda-approved final product|fda approved final product)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(non-compounded)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(unit dose form|concentrated form)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(unit dose)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(dose form)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(administered through dme)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(fda approved prescription anti-emetic, for use as a complete substitute for an iv anti-emetic at time of chemotherapy treatment not to exceed a 48 hour dosage regimen)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(fda approved prescription anti-emetic, for use as a complete therapeutic substitute for an iv anti-emetic at time of chemotherapy treatment not to exceed a 48 hour dosage regimen)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(fda approved prescription anti-emetic, for use as a complete therapeutic substitute for an iv anti-emetic at the time of chemotherapy treatment, not to exceed a [0-9]+ hour dosage regimen)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(fda approved prescription anti-emetic, for use as a complete  substitute for an iv anti-emetic at the time of chemotherapy treatment, not to exceed a [0-9]+ hour dosage regimen)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(code may be used for medicare when drug administered under the direct supervision of a physician, not for use when drug is self administered)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(adsorbed when administered to individuals 7 years or older)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(minimum of 50 million autologous cd54\\+ cells activated with pap-gm-csf, including leukapheresis and all other preparatory procedures, per infusion)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(, not otherwise specified)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(, provided in [0-9.]+ mg vial|provided in  vial)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(, solvent detergent)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(, aqueous)", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("( \\(for esrd on dialysis\\) )", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(\\(non-esrd use\\)|\\(for non-esrd use\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(\\([2-4] dose schedule\\))", "", partBDataRAW$brand_name, ignore.case = T)
partBDataRAW$brand_name <- gsub("(\\(mini dose\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(injectable)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(\\(preservative-free sterile solution\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(powder or solution)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(\\(for depot suspension\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(lyophilized \\(e.g. powder\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(non-lyophilized, \\(e.g. liquid\\)|non-lyophilized \\(e.g. liquid\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(vial)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(for treatment of iron deficiency anemia)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(\\( = \\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("( \\(human\\))", ", human", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(, \\()|, \\(|,  \\()", " \\(", partBDataRAW$brand_name)


#final cleaning- stripping white space and remnants
partBDataRAW$brand_name <- gsub("(\\($)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(  )", " ", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(  )", " ", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(,  | , )", ", ", partBDataRAW$brand_name)
partBDataRAW$brand_name <- str_trim(partBDataRAW$brand_name, side="both")
partBDataRAW$brand_name <- gsub("(\\($)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(\\(\\))", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- str_trim(partBDataRAW$brand_name, side="both")
partBDataRAW$brand_name <- gsub("( ,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(, ,|,,)", ", ", partBDataRAW$brand_name)
partBDataRAW$brand_name <- str_trim(partBDataRAW$brand_name, side="both")
partBDataRAW$brand_name <- gsub("( ,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- str_trim(partBDataRAW$brand_name, side="both")
partBDataRAW$brand_name <- gsub("( ,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("( ,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("( ,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(;$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(;)", ",", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- str_trim(partBDataRAW$brand_name, side="both")
partBDataRAW$brand_name <- gsub("(,$)", "", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(, and)", " and", partBDataRAW$brand_name)

# make abbreviations consistent
partBDataRAW$brand_name <- gsub("(g-csf)", "G-CSF", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(gm-csf)", "GM-CSF", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(rho d)", "Rho\\(D\\)", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(xiii)", "XIII", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(viii)", "VIII", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(viia)", "VIIa", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub(" (ix)", " IX", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(hcl$)", "hydrochloride", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("[^,] extended release", ", extended release", partBDataRAW$brand_name)
partBDataRAW$brand_name <- gsub("(Influenza virus vaccine)", "Vaccine for influenza", partBDataRAW$brand_name)

partBDataRAW$brand_name_lower <- tolower(partBDataRAW$brand_name)

# SAVE THE FILE
saveRDS(partBDataRAW, "partBDataRAW.rds")

# CONVERT FROM WIDE TO LONG FORMAT
#names(partBDataRAW)
#partBDataRAW <- readRDS("partBDataRAW.rds")
partBDataRAW$row <- 1:nrow(partBDataRAW)
varsToDeleteB <- which(names(partBDataRAW) %in% c("hcpcs_code"))
partBDataRAW <- partBDataRAW[, -varsToDeleteB]
partBDataRAW %>%  gather(var, value, -c(hcpcs_description, brand_name, brand_name_lower, row)) %>% # Convert to long format
  extract(var, c("var", "year"), "([a-z_]*)_(20[0-9]*)") %>% 
  spread(key=var, value=value)  -> drugBData

# make the varnames compatible
#drugData <- readRDS("drugData.rds") 
#names(drugData)
#names(drugBData)
drugBData %>% rename(formulation=hcpcs_description,
                     generic_name=brand_name_lower) %>% 
  select(-annual_change_in_average_cost_per_unit, -average_annual_beneficiary_cost_share, 
         -average_beneficiary_cost_share) -> drugBData
drugBDataSum <-drugBData %>% group_by(generic_name, year) %>% # test to make sure calcuations done properly
  summarise(claim_count=sum(claim_count, na.rm=T),
            beneficiary_count=sum(beneficiary_count, na.rm=T),
            total_spending=sum(total_spending, na.rm=T),
            unit_count=sum(unit_count, na.rm=T),
            average_cost_per_unit = total_spending/unit_count,
            total_annual_spending_per_user=total_spending/beneficiary_count)

# Create ranking and cost change by year
drugBDataSum %>% group_by(generic_name) %>% 
  arrange(generic_name, year) %>% 
  filter(!is.na(total_spending)) -> drugBDataSum
drugBDataSum %>% ungroup %>%  
  group_by(year) %>% arrange(year) %>% 
  mutate(pctile_cost_per_unit = percent_rank(average_cost_per_unit),
         pctile_total_cost = percent_rank(total_spending),
         nDrugs = sum(!is.na(total_spending))) %>% ungroup() -> drugBDataSum
drugBDataSum %>% group_by(generic_name) %>% arrange(year) %>% 
  mutate(annual_change_per_unit = average_cost_per_unit - lag(average_cost_per_unit),
         annual_change_per_unit_pct = annual_change_per_unit/lag(average_cost_per_unit)*100,
         annual_change_total = total_spending - lag(total_spending),
         annual_change_total_pct = annual_change_total/lag(total_spending)*100,
         pctile_differential =  pctile_total_cost - pctile_cost_per_unit) -> drugBDataSum
drugBDataSum %>% group_by(year) %>% arrange(desc(total_spending)) %>% 
  mutate(rank_total=row_number()) -> drugBDataSum
drugBDataSum %>% group_by(year) %>% arrange(desc(average_cost_per_unit)) %>% 
  mutate(rank_avg_cost=row_number()) -> drugBDataSum

# make variables for consistency
drugBDataSum$brand_name <- str_to_title(drugBDataSum$generic_name)
drugBDataSum$generic_name <- str_to_title(drugBDataSum$generic_name)
drugBDataSum$formulation <- paste0(drugBDataSum$generic_name, " -as- ", drugBDataSum$brand_name) 


# Create tooltip with html code
drugBDataSum$tip <-
  paste0("<h4><strong><span style=\"color: red\"}>(",stri_trans_totitle(drugBDataSum$generic_name), ")</span></strong></h4>",
         "<h4><strong><span style=\"color: red\"}>Medicaid Part B</span></strong></h4>",
         "<hr />", 
         "Total Cost(millions): $", round(drugBDataSum$total_spending/1000000, 2),"<br>",
         "Total Cost (%ile): ", round(drugBDataSum$pctile_total_cost, 2),  "<br>",
         "Total Cost (Rank): #", drugBDataSum$rank_total, " of ", drugBDataSum$nDrugs, "<br>",
         "<hr />", 
         "Unit Cost: $", round(drugBDataSum$average_cost_per_unit, 2),"<br>",
         "Unit Cost (%ile): ", round(drugBDataSum$pctile_cost_per_unit, 2), "<br>",
         "Unit Cost (Rank): #", drugBDataSum$rank_avg_cost, " of ", drugBDataSum$nDrugs, "<br>",
         "Price change($): $", ifelse(is.na(drugBDataSum$annual_change_per_unit), "", ifelse(drugBDataSum$annual_change_per_unit>0, "+", "")),
         ifelse(is.na(drugBDataSum$annual_change_per_unit)," --- ",round(drugBDataSum$annual_change_per_unit, 2)),  "<br>",
         "Price change(%): ",ifelse(is.na(drugBDataSum$annual_change_per_unit_pct), "", ifelse(drugBDataSum$annual_change_per_unit_pct>0, "+", "")),
         ifelse(is.na(drugBDataSum$annual_change_per_unit_pct)," --- ",round(drugBDataSum$annual_change_per_unit_pct, 2)), "%<br>")

drugBDataSum$brand_name <- gsub("(.*)\\((.*)\\)(.*)", "\\1-\\2\\3", drugBDataSum$brand_name) 
drugBDataSum$generic_name <- gsub("(.*)\\((.*)\\)(.*)", "\\1-\\2\\3", drugBDataSum$generic_name) 
drugBDataSum$formulation <- gsub("(.*)\\((.*)\\)(.*)", "\\1-\\2\\3", drugBDataSum$formulation) 

saveRDS(drugBDataSum, "partBData.rds")
rm(drugBData, drugBDataSum, partBDataRAW)
#______________________________________________________________________________________________________
# Post-processing
#______________________________________________________________________________________________________
partBData <- readRDS("partBData.rds")
partDData <- readRDS("partDData.rds") 
drugData <- bind_rows(list(partDData, partBData), .id="dataset")
drugData$dataset <- factor(drugData$dataset, levels = c(1,2), labels=c("Part D", "Part B"))
drugData$year <- as.numeric(drugData$year)
drugData$generic_name <- stringi::stri_trans_totitle(drugData$generic_name)
drugData$brand_name <- stringi::stri_trans_totitle(drugData$brand_name)
drugData <- drugData %>% filter(!average_cost_per_unit=="NaN" & !total_spending==0) 

drugData %>% group_by(year) %>% filter(beneficiary_count!=0) %>% 
  arrange(desc(total_annual_spending_per_user)) %>% 
  mutate(rank_total_per_user=row_number()) -> drugData


saveRDS(drugData, "drugData.rds")

#______________________________________________________________________________________________________
# CODE TO LOAD
#______________________________________________________________________________________________________
drugData <- readRDS("drugData.rds") # Contains both PART B and Part D


