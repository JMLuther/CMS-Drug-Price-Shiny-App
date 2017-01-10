# CMS-Drug-Price-Shiny-App

Interactive Medicare Part B and D Drug Prices for 2011-2015  
by [JM Luther](https://twitter.com/DrJMLuther)

## Medicare Part B & D Data  

Medicare releases data to the public on their website, and they recently released a dataset with the cost of medications paid by Medicaid Part D, which accounts for ~75% of medications paid for by Medicaid.  This does not include any data from private insurers, and therefore this data is representative of a different population that the average insured American. Data can be download  as an Excel file (.xlsx) from the [2015 Medicare Drug Spending Data](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Information-on-Prescription-Drugs/2015MedicareData.html). It contains drug costs paid by Medicare Part D between 2011-2015. 

Part B data is summarised by drug when possible, and all possible drugs are contained in the datasets. A selected subset of drug data is available at the [CMS Dashboard](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Dashboard/2015-Medicaid-Drug-Spending/2015-Medicaid-Drug-Spending.html).

## Known Issues:  
This dataset contains average drug prices paid by Medicare Part D and Part B claims, which account for ~75% of medications paid for by Medicaid. Part D reflects prescriptions filled through Pharmacies, and Part B reflects medications administered in Outpatient settings (e.g., Doctor's office, Pharmacy). This does not include any data from private insurers, and therefore it is not representative of the general population. These data also do not account for manufacturer rebates, which reduce the actual cost paid by Medicare/Medicaid

Some results differ for reasons that are not clear to me, and hopefully others will help clean this data and fix these discrepancies. The rebates may also explain the difference in cost. In general, the Total Cost for the highest cost drugs are approximately twice the actual CMS Total Drug cost, although there is a highly variable relationship between the two. Similarly, the trends in unit prices and total costs generally reflect the actual CMS costs.  
  *  As examples, compare Harvoni, Advair Diskus, Lyrica, Humira, Sovaldi, Vyvanse  
  *  If you can contribute to cleaning this data, I welcome it.  

## Source Data  
Click on the "Part D data" link to download the .zip folder containing the excel file.  The data is not clean, and a few dataframe tidying steps are needed.  The data handling code can be found in the `data-raw` folder  
  *  Import data using the `read_excel` function from the `readxl` package.  Need to download just the Data sheet, and avoid the first few lines.  
  *  Variable names have been cleaned up  
  *  The data is in wide format, with repeated variables representing the same thing, just different years (e.g., `cost_2011`, `cost_2012`, etc.).  This is cleaned up quickly using a few `tidyr` and `dplyr` functions.:  
  *  For some reason, CMS just calculated the price change for the year 2015, even though data is there for every year.  I've calculated for each year.  
  
## Credits (not necessarily comprehensive):  
  1.  [CMS.gov](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Dashboard/2015-Medicaid-Drug-Spending/2015-Medicaid-Drug-Spending.html) who releases data for public use  
  2.  (Multiple tidyverse packages)[https://github.com/tidyverse/tidyverse]  
  3.  [ggiraph](https://twitter.com/davidgohel?lang=en) to make the graphs interactive  
  4.  [Shiny](http://shiny.rstudio.com/)  
