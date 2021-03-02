#library in the R package that we need
library(tidyverse)

#Load in the excel spreadsheet as a data frame
EPS_data <- read_excel("Band8adata.xlsx")

#Rename columns so that they don't have spaces to make them easier to manipulate in R
EPS_data <- rename(EPS_data, 
                   practiceCode = Practice_Code,
                   weightedIMD = "weightedIMD (higher score more deprived)",
                   ruralScore = "Rural score (higher more rural)",
                   GPCount = "GP Count",
                   dispensingListSize = "Dispensing List Size",
                   prescribingListSize = "Prescribing List Size",
                   totalListSize = "Total List Size",
                   EPSItemsPerMonth = "EPS ITEMS per month"
                   )

#First work out how much EPS will cost each practice in software and training costs
#Costs for list size bands (source: table on page 2)
cost_under_5000 <- 2000
cost_5001_12000 <- 3000
cost_over_12001 <- 4000

#Add a column to the data for software and training costs based on total list size
EPS_data <- EPS_data %>%
  mutate(costs = if_else(totalListSize < 5000, 
                         cost_under_5000, 
                         ifelse(totalListSize < 12000, 
                                cost_5001_12000, 
                                cost_over_12001)))

#Next, work out how much each practice will save on printing costs
EPS_data <- EPS_data %>%
  #Add a column for the average number of ESP forms sent to each practice per month
  mutate(EPSFormsPerMonth = EPSItemsPerMonth / 2) %>%
  #Add a column for the average number of acute ESP forms per month
  mutate(acuteEPSFormsPerMonth = EPSFormsPerMonth * 0.3) %>%
  #Add column for number of tokens printed per month
  mutate(numberOfTokensPrinted = acuteEPSFormsPerMonth) %>%
  #Add a column for the printing costs saved per month
  mutate(savingsPerMonth = numberOfTokensPrinted * 0.1) %>%
  #Add a column for savings per year
  mutate(savingsPerYear = savingsPerMonth * 12) %>%
  #Add a column saying whether the practice has seen a financial benefit after the first year
  mutate(benefitOrLoss = if_else(savingsPerYear >= costs, "Benefit", "Loss")) %>%
  #Add a column for the number of years to recoup the initial costs to 2 d.p.
  mutate(yearsToRecoupExpenditiure = round(costs / savingsPerYear, 2))

#create separate table showing years to recoup expenditure for each practice
expenditureRecoupYears <- EPS_data %>%
  select(practiceCode, yearsToRecoupExpenditiure)

#return a list of practices that will financially benefit after the first year
practiceWinners <- EPS_data %>%
  filter(benefitOrLoss == "Benefit") %>%
  select(practiceCode)

#return a list of practices that will make a financial loss after the first year
practicesLosers <- EPS_data %>%
  filter(benefitOrLoss == "Loss") %>%
  select(practiceCode)

#save results as CSV file
write.csv(EPS_data, "EPS_data.csv")
write.csv(expenditureRecoupYears, "expenditureRecoupYears.csv")
write.csv(practiceWinners, "practiceWinners.csv")
write.csv(practicesLosers, "practicesLosers.csv")

