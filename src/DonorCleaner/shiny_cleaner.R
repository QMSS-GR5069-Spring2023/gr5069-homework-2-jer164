library(tidyverse)
library(glue)
library(lubridate)
library(janitor)
library(stringr)



donor_cleaner <- function(input_data_path, state_fin){
  
  if (is_zip == TRUE){
    
    unzip(input_data_path) #unzip to environment and get into read_csv 
    
  }
  
  good_names <- c("donation_date", "donation_amount",	"full_name", "addr1", 
                  "city",	"state","zip", "full_address", "first_name", "middle_name",	
                  "last_name", "addr2",	"phone1",	"phone2",	"email1",	"email2")
  
  temp_data <- read.csv(input_data_path) %>% as_tibble()
  #temp_data <- temp_data %>% clean_names()
  
  ###### state-level transforms

  if (state_fin == "IL"){
  
  temp_data <- temp_data %>% rename("full_name" = "ContributedBy",
                                    "donation_amount" = "Amount",
                                    "donation_date" = "RcvdDate",
                                    "addr1" = "Address1",
                                    "addr2" = "Address2",
                                    "state" = "State",
                                    "city" = "City", 
                                    "zip" = "Zip")
  
  temp_data <- temp_data %>% add_column("full_address" = '',
                          "first_name" = '',
                          "middle_name" = '',
                          "last_name" = '',
                          "phone1" = '',
                          "phone2" = '',
                          "email1" = '',
                          "email2" = '')}
  
  else if (state_fin == "AL"){
    
    temp_data <- temp_data %>% rename("full_name" = "Contributor",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "ContributionDate",
                                      "state" = "State",
                                      "city" = "City", 
                                      "zip" = "ZipCode")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "first_name" = '',
                                          "middle_name" = '',
                                          "last_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr1" = '',
                                          "addr2" = ''
      )
  }
  
  else if (state_fin == "AK"){
    
    temp_data <- temp_data %>% rename("first_name" = "FirstName",
                                      "last_name" = "Last_BusinessName",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "Date",
                                      "state" = "State",
                                      "city" = "City",
                                      "full_address" = "Address",
                                      "zip" = "Zip")
    
    temp_data <- temp_data %>% add_column("full_name" = '',
                                          "middle_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr1" = '',
                                          "addr2" = ''
    )
    
    temp_data$donation_amount <- as.numeric(gsub("\\$", "", temp_data$donation_amount))
  }
  
  else if (state_fin == "AZ"){
    
    temp_data <- temp_data %>% rename("full_name" = "ReceivedFromName",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "Date")
    
    temp_data <- temp_data %>% add_column("first_name" = '',
                                          "last_name" = '',
                                          "middle_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr1" = '',
                                          "addr2" = '',
                                          "state" = "State",
                                          "city" = "City",
                                          "full_address" = "Address",
                                          "zip" = "Zip")
    
    temp_data$donation_amount <- as.numeric(gsub("\\$", "", temp_data$donation_amount))
  }
  
  else if (state_fin == "CA"){
    
    temp_data <- temp_data %>% clean_names()
    
    temp_data <- temp_data %>% rename("full_name" = "contributor_name",
                                      "donation_amount" = "amount",
                                      "donation_date" = "start_date",
                                      "state" = "contributor_state",
                                      "city" = "contributor_city", 
                                      "zip" = "contributor_zip_code")
    
    temp_data <- temp_data %>% add_column("full_address" = 'NULL',
                                          "first_name" = '',
                                          "middle_name" = '',
                                          "last_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr1" = '',
                                          "addr2" = ''
    )
    
    temp_data$zip <- substr(temp_data$zip, 0, 5)
    temp_data <- temp_data %>% filter(zip >= 5)
    
  }
  
  else if (state_fin == "CO"){
    
    temp_data <- temp_data %>% rename("full_name" = "Contributor",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "ContributionDate",
                                      "state" = "State",
                                      "city" = "City")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "first_name" = '',
                                          "middle_name" = '',
                                          "last_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr1" = '',
                                          "addr2" = '',
                                          "zip" = ''
    )
  }

  else if (state_fin == "DE"){
    
    temp_data <- temp_data %>% rename("full_name" = "Contributor_Name",
                                      "addr1" = "Contributor_Address_Line_1",
                                      "donation_amount" = "Contribution_Amount",
                                      "donation_date" = "Contribution_Date",
                                      "state" = "Contributor_State",
                                      "city" = "Contributor_City",
                                      "zip" = "Contributor_Zip")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "first_name" = '',
                                          "middle_name" = '',
                                          "last_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr2" = ''
    )
  }
  
  else if (state_fin == "FL"){
    
    temp_data <- temp_data %>% rename("full_name" = "Contributor_Name",
                                      "full_address" = "Address",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "Date",
                                      "state" = "State",
                                      "city" = "City",
                                      "zip" = "Zip")
    
    temp_data <- temp_data %>% add_column("addr1" = '',
                                          "first_name" = '',
                                          "middle_name" = '',
                                          "last_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr2" = ''
    )
  }
  
  else if (state_fin == "HI"){
    
    temp_data <- temp_data %>% clean_names()
    
    temp_data <- temp_data %>% rename("full_name" = 'contributor_name',
                                      "addr1" = "address_1",
                                      "addr2" = "address_2",
                                      "donation_amount" = "amount",
                                      "donation_date" = "date",
                                      "zip" = "zip_code",)
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "middle_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "first_name" = '',
                                          "last_name" = ''
    )
  }
  
  else if (state_fin == "IA"){
    
    temp_data <- temp_data %>% rename("first_name" = "First_Name",
                                      "last_name" = "Last_Name",
                                      "addr1" = "Address_Line_1",
                                      "donation_amount" = "Contribution_Amount",
                                      "donation_date" = "Date",
                                      "state" = "State",
                                      "city" = "City",
                                      "zip" = "Zip_Code")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "full_name" = '',
                                          "middle_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr2" = ''
    )
  }
  
  else if (state_fin == "ID"){
    
    temp_data <- temp_data %>% clean_names()
    
    temp_data <- temp_data %>% rename("full_name" = "from_name",
                                      "full_address" = 'from_address',
                                      "donation_amount" = "amount",
                                      "donation_date" = "date",
                                      "state" = "from_state",
                                      "city" = "from_city",
                                      "zip" = "from_zip")
    
    temp_data <- temp_data %>% add_column("middle_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr1" = '',
                                          "addr2" = '',
                                          "first_name" = '',
                                          "last_name" = '',
    )
    
    temp_data <- temp_data %>% mutate(donation_date = as.Date(donation_date))
    
  }
  
  else if (state_fin == "KY"){
    
    temp_data <- temp_data %>% rename("first_name" = "ContributorFirstName",
                                      "last_name" = "ContributorLastName",
                                      "addr1" = "Address1",
                                      "addr2" = "Address2",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "ReceiptDate",
                                      "state" = "State",
                                      "city" = "City",
                                      "zip" = "Zip")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "middle_name" = '',
                                          "full_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = ''
    )
    
    temp_data <- temp_data %>% mutate(donation_date = mdy_hms(donation_date,tz=Sys.timezone()))
    
  }
  
  else if (state_fin == "LA"){
    
    temp_data <- temp_data %>% rename("full_name" = "ContributorName",
                                      "addr1" = "ContributorAddr1",
                                      "addr2" = "ContributorAddr2",
                                      "donation_amount" = "ContributionAmt",
                                      "donation_date" = "ContributionDate",
                                      "state" = "ContributorState",
                                      "city" = "ContributorCity",
                                      "zip" = "ContributorZip")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "first_name" = '',
                                          "last_name" = '',
                                          "middle_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = ''
    )
    temp_data$donation_amount <- as.numeric(gsub("\\$|,", "", temp_data$donation_amount))
    
  }
  
  else if (state_fin == "MN"){
    
    temp_data <- temp_data %>% rename("full_name" = "Contributor",
                                      "addr1" = "Address_Line_1",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "Receipt_date",
                                      "state" = "State",
                                      "city" = "City",
                                      "zip" = "Zip_Code")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "first_name" = '',
                                          "last_name" = '',
                                          "middle_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr2" = ''
    )
  }
  
  else if (state_fin == "MT"){
    
    temp_data <- temp_data %>% rename("first_name" = "First_Name",
                                      "last_name" = "Last_Name",
                                      "middle_name" = 'Middle_Initial',
                                      "addr1" = "Addr_Line1",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "Date_Paid",
                                      "state" = "State",
                                      "city" = "City",
                                      "zip" = "Zip")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "full_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "addr2" = ''
    )
    
    temp_data <- temp_data %>% mutate(first_name = ifelse(first_name == '', Entity_Name, 'No_Name'))
  }
  
  else if (state_fin == "ME"){
    
    temp_data <- temp_data %>% clean_names()
    
    temp_data <- temp_data %>% rename("first_name" = "receipt_amount",
                                      "last_name" = "receipt_amount",
                                      "addr1" = "address1",
                                      "addr2" = "address2",
                                      "donation_amount" = "receipt_amount",
                                      "donation_date" = "receipt_date",
                                      "state" = "state",
                                      "city" = "city",
                                      "zip" = "zip")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "full_name" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = ''
    )
    temp_data$donation_amount <- as.numeric(gsub("\\$|,", "", temp_data$donation_amount))

  }
  
  else if (state_fin == "ND"){
    
    temp_data <- temp_data %>% rename("full_name" = "Name",
                                      "addr1" = "AddressStreet",
                                      "addr2" = "AddressStreet2",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "TransactionDate",
                                      "state" = "AddressStateID",
                                      "city" = "AddressCity",
                                      "zip" = "AddressZip")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "first_name" = '',
                                          "last_name" = '',
                                          "middle_name" = ''
    )
    temp_data$donation_amount <- as.numeric(gsub("\\$|,", "", temp_data$donation_amount))
    
  }
  
  else if (state_fin == "NJ"){
    
    
    temp_data <- temp_data %>% rename("first_name" = "FirstName",
                                      "last_name" = "LastName",
                                      "middle_name" = "MI",
                                      "full_address" = "Street",
                                      "donation_amount" = "ContributionAmount",
                                      "donation_date" = "ContributionDate",
                                      "state" = "State",
                                      "city" = "City",
                                      "zip" = "ZIP")
    
    temp_data <- temp_data %>% add_column("addr1" = '',
                                          "addr2" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "full_name" = ''
    )
   
    temp_data <- temp_data %>% mutate(full_name = ifelse(first_name == '', NonIndName, '')) 
  }
  
  else if (state_fin == "NY"){
  
    
    temp_data <- temp_data %>% rename("first_name" = "FirstName",
                                      "last_name" = "LastName",
                                      "middle_name" = "MiddleName",
                                      "full_address" = "StreetAddress",
                                      "donation_amount" = "Amount",
                                      "donation_date" = "TransactionDate",
                                      "state" = "State",
                                      "city" = "City",
                                      "zip" = "ZipCode")
    
    temp_data <- temp_data %>% add_column("addr1" = '',
                                          "addr2" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "full_name" = ''
    )
    temp_data$donation_amount <- as.numeric(gsub("\\$|,", "", temp_data$donation_amount))
    temp_data <- temp_data %>% mutate(zip = str_extract(zip, '\\d{5}'))
    
  }
  
  else if (state_fin == "OH"){
    
    temp_data <- temp_data %>% clean_names()
    
    temp_data <- temp_data %>% rename("first_name" = "first_name",
                                      "last_name" = "last_name",
                                      "middle_name" = "middle_name",
                                      "full_address" = "address",
                                      "donation_amount" = "amount",
                                      "donation_date" = "contribution_date",
                                      "state" = "state",
                                      "city" = "city",
                                      "zip" = "zip")
    
    temp_data <- temp_data %>% add_column("phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "full_name" = '',
                                          "addr1" = '',
                                          "addr2" = '',
    )
    
    temp_data$donation_amount <- as.numeric(gsub("\\$|,", "", temp_data$donation_amount))
    temp_data <- temp_data %>% filter(full_address != '')
    temp_data <- temp_data %>% filter(nchar(first_name) > 2)
    temp_data <- temp_data %>% filter(nchar(zip) == 5) %>% #these transforms probably don't matter for Ohio specifically
      mutate(first_name = gsub("&|\\.", '', first_name)) %>% 
      mutate(middle_name = '') %>% 
      mutate(first_name = str_to_title(first_name)) %>% 
      mutate(last_name = str_to_title(last_name)) %>% 
      mutate(city = str_to_title(city))
    
  }
  
  else if (state_fin == "OR"){
    
    temp_data <- temp_data %>% clean_names()
    
    temp_data <- temp_data %>% rename("full_name" = "contributor_payee",
                                      "addr1" = "addr_line1",
                                      "addr2" = "addr_line2",
                                      "donation_amount" = "amount",
                                      "donation_date" = "tran_date",
                                      "state" = "city",
                                      "city" = "state",
                                      "zip" = "zip")
    
    temp_data <- temp_data %>% add_column("phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "first_name" = '',
                                          "last_name" = '',
                                          "middle_name" = '',
                                          "full_address" = ''
    )
    
    
  }
  
  else if (state_fin == "UT"){

    
    temp_data <- temp_data %>% rename("full_name" = "NAME",
                                      "addr1" = "ADDRESS1",
                                      "addr2" = "ADDRESS2",
                                      "donation_amount" = "TRAN_AMT",
                                      "donation_date" = "TRAN_DATE",
                                      "state" = "CITY",
                                      "city" = "STATE",
                                      "zip" = "ZIP")
    
    temp_data <- temp_data %>% add_column("phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "first_name" = '',
                                          "last_name" = '',
                                          "middle_name" = '',
                                          "full_address" = ''
    )
    
    temp_data <- temp_data %>% filter(addr1 != '')
    
  }
  
  else if (state_fin == "VT"){
    
    temp_data <- temp_data %>% clean_names()
    
    
    temp_data <- temp_data %>% rename("full_name" = "contributor_name",
                                      "full_address" = "contributor_address",
                                      "donation_amount" = "amount",
                                      "donation_date" = "transaction_date",
                                      "state" = "town_state",
                                      "city" = "town_city")
    
    temp_data <- temp_data %>% add_column("phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "first_name" = '',
                                          "last_name" = '',
                                          "middle_name" = '',
                                          "addr1" = '', 
                                          "addr2" = '',
                                          "zip" = ''
    )
    
    temp_data <- temp_data %>% mutate(donation_date = mdy_hm(donation_date,tz=Sys.timezone()))
  
  }
  
  else if (state_fin == "WI"){
    
    temp_data <- temp_data %>% rename("full_name" = "ContributorName",
                                      "addr1" = "AddressLine1",
                                      "addr2" = "AddressLine2",
                                      "donation_amount" = "ContributionAmount",
                                      "donation_date" = "TransactionDate",
                                      "state" = "StateCode",
                                      "city" = "City",
                                      "zip" = "Zip")
    
    temp_data <- temp_data %>% add_column("full_address" = '',
                                          "phone1" = '',
                                          "phone2" = '',
                                          "email1" = '',
                                          "email2" = '',
                                          "first_name" = '',
                                          "last_name" = '',
                                          "middle_name" = ''
    )
    
  }
  
  ###### ABBA transforms
  
  temp_data <- temp_data %>% select(good_names)
  temp_data <- temp_data %>% mutate(donation_amount = as.integer(donation_amount)) %>% 
    filter(donation_amount > 0)
  
  return(temp_data)
  
}
