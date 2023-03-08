# Putting the imports outside the function instead. This helps to avoid importing the same module 
# multiple times if the function is called multiple times.

import requests
import pandas as pd

def philadelphia(candidate):

    good_names = ["donation_date", "donation_amount", "full_name", "addr1", "city", "state", "zip", \
    "full_address", "first_name", "middle_name", "last_name", \
    "addr2",	"phone1", "phone2", "email1", "email2"]

    response = requests.get(f"https://cf-api.phila.gov/services/campaign-finance/search/searchtransactions?query=&limit=10000&ascending=1&page=1&byColumn=0&orderBy=transactionDate&searchString={candidate}&amount=&isSearchByFilingEntity=true&year=&cycleCode=&isSearchOnExpenditure=false")
    j = response.json()
    df = pd.json_normalize(j['transactionList'])
    df = df.rename(columns={'transactionEntityName': 'full_name', 'transactionEntityAddress': 'full_address',  'EntityCity': 'city', \
        'transactionDate': 'donation_date', 'transactionAmount': 'donation_amount'})

    for col in df.columns.values:
        if col not in good_names:
            df.drop(col, axis=1, inplace=True)
            
    for col in good_names:
        if col not in df.columns.values:
            df[col] = ""

    df['donation_date'] = df['donation_date'].astype(str).str[0:10]
    
    return df