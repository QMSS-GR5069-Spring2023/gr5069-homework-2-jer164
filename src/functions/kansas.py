def kansas(input_path):

    from bs4 import BeautifulSoup, SoupStrainer
    import pandas as pd
    import re


    strainer = SoupStrainer('span', attrs={"id": \
                                       [re.compile('lblContributor.*'), re.compile('lblAddress.*'), re.compile('lblCity.*'), re.compile('lblState.*'), re.compile('lblZip.*'), re.compile('lblDate.*'), re.compile('lblAmount.*')]})
    with open(input_path) as html:
        soup = BeautifulSoup(html, "lxml", parse_only=strainer)

    names = []
    addresses = []
    cities = []
    states = []
    zips = []
    dates = []
    amounts = []
    donors_df = pd.DataFrame()

    for name in soup.find_all('span', id = re.compile('lblContributor.*')):
        names.append(name.text)

    for address in soup.find_all('span', id = re.compile('lblAddress_.*')):
        addresses.append(address.text)

    for city in soup.find_all('span', id = re.compile('lblCity.*')):
        cities.append(city.text)

    for state in soup.find_all('span', id = re.compile('lblState.*')):
        states.append(state.text)

    for zipcode in soup.find_all('span', id = re.compile('lblZip.*')):
        zips.append(zipcode.text)

    for date in soup.find_all('span', id = re.compile('lblDate.*')):
        dates.append(date.text)

    for amount in soup.find_all('span', id = re.compile('lblAmount.*')):
        amounts.append(amount.text)

    donors_df['full_name'] = names
    donors_df['addr1'] = addresses
    donors_df['city'] = cities
    donors_df['state'] = states
    donors_df['zip'] = zips
    donors_df['donation_date'] = dates
    donors_df['donation_amount'] = [s.lstrip("$") for s in amounts]
    
    donors_df = donors_df.drop_duplicates(subset=["full_name"], keep="first")

    return donors_df