def kansas(input_path):

    from bs4 import BeautifulSoup, SoupStrainer
    import pandas as pd
    import re


    strainer = SoupStrainer('span', attrs={"id": \
                                       [re.compile('lblContributor.*'), re.compile('lblAddress.*'), re.compile('lblCity.*'), re.compile('lblState.*'), re.compile('lblZip.*'), re.compile('lblDate.*'), re.compile('lblAmount.*')]})
    with open(input_path) as html:
        soup = BeautifulSoup(html, "lxml", parse_only=strainer)

    
    # The code below was changed into a list comprehension for more concise code:

    names = [name.text for name in soup.find_all('span', id=re.compile('lblContributor.*'))]
    addresses = [address.text for address in soup.find_all('span', id=re.compile('lblAddress_.*'))]
    cities = [city.text for city in soup.find_all('span', id=re.compile('lblCity.*'))]
    states = [state.text for state in soup.find_all('span', id=re.compile('lblState.*'))]
    zips = [zipcode.text for zipcode in soup.find_all('span', id=re.compile('lblZip.*'))]
    dates = [date.text for date in soup.find_all('span', id=re.compile('lblDate.*'))]
    amounts = [amount.text for amount in soup.find_all('span', id=re.compile('lblAmount.*'))]


    # The code below was changed to create a dataframe in a more effcient way:

    donors_df = pd.DataFrame({
        'full_name': names,
        'addr1': addresses,
        'city': cities,
        'state': states,
        'zip': zips,
        'donation_date': dates,
        'donation_amount': [s.lstrip("$") for s in amounts]
    })

    donors_df.drop_duplicates(subset=["full_name"], keep="first", inplace=True)

    return donors_df





