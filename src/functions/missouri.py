# Putting the imports outside the function instead. This helps to avoid importing the same module 
# multiple times if the function is called multiple times.

import pandas as pd

def missouri(input_path):

    """
    Reads an HTML file and returns a pandas DataFrame with the contents.

    Args:
        input_path (str): the path to the input HTML file

    Returns:
        pandas.DataFrame: a DataFrame containing the contents of the HTML file
   
    """


    with open(input_path) as html:
        df = pd.read_html(html)[0]

    return df