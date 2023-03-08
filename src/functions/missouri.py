# Putting the imports outside the function instead. This helps to avoid importing the same module 
# multiple times if the function is called multiple times.

import pandas as pd

def missouri(input_path):

    with open(input_path) as html:
        df = pd.read_html(html)[0]

    return df