def missouri(input_path):

    import pandas as pd

    with open(input_path) as html:
        df = pd.read_html(html)[0]

    return df