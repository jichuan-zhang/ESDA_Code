# -*- coding: utf-8 -*-
"""
Created on Wed Oct  4 14:11:52 2023

@author: Jichu
"""

import pandas as pd

# Load your dataset into a Pandas DataFrame
df = pd.read_csv("Population.csv")

# Create a list of country codes for aggregation
country_codes_to_aggregate = ["ARM", "AZE", "BLR", "GEO", "KAZ", "KGZ", "LVA", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB", "LTU", "EST"]

# Filter the DataFrame to keep only the selected country codes
filtered_df = df[df["Country Code"].isin(country_codes_to_aggregate)]

# Group by "Country", "Sector", and "Gas", and sum the values
result_df = filtered_df.groupby(["Country", "sector", "gas"]).sum().reset_index()

# Generate a list of year columns from 1960 to 2021
years = [str(year) for year in range(1960, 2022)]

# Sum the values for each year across different countries
result_df = result_df.groupby(['sector', 'gas']).sum().reset_index()

# Flatten the column names and reset the index
result_df.columns = [" ".join(col).strip() if col[0] not in ('sector', 'gas') else "Result" for col in result_df.columns.values]
result_df.reset_index(drop=True, inplace=True)

# Export the result to a CSV file
result_df.to_csv("USSR_Population.csv", index=False)