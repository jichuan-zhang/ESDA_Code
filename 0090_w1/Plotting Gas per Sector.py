# -*- coding: utf-8 -*-
"""
Created on Mon Oct  2 23:47:14 2023

@author: Jichu
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

file_path = "Emission_per_Capita.csv"
df = pd.read_csv(file_path)

# Define the country codes
country_codes = ["BEL", "AUT"]

# Define the sectors
sectors = ["Agriculture", "Energy", "Industrial Processes and Product Use", "Other", "Total excluding LULUCF", "Waste"]

# Define year interval
Year = np.linspace(1960, 2021, 62)

# Loop through the sectors and create plots for each country code
for sector in sectors:
    
    # Initialize an empty plot
    fig, ax = plt.subplots()

    # Loop through the country codes
    for country_code in country_codes:
        # Filter the DataFrame to get rows for the specific country code and sector
        filtered_df = df[(df['Country Code'] == country_code) & (df['sector'] == sector)]

        if not filtered_df.empty:
            # Extract the gas type for the current sector
            gas_type = filtered_df.iloc[:, 3].tolist()  

            # Plot each gas as a line on the same graph
            for gas in gas_type:
                # Find the row in filtered_df where the 4th column matches the gas type
                gas_row = filtered_df[filtered_df.iloc[:, 3] == gas]

                # Check if a row with the gas type was found
                if not gas_row.empty:
                    gas_value = gas_row.iloc[:, 4:].values.flatten()

                    plt.plot(Year, gas_value, label=f'{country_code} - {gas}')

    # Set labels and title
    plt.xlabel('Year')
    plt.ylabel('Emission/MtCO2e')
    plt.title(f'{sector} Gas Values Comparison')
    plt.legend()
    
    file_name = f'{file_path} - {sector} Gas Values Comparison.png'
    plt.savefig(file_name)
    
    plt.show()

    # Clear the current plot for the next sector
    plt.clf()