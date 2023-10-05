# -*- coding: utf-8 -*-
"""
Created on Wed Oct  4 14:51:40 2023

@author: Jichu
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

file_path = "Emission_per_Capita.csv"
df = pd.read_csv(file_path)

# Define the country codes
#country_codes = ["ARM", "AZE", "BLR", "GEO", "KAZ", "KGZ", "LVA", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB", "LTU", "EST"]
country_codes = ["AZE", "BLR", "GEO", "KAZ", "RUS", "UKR", "UZB"]
#country_codes = ["EST"]
# Define the sectors
#sectors = ["Agriculture", "Energy", "Industrial Processes and Product Use", "Other", "Total excluding LULUCF", "Waste"]
sectors = ["Total excluding LULUCF"]

# Define year interval
Year = np.linspace(1960, 2021, 62)

# Loop through the sectors
for sector in sectors:
    # Loop through the gases in the current sector
    for gas in df[df['sector'] == sector]['gas'].unique():
        # Initialize an empty plot for the current sector and gas
        fig, ax = plt.subplots()
        fig.set_size_inches(10, 8)

        # Loop through the country codes
        for country_code in country_codes:
            # Filter the DataFrame to get rows for the specific country code, sector, and gas
            filtered_df = df[(df['Country Code'] == country_code) & (df['sector'] == sector) & (df['gas'] == gas)]

            if not filtered_df.empty:
                # Extract the gas values for the current country, sector, and gas
                gas_values = filtered_df.iloc[:, 4:].values.flatten()

                plt.plot(Year, gas_values, label=f'{country_code}')
                
        # seperate code to plot EST        
        filtered_df = df[(df['Country Code'] == "EST") & (df['sector'] == sector) & (df['gas'] == gas)]

        if not filtered_df.empty:
            # Extract the gas values for the current country, sector, and gas
            gas_values = filtered_df.iloc[:, 4:].values.flatten()

            plt.plot(Year, gas_values, label="EST", color='black', linestyle='dashed', linewidth=2)

        # Set labels and title for the current sector and gas
        plt.xlabel('Year')
        plt.ylabel('Emission/MtCO2e')
        plt.title(f'{sector} - {gas} Gas Values Comparison')
        plt.legend()
        
        file_name = f'{file_path} - {sector} - {gas} Gas Values Comparison.png'
        plt.savefig(file_name)
        
        plt.show()
        
        # Close the current plot for the next sector and gas
        plt.clf()