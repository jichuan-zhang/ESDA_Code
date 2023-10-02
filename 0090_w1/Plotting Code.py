# -*- coding: utf-8 -*-
"""
Created on Mon Oct  2 23:47:14 2023

@author: Jichu
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Load your dataset into a Pandas DataFrame (assuming you've already done this)

# Assuming your dataset is in a CSV file named "data.csv"
file_path = "DataSet1.csv"

# Read the CSV file into a Pandas DataFrame
df = pd.read_csv(file_path)

# Define the country codes you want to plot
country_codes = ["AFG", "AGO"]  # Replace with your desired country codes

# Define the sectors you want to compare
sectors = ["Agriculture", "Energy", "Industrial Processes and Product Use", "Other", "Total excluding LULUCF", "Waste"]

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
            # Extract the gas columns for the current sector
            gas_type = filtered_df.iloc[:, 3].tolist()  # Assuming the gas columns start from column 4

            # Plot each gas as a line on the same graph
            for gas in gas_type:
                # Find the row in filtered_df where the 4th column matches the gas_column
                gas_row = filtered_df[filtered_df.iloc[:, 3] == gas]

                # Check if a row with the gas_column was found
                if not gas_row.empty:
                    gas_value = gas_row.iloc[:, 4:].values.flatten()

                    plt.plot(Year, gas_value, label=f'{country_code} - {gas}')

    # Set labels and title
    plt.xlabel('Year')
    plt.ylabel('Value')
    plt.title(f'{sector} Gas Values Comparison')

    # Add a legend to distinguish gas types and country codes
    plt.legend()

    # Show or save the plot (uncomment the relevant line)
    plt.show()  # Uncomment this line to display the plot

    # Clear the current plot for the next sector
    plt.clf()