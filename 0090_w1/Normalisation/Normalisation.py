# -*- coding: utf-8 -*-
"""
Created on Tue Oct  3 13:47:27 2023

@author: Jichu
"""

import pandas as pd

# Load the emission dataset
df1 = pd.read_csv('DataSet1_no_sector.csv')  

# Load the other dataset
df2 = pd.read_csv('Population.csv')  

# Iterate through each row in the emission dataset
for index, row in df1.iterrows():
    country_code = row['Country Code']
    
    # Get the corresponding country row
    population_row = df2[df2['Country Code'] == country_code]
    
    # Perform the division while handling the case when population is 0
   
    # Emission/Other Dataset
    division_result = row['1960':] / population_row.iloc[0, 1:]
    
    # Other Dataset/Emission
    # division_result = population_row.iloc[0, 1:] / row['1960':]
    
    df1.loc[index, '1960':] = division_result.fillna(0)  # Fill NaN (result of 0/0) with 0

# Save the result as a new CSV file
df1.to_csv('Emission_per_Capita.csv', index=False)  # Replace 'result.csv' with the desired filename
