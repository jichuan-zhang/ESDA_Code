# -*- coding: utf-8 -*-
"""
Created on Sat Oct 21 21:28:00 2023

@author: Jichu
"""

import pandas as pd
import os
import zipfile

# Can't really be asked to expose my whole system directory, so doing some manipulation to get to the directory of the repo first

# This calls the path of current file
current = os.path.realpath(__file__)

# Record the current path
current_path = os.path.dirname(current)

# This is the folder name of the repo
repo = "ESDA_Code"

# This finds the path of the parent of current file, which is actually ESDA_Codes
repo_path = os.path.realpath(current)

# This loop does absolutely nothing here but if there are more layers then it will loop until it finds the path I want
while os.path.basename(repo_path) != repo:
    repo_path = os.path.dirname(repo_path)

# Very funny he doesn't realise read_csv don't work when there is more than one thing in that zip
with zipfile.ZipFile("Datasets.zip") as z:
    with z.open("Datasets/CW_CAIT_GHG.csv") as f:
        df0 = pd.read_csv(f)
        print(df0.head())

# Below is what I think he actually want to see, I grabbed some old data and zipped it
# Method 1
df1 = pd.read_csv("Datasets/CW_CAIT_GHG.csv")
print(df1.head())

# Change directory up so I need to type the folder
os.chdir(repo_path)

# Method 2
df2 = pd.read_csv("0091_w3/Datasets/economic indicators.csv", encoding = "cp1252")
print(df2.head(3))

# Method 3
path = os.path.join(repo_path, "0091_w3/Datasets/GEO.csv")
df3 = pd.read_csv(path)
print(df3)