# -*- coding: utf-8 -*-
"""
Created on Wed Oct  4 20:14:25 2023

@author: Jichu
"""

import pandas as pd
import matplotlib.pyplot as plt

df0 = pd.read_csv('Estonia_PPP.csv')

fig, ax = plt.subplots()
fig.set_size_inches(10, 6.5)


df0.plot(x='Year', y='Estonia worldBank/NY_GDP_PCAP_KD', ax=ax)

plt.xlabel('Year')
plt.ylabel('Dollars, 2010 Equivelant')
plt.title('PPP Estonia 1995-2022')
plt.savefig('PPP Estonia.png')
