# read metadata about companies from Google spreadsheet and write to local file

import requests
import pandas as pd

SHEET_ID='1_aQKhRUhpMRHbNdz22ysY8wIdQ-93EJHmk_Vyb04HZE'

r = requests.get(f'https://docs.google.com/spreadsheet/ccc?key={SHEET_ID}&output=csv')
open('dataset.csv', 'wb').write(r.content)
df = pd.read_csv('dataset.csv')
