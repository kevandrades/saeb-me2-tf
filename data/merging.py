# %%
from os import chdir, getcwd
if getcwd()[-4:] != 'data':
    chdir('./data')

# %%
import pandas as pd

# %%
saeb = (pd.concat([
    pd.read_csv('190015853.csv'), # reading data...
    pd.read_csv('190127180.csv'),
    pd.read_csv('190029498.csv')
])
    .drop_duplicates() # ommiting duplicates
    .dropna() # omitting nas
)

#%%
saeb['ESC_MAE'].replace({
    'a [48]\.ª série\/' : 'o '
},
    regex = True,
    inplace = True)

#%%
saeb.to_csv('saeb.csv', encoding='utf-8')
