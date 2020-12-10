# %%
from os import chdir, getcwd, listdir
from re import findall
chdir(findall('.+saeb\-me2\-tf', getcwd())[0] + '/data')

# %%
import pandas as pd

#%%
files = ['samples/' + name for name in listdir('samples')]

# %%
saeb = (
    pd.concat(
        [pd.read_csv(file) for file in files]
    ) #
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
