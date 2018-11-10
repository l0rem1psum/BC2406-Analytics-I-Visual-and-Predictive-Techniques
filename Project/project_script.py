import pandas as pd
import seaborn as sns
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import re

from scipy import stats
from functools import reduce

matplotlib.style.use("ggplot")

pd.set_option('display.max_colwidth', -1)
pd.options.display.max_columns = 100 
pd.options.display.max_rows = 10000 

sns.set_style("whitegrid")

hr = pd.read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
print(hr.shape)
before_dedup = hr.shape[0]
hr.describe(include='all')

hr.head()