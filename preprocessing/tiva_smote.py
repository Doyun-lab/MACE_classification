import pandas as pd

tiva_data = pd.read_csv('TIVA_for_SMOTE.csv', encoding='cp949')

data = tiva_data
x, y = data.iloc[:, :-1], data.iloc[:, -1]

from imblearn.over_sampling import SMOTE
from collections import Counter
from matplotlib import pyplot

# 필요 함수 정의
def count_and_plot(y): 
    counter = Counter(y)
    for k,v in counter.items():
        print('Class=%d, n=%d (%.3f%%)' % (k, v, v / len(y) * 100))
    pyplot.bar(counter.keys(), counter.values())
    pyplot.show()
    
X_resampled, y_resampled = SMOTE(random_state=0).fit_resample(x, y)
count_and_plot(y_resampled)

tiva_smote = X_resampled.assign(Class=y_resampled)

tiva_smote.to_csv("tiva_smote.csv")
