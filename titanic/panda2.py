#!/usr/bin/python

import pandas as pd

train_data = pd.read_csv('train.csv')

# temp data
sex_serie = train_data.Sex.unique()
class_serie = train_data.Pclass.unique()
faregroups = [0,1,2,3]

# compute means
means_data = pd.DataFrame(columns = ['Sex','Pclass', 'FareGroup', 'Mean'])
for s in sex_serie:
    for c in class_serie:
        for f in faregroups:
            minFareInGroup = f * 10
            maxFareInGroup = (f+1) * 10
            if f == 3 : maxFareInGroup = 1000
            means_data.loc[means_data.size] = [\
                s, \
                c, \
                f, \
                train_data[ \
                    (train_data.Sex == s) \
                    & (train_data.Pclass == c) & \
                    (train_data.Fare >= minFareInGroup) & \
                    (train_data.Fare < maxFareInGroup) \
                ].Survived.mean() \
            ]

# add column on test_data
test_data = pd.read_csv('test.csv')
test_data.insert(test_data.columns.size, 'Survived', 0)

for i,row in means_data.iterrows():
    if row.Mean >= .5:
        minFareInGroup = row.FareGroup * 10
        maxFareInGroup = (row.FareGroup+1) * 10
        test_data.loc[(test_data.Sex == row.Sex) & (test_data.Pclass == row.Pclass) & (test_data.Fare >= minFareInGroup) & (test_data.Fare < maxFareInGroup), 'Survived'] = 1

# apply
response_data = pd.DataFrame(columns = ['PassengerId','Survived'])

response_data.PassengerId = test_data.PassengerId
response_data.Survived = test_data.Survived

response_data.to_csv('panda2.out', index = False)
