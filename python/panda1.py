#!/usr/bin/python

import pandas as pd

test_data = pd.read_csv('test.csv')

response_data = pd.DataFrame(columns = ['PassengerId','Survived'])
response_data.loc[:,'PassengerId'] = test_data.PassengerId
response_data.loc[:,'Survived'] = 0
response_data.loc[test_data.Sex == 'female','Survived'] = 1

response_data.to_csv('panda1.out', index = False)
