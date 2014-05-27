#import sys
#sys.path.append('/shared/andykeller')
#from nflix_io import *
from sklean.neighbors import KNeighborsRegressor
import numpy as np

def run_model(base_path, hidden_path, qual_path):
    '''
    '''
    user, movie, rating = np.loadtxt(base_path,
                                     unpack=True,
                                     comments='%',
                                     usecols=(0, 1, 2))
    print '01/13 base.dta loaded'

    X = np.array(zip(user, movie))
    print '02/13 user and movie zipped to create X'

    knn = KNeighborsRegressor(n_neighbors=5,
                              weights='uniform',
                              algorithm='kd_tree',
                              leaf_size=30,
                              p=2,
                              metric='minkowski')
    print '03/13 regressor created'

    knn.fit(X, rating)
    print '04/13 fitting completed'

    user = None
    move = None
    rating = None
    X = None
    print '05/13 variables cleared'

    user, movie = np.loadtxt(hidden_path,
                             unpack=True,
                             comments='%',
                             usecols=(0, 1))
    print '06/13 hidden.dta loaded'

    hidden = np.array(zip(user, movie))
    print '07/13 user and movie zipped to create hidden'

    hidden_result = knn.predict(hidden)
    hidden = None
    print '08/13 hidden_result calculated, hidden cleared'

    np.savetxt('/shared/out/k-NN/k-NN_hidden.dta',
               hidden_result)
    hidden_result = None
    print '09/13 hidden_result saved to disk, hidden_result cleared'

    user, movie = np.loadtxt(qual_path,
                             unpack=True,
                             comments='%',
                             usecols=(0, 1))
    print '10/13 qual.dta loaded'

    qual = np.array(zip(user, movie))
    print '11/13 user and movie zipped to create qual'

    qual_result = knn.predict(qual)
    qual = None
    print '12/13 qual_result calculated, qual cleared'

    np.savetxt('/shared/out/k-NN/k-NN_qual.dta',
               qual_result)
    qual_result = None
    print '13/13 qual_result saved to disk, qual_result cleared'


if __name__ == '__main__':
    run_model('/shared/data/base.dta',
              '/shared/data/hidden.dta',
              '/shared/data/qual.dta')