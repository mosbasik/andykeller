#import sys
#sys.path.append('/shared/andykeller/python')
#from nflix_io import *
from sklearn.neighbors import KNeighborsRegressor
import numpy as np
import h5py
import os

def make_dense_matrix(U_path, V_path):
    '''
    Given paths to user features and movie features, creates a dense h5py file 
    with the euclidean distances 
    '''
    with h5py.File(U_path, 'r') as U_h5py:
        U = U_h5py['base_U.h5py']
        with h5py.File(V_path, 'r') as V_h5py:
            V = V_h5py['base_V.h5py']
            dense_shape = (U.shape[0], V.shape[0])
            with h5py.File(os.path.dirname(U_path) + '/dense.h5py', 'w') as dense_h5py:
                dense_dset = dense_h5py.create_dataset('dense.h5py',
                                                        dense_shape,
                                                        dtype='float')
                for i, user in enumerate(U):
                    for j, movie in enumerate(V):
                        distance = np.linalg.norm(np.array(user) - np.array(movie))
                        dense_dset[i,j] = distance


def run_model(U_path, V_path, hidden_path, qual_path, force_update=False):
    '''
    '''

    # if force_update:
    #     make_dense_matrix(U_path, V_path)

    # assert os.path.exists(os.path.dirname(U_path) + '/dense.h5py')

    knn = KNeighborsRegressor(n_neighbors=5,
                              weights='uniform',
                              algorithm='kd_tree',
                              leaf_size=30,
                              p=2,
                              metric='minkowski')
    #print '03/13 regressor created'

    with h5py.File(os.path.dirname(U_path) + '/base_U.h5py', 'r') as U_h5py:
        U = U_h5py['base_U.h5py']

        for i in range(10):
            print U[i]

        assert false

        knn.fit(U, rating)
        #print '04/13 fitting completed'

    user = None
    move = None
    rating = None
    X = None
    #print '05/13 variables cleared'

    user, movie = np.loadtxt(hidden_path,
                             unpack=True,
                             comments='%',
                             usecols=(0, 1))
    #print '06/13 hidden.dta loaded'

    hidden = np.array(zip(user, movie))
    #print '07/13 user and movie zipped to create hidden'

    hidden_result = knn.predict(hidden)
    hidden = None
    #print '08/13 hidden_result calculated, hidden cleared'

    np.savetxt('/shared/out/k-NN/k-NN_hidden.dta',
               hidden_result)
    hidden_result = None
    #print '09/13 hidden_result saved to disk, hidden_result cleared'

    user, movie = np.loadtxt(qual_path,
                             unpack=True,
                             comments='%',
                             usecols=(0, 1))
    #print '10/13 qual.dta loaded'

    qual = np.array(zip(user, movie))
    #print '11/13 user and movie zipped to create qual'

    qual_result = knn.predict(qual)
    qual = None
    #print '12/13 qual_result calculated, qual cleared'

    np.savetxt('/shared/out/k-NN/k-NN_qual.dta',
               qual_result)
    qual_result = None
    #print '13/13 qual_result saved to disk, qual_result cleared'


if __name__ == '__main__':
    run_model('/shared/data/base.dta',
              '/shared/data/hidden.dta',
              '/shared/data/qual.dta')