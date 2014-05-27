#import sys
#sys.path.append('/shared/andykeller/python')
#from nflix_io import *
from sklearn.neighbors import KNeighborsRegressor
from sklearn.neighbors import NearestNeighbors
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


def unit_vector(vector):
    """ Returns the unit vector of the vector.  """
    return vector / np.linalg.norm(vector)


def angle_between(v1, v2):
    """ Returns the angle in radians between vectors 'v1' and 'v2'::

            >>> angle_between((1, 0, 0), (0, 1, 0))
            1.5707963267948966
            >>> angle_between((1, 0, 0), (1, 0, 0))
            0.0
            >>> angle_between((1, 0, 0), (-1, 0, 0))
            3.141592653589793
    """
    v1_u = unit_vector(v1)
    v2_u = unit_vector(v2)
    angle = np.arccos(np.dot(v1_u, v2_u))
    if np.isnan(angle):
        if (v1_u == v2_u).all():
            return 0.0
        else:
            return np.pi
    return angle


def run_model(U_path, V_path, hidden_path, qual_path):
    '''
    '''
    N = 5

    with h5py.File(os.path.dirname(U_path) + '/base_U.h5py', 'r') as U_h5py:

        U = U_h5py['base_U.h5py']

        knn = KNeighborsRegressor(n_neighbors=5,
                                  weights='uniform',
                                  algorithm='kd_tree',
                                  leaf_size=30,
                                  p=2,
                                  metric='minkowski')

        neigh = NearestNeighbors(n_neighbors=N)

        neigh.fit(U)

        #points from base (only users and ratings)
        base_users, base_ratings = np.loadtxt('/shared/data/base.dta',    
                                              unpack=True,
                                              comments='%',
                                              usecols=(0,3))

        #points from hidden (only users and ratings)
        hidd_users, hidd_ratings = np.loadtxt('/shared/data/hidden.dta',    
                                              unpack=True,
                                              comments='%',
                                              usecols=(0,3))

        #U-indices of the k neighbors of a hidden-point (or uids)
        foo = np.zeros((hidd_users[0], N))

        for i, point in enumerate(hidd_users):
            _, kneigh = neigh.kneighbors(U[point])
            foo[i] = kneigh

        #averaged ratings of the k neighbors
        bar = np.zeros((hidd_users[0],))
        for i, neighbors in enumerate(foo):
            bar[i] = np.mean[hidd_ratings[neighbors]]

        print bar

        np.savetxt(os.path.dirname(U_path) + '/k-NN_hidden.dta')


if __name__ == '__main__':
    run_model('/shared/out/sgd/base_U.mm',
              '/shared/out/sgd/base_V.mm',
              '/shared/data/hidden.dta',
              '/shared/data/qual.dta')