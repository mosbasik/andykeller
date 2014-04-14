
import numpy as np
from scipy.sparse import csr_matrix


def slice(index):
    '''
    Given an integer index between 1 and 5, returns as an np.array the
    corresponding slice of data from the main .npz archive.
    '''
    with np.load('nflix.npz') as f:
        ratings = f['ratings']
        dates = f['dates']
        dsets = f['dsets']
        indices = f['indices']
        indptr = f['indptr']

    #[e for i,e in enumerate()]


if __name__ == '__main__':
    print 'No main specified at this time'