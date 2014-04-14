
import numpy as np
from scipy.sparse import csr_matrix


def data_slice(index):
    '''
    Given an integer index between 1 and 5, returns as an np.array the
    corresponding slice of data from the main .npz archive.

    THIS IS A TERRIBLE TERRIBLE FUNCTION THAT YOU SHOULD NOT RUN BECAUSE JOHNO
    HAS SLICED DATA FILES
    '''
    data_full = []
    data_slice = []
    index_full = []
    with open('./data/mu/all.dta') as f_dta:
        with open('./data/mu/all.idx') as f_idx:
            for d_line in f_dta:
                data_full.append(d_line)
            for i_line in f_idx:
                index_full.append(i_line)
    print len(data_full)
    print len(index_full)


    #[e for i,e in enumerate()]


if __name__ == '__main__':
    #print 'No main specified at this time'
    data_slice(0)