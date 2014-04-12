import os
import sys

import numpy as np
import scipy.io
import scipy.sparse


NUM_USERS = 458293
NUM_MOVIES = 17770

MAT_SIZE = (NUM_MOVIES, NUM_USERS)

MAT_ENTRY_T = np.dtype([('dset', np.int8),
                        ('rating', np.int8),
                        ('date', np.int16)])


"""
This mmap's a full matrix. This is roughly 32GB :O.

This should never have to be ran again.
"""
def convert_to_full_matrix(data_dir):
    # you need to create the file because mode is "r+" (prevents overwriting)
    all_data = np.memmap('full.npy', dtype=MAT_ENTRY_T, mode='r+', shape=MAT_SIZE)

    with open(os.path.join(data_dir, 'all.dta')) as f_data:
        with open(os.path.join(data_dir, 'all.idx')) as f_idx:
            i = 0
            for line in f_data:
                user, movie, date, rating = map(int, line.split(' '))
                dset = int(f_idx.readline())

                movie -= 1
                user -= 1

                all_data[movie, user] = (dset, rating, date)

                i += 1
                if i % 100000 == 0:
                    print i
                    all_data.flush()

"""
Writes sparse matrices to disc for dataset.

This should never have to be ran again.
"""
def full_to_sparse(data_dir):
    all_data = np.memmap('full.npy', dtype=MAT_ENTRY_T,
                         mode='r', shape=MAT_SIZE)

    # dset is never 0, so make indices and indptr out of that
    spmat = scipy.sparse.csr_matrix(all_data['dset'])

    print "done with sparse creation, saving backup"
    scipy.io.mmwrite("dsets_backup.mtx", spmat)
    print "done saving backup"

    del all_data

    dsets = spmat.data
    indices = spmat.indices
    indptr = spmat.indptr

    ratings = np.zeros(dsets.size, dtype=np.int8)
    dates = np.zeros(dsets.size, dtype=np.int16)

    print "iterating through file for ratings and dates"
    with open(os.path.join(data_dir, 'all.dta')) as f:
        for i, line in enumerate(f):
            user, movie, date, rating = map(int, line.split(' '))
            ratings[i] = rating
            dates[i] = date

    print "done iterating through file, about to do compressed save"
    save_me = {
        'dsets': dsets,
        'ratings': ratings,
        'dates': dates,
        'indices': indices,
        'indptr': indptr
    }
    np.savez_compressed('nflix.npz', **save_me)
    print "done"

"""
Returns sparse CSR matrices for (ratings, dates, datasets).

This function takes ~5s and uses about 700MB of memory.
"""
def load_data():
    with np.load('nflix.npz') as f:
        ratings = f['ratings']
        dates = f['dates']
        dsets = f['dsets']
        indices = f['indices']
        indptr = f['indptr']
    util = lambda x: csr_matrix((x, indices, indptr))

    return util(ratings), util(dates), util(dsets)


if __name__ == '__main__':
    print "No main specified at this time"
