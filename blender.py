'''
This module is designed to operate on a folder of machine learning "raw
results" in the form of numpy array files.

It will compare each of the results to a validation data set and use linear
regression to give each result a weight.  A weighted average will be taken, and
the result will be saved as a "blended result"
'''


import numpy as np
import scipy.linalg
import os
import pprint
import subprocess


def get_paths(path):
    '''
    Given a path to a parent directory, returns two lists: all the validation
    result files and all the raw result files (both *.npy files) in that
    directory tree as a list of path strings.
    '''
    # saves the results of this linux find command as a string, then parses the
    # string to a list of paths.
    syscall_output = subprocess.check_output(['find', path, '-iname', 'validation.npy'])
    validationpaths = syscall_output[:-1].split('\n')

    # saves the results of this linux find command as a string, then parses the
    # string to a list of paths.
    syscall_output = subprocess.check_output(['find', path, '-iname', 'raw.npy'])
    rawpaths = syscall_output[:-1].split('\n')

    return (validationpaths, rawpaths)



if __name__ == '__main__':
    resultsdir = 'models'  # parent directory of all results
    validationpaths, rawpaths = get_paths(resultsdir)  # get filepaths

    # VALIDATION: CALCULATION OF WEIGHT VECTOR
    # ----------------------------------------

    # calculate necessary size for the matrix V which contains all validation
    # results, initialize it with zeros, and populate it
    numrows = len(validationpaths)
    numcols = np.load(validationpaths[0], 'r+').shape[0]
    V = np.zeros((numrows, numcols))
    for i, validtionpath in enumerate(validationpaths):
        V[i] = np.load(validtionpath, 'r+').T
    V = V.T

    # save ratings of the validation set ("hidden") in vector S
    S = np.loadtxt('data/sliced_data/hidden.dta', dtype=int, unpack=True)[3]

    # find weight vector X using least-squares linear regression
    regression = scipy.linalg.lstsq(V, S, check_finite=False)
    X = regression[0]


    # RESULTS: CREATION OF RESULT VECTOR
    # ----------------------------------

    # calculate necessary size for the matrix R which contains all raw results,
    # initialize it with zeros, and populate it
    numrows = len(rawpaths)
    numcols = np.load(rawpaths[0], 'r+').shape[0]
    R = np.zeros((numrows, numcols))
    for i, rawpath in enumerate(rawpaths):
        R[i] = np.load(rawpath, 'r+').T

    print R.shape
    print R

    for i in range(R.shape[0]):
        R[i] = np.multiply(R[i], X[i])

    print R.shape
    print R

    R = np.mean(R, axis=0)

    print R.shape
    print R