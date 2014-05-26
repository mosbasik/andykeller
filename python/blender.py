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
import time


def get_paths(path):
    '''
    Given a path to a parent directory, returns two lists: all the validation
    result files and all the raw result files (both *.dta files) in that
    directory tree as a list of path strings.
    '''
    # saves the results of this linux find command as a string, then parses the
    # string to a list of paths.
    syscall_output = subprocess.check_output(['find', path, '-iname', '*hidden.dta'])
    validationpaths = syscall_output[:-1].split('\n')

    # saves the results of this linux find command as a string, then parses the
    # string to a list of paths.
    syscall_output = subprocess.check_output(['find', path, '-iname', '*qual.dta'])
    rawpaths = syscall_output[:-1].split('\n')

    return (validationpaths, rawpaths)


def save_blending_result(result_array):
    '''
    '''
    print 'save_blending_result has been called'
    filename = '/shared/out/blender/blended_' + str(time.time()) + '.dta'
    with open(filename, 'w') as f:
        for rating in result_array:
            if rating > 5:
                print '5 written'
                f.write('5\n')
            elif rating < 1:
                print '1 written'
                f.write('1\n')
            else:
                print 'rating written'
                f.write(str(rating) + '\n')


if __name__ == '__main__':
    resultsdir = '/shared/out'  # parent directory of all results
    #validationpaths, rawpaths = get_paths(resultsdir)  # get filepaths

    # VALIDATION: CALCULATION OF WEIGHT VECTOR
    # ----------------------------------------

    # calculate necessary size for the matrix V which contains all validation
    # results, initialize it with zeros, and populate it
    #numrows = len(validationpaths)
    #numcols = np.loadtxt(validationpaths[0], unpack=True, comments='%').shape[0]

    #print 'number of validation paths: ' + str(numrows)
    #print 'number of rows in each validation file: ' + str(numcols)

    #V = np.zeros((numrows, numcols))
    #for i, validationpath in enumerate(validationpaths):
    #    V[i] = np.loadtxt(validationpath, unpack=True, comments='%')
    V = np.array([[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
                  [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
                  #[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0],
                  #[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
                  [2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0],
                  [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]])
    V = V.T

    print 'Information of V (validation result matrix)'
    print V.shape
    print V
    print ''

    # save ratings of the validation set ("hidden") in vector S
    #S = np.loadtxt('/shared/data/hidden.dta', dtype=int, unpack=True)[3]
    S = np.array([1, 2, 3, 4, 5, 6, 7, 8])

    print 'Information of S (rating vector of validation records)'
    print S.shape
    print S
    print ''

    # find weight vector X using least-squares linear regression
    regression = scipy.linalg.lstsq(V, S, check_finite=False)
    X = regression[0]

    print 'Information of X (blending weights vector)'
    print X.shape
    print X
    print ''


    # RESULTS: CREATION OF RESULT VECTOR
    # ----------------------------------

    # calculate necessary size for the matrix R which contains all raw results,
    # initialize it with zeros, and populate it
    #numrows = len(rawpaths)
    #numcols = np.loadtxt(rawpaths[0], unpack=True, comments='%').shape[0]

    #R = np.zeros((numrows, numcols))
    #for i, rawpath in enumerate(rawpaths):
    #    R[i] = np.loadtxt(rawpath, unpack=True, comments='%')
    R = np.array([[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
                  [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
                  #[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
                  [2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0],
                  [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]])

    print 'Information of R (raw results matrix)'
    print R.shape
    print R
    print ''

    for i in range(R.shape[0]):
        R[i] = np.multiply(R[i], X[i])

    print 'Information of R after multipying by weights vector'
    print R.shape
    print R
    print ''

    #R = np.mean(R, axis=0)
    R = np.sum(R, axis=0)

    print 'Information of R after taking elementwise sum'
    print R.shape
    print R
    print ''

    #save_blending_result(R)