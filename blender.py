'''
This module is designed to operate on a folder of machine learning "raw
results" in the form of numpy array files.

It will compare each of the results to a validation data set and use linear
regression to give each result a weight.  A weighted average will be taken, and
the result will be saved as a "blended result"
'''


import numpy as np
import os
import pprint


def get_paths(rootdir):
    '''
    Given a path to a directory, returns a list of paths to all the files in
    that directory as strings.
    '''
    # returned object
    filepaths = []
    # walk through the directory tree
    for root, directories, files in os.walk(rootdir):
        for filename in files:
            # join strings to get a complete path
            filepath = os.path.join(root, filename)
            filepaths.append(filepath)
    # return the list of paths
    return filepaths


def load_data(filepath):
    '''
    Given a filepath to a raw data file, returns the np.array it contains.
    '''
    return np.load(filepath)


if __name__ == '__main__':

    rootdir = './raw_results'      # directory that stores all the raw results
    filepaths = get_paths(rootdir) # get a list of all the raw result filepaths
    numrows = len(filepaths)       # get the number of models being blended
    numcols = 2749898              # const # of ratings calculated by each model
    
    # initialize result matrix with zeros
    result_matrix = np.zeros(shape=(numrows, numcols))

    # populate result matrix
    for i, filepath in enumerate(filepaths):
        result_matrix[i] = load_data(filepath)

    # initialize validation array with zeros
    valid_array = np.zeros((1, numcols))
    
    # populate validation array
    with open('./data/sliced_data/hidden.dta') as f:
        i = 0
        for line in f:
            rating = int(line.split()[3])
            valid_array[0][i] = rating
            i += 1

    # debugging
    print result_matrix.shape
    print valid_array.shape