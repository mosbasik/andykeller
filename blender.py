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
    Given a filepath to a raw data file, loads the data into memory.
    '''
    #with np.load(filepath) as f:
        #TODO


if __name__ == '__main__':

    # directory that stores all the raw results
    rootdir = './raw_results'

    # list of all result file paths
    filepaths = get_paths(rootdir)
    
    for filepath in filepaths:
        #TODO