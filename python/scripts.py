import os
import numpy as np

def dta_top_Q_users(dta_path, Q):
    '''
    Creates a new version of the specified dta file containing only entries of
    the top Q users.
    '''

    path = os.path.dirname(dta_path)
    basename = os.path.basename(dta_path)
    plainname = '.'.join(basename.split('.')[:-1])
    
    dta = np.loadtxt(dta_path, dtype=float) # load up the file

    dta_counts = np.zeros((dta.shape[0],))  # make counts array full of zeros

    for point in dta:
        dta_counts[point[0]] += 1           # add 1 to bin for every occurance

    # http://stackoverflow.com/questions/6910641/how-to-get-indices-of-n-maximum-values-in-a-numpy-array
    dta_indices = np.argpartition(dta_counts, -Q)[-Q:] # array of dta_count indices containing topQ user ids

    dta_topQusers = dta_counts[dta_indices]     # array of topQ user ids

    topQ = np.zeros((dta.length, dta[0].length))     # make topQ array full of zeros

    count = 0                               # count of points by topQ users

    for i, point in enumerate(dta):         # loop through all points in dta
        if point[0] in dta_topQusers:       # if the user is a top Q user
            topQ[i] = point                 # save that point into topQ
            count += 1                      # count is incremented

    topQshort = np.zeros((count, dta[0].length))

    for i, point in enumerate(topQ):
        if point[0] == 0:
            print 'breakpoint reached'
            break
        topQshort[i] = point


    np.savetext(path + '/' + plainname + '_top' + Q + 'users.dta', topQ)


def mm_to_h5py_svdpp(mm_path):
    '''
    Takes a string filepath of a feature vector file (.mm) of the form produced
    by svdpp and parses it.  Creates an h5py dataset of the same name, such that
    the rows represent movies and are of the form: {feature, feature, feature,
    ..., feature}.  Note that the h5py file will be half as wide.
    '''

    path = os.path.dirname(mm_path)
    basename = os.path.basename(mm_path)
    name = basename.split('.')[0]
    result_path = path + '/' + name + '.h5py'

    # load from matrix market file into numpy array, dropping header
    np_array = np.loadtxt(mm_path,
                          dtype=float,
                          comments="%",
                          skiprows=3,
                          ndmin=2)

    # save the shape of the array, and the shape if only features were included
    shape = np_array.shape
    feature_shape = (shape[0], shape[1]/2)

    # create new array full of zeros half as wide to save only the features
    np_features = np.zeros(feature_shape)

    # populate the new feature array
    for i, row in enumerate(np_features):
        for j, col in enumerate(np_features):
            np_features[i, j] = np_array[i, j]

    # create and initialize h5py file and dataset of feature shape
    with h5py.File(result_path, 'w') as f:
        dset = f.create_dataset(name + '.h5py',
                                feature_shape,
                                dtype="float",
                                data=np_features)

    print result_path + ' created'
    return result_path 