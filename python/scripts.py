import os
import numpy as np

def dta_top_Q_users(dta_path, Q):
    '''
    Creates a new version of the specified dta file containing only entries of
    the top Q users.
    '''

    path = os.path.dirname(filepath)
    basename = os.path.basename(filepath)
    plainname = '.'.join(basename.split('.')[:-1])
    
    dta = np.loadtxt(dta_path, dtype=float) # load up the file

    dta_counts = np.zeros((dta.shape[0],))  # make counts array full of zeros

    for point in dta:
        dta_counts[point[0]] += 1           # add 1 to bin for every occurance

    # http://stackoverflow.com/questions/6910641/how-to-get-indices-of-n-maximum-values-in-a-numpy-array
    dta_indices = np.argpartition(dta_counts, -Q)[-Q:] # array of dta_count indices containing topQ user ids

    dta_topQusers = dta_counts[dta_indices]     # array of topQ user ids

    topQ = np.zeros((Q, dta[0].length))     # make topQ array full of zeros

    for i, point in enumerate(dta):         # loop through all points in dta
        if point[0] in dta_topQusers:       # if the user is a top Q user
            topQ[i] = point                 # save that point into topQ

    np.savetext(path + '/' + plainname + '_top' + Q + 'users.dta', topQ)