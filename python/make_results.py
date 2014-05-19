import sys
import os.path
sys.path.append('/shared/andykeller/python')
from nflix_io import *
import h5py


def get_qual_user_feature_vectors(u_array):
    '''
    Given a 2D np.array where each row is a feature vector corresponding to a 
    specific user, return an analogous array that only includes users used in
    the qual data set.
    '''

    # Check existance of file containing unique ids of all qual users.  If it
    # doesn't exist; we'll make one.
    if not os.path.isfile('/shared/data/qual_users.npy'):
        qual_users = np.loadtxt('/shared/data/qual.dta', dtype=int, usecols=(0,), unpack=True)
        qual_users = np.unique(qual_users)
        qual_users = qual_users - 1
        np.save('/shared/data/qual_users.npy', qual_users)

    # Get the array
    qual_users = np.load('/shared/data/qual_users.npy')

    print 'qual_users:'
    print qual_users
    print qual_users.size
    print qual_users.shape
    print ''

    # Select corresponding rows from u_array
    qual_users_features = u_array[qual_users]

    print 'qual_users_features:'
    print qual_users_features
    print qual_users_features.size
    print qual_users_features.shape
    print ''

    return qual_users_features



if __name__ == "__main__":
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("-u", "--user_features",
                      help="Location of the file containing the features for each \
                      user. Should be in MM format.", metavar="UFILE")
    
    parser.add_option("-v", "--item_features",
                      help="Location of the file containing the features for each \
                      item. Should be in MM format.", metavar="VFILE")
    
    parser.add_option("-o", "--output_file",
                      help="Output destination for ratings file.", metavar="RFILE")
    
    (options, args) = parser.parse_args()
    
    load_mm(options.user_features)
    
    # Get the data from the output of graphchi
    #U = load_mm(options.user_features)
    #V = load_mm(options.item_features)

    # Filter out users not needed by qual
    #U = get_qual_user_feature_vectors(U)

    # Transform from arrays to matrices
    #U = np.matrix(U)
    #V = np.matrix(V)
    
    # print "U:"
    # print U
    # print type(U)
    # print U.size
    # print U.shape
    # print ''

    # print "V:"
    # print V
    # print type(V)
    # print V.size
    # print V.shape
    # print ''

    # R = U * V.T

    # print "R:"
    # print R
    # print type(R)
    # print R.size
    # print R.shape
    # print ''
