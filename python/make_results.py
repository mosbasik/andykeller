import sys
sys.path.append('/shared/andykeller/python')
from nflix_io import *

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
                      help="Output directory for ratings file.", metavar="RFILE")
    
    (options, args) = parser.parse_args()
    
    # Get the data from the output of graphchi
    U = load_mm(options.user_features)
    V = load_mm(options.item_features)

    # Transform from arrays to matrices
    U = np.matrix(U)
    V = np.matrix(V)
    
    print "U:"
    print U
    print type(U)
    print U.size
    print U.shape
    print ''

    print "V:"
    print V
    print type(V)
    print V.size
    print V.shape
    print ''

    R = U * V.T

    print "R:"
    print R
    print type(R)
    print R.size
    print R.shape
    print ''