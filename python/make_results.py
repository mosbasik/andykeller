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
                      help="Output destination for ratings file.", metavar="RFILE")
    
    (options, args) = parser.parse_args()
    
    U = load_mm(options.user_features)
    V = load_mm(options.item_features)
    
    print "U:"
    print U.size
    print U.shape

    print "V:"
    print V.size
    print V.shape

    R = U * V.T

    print "R:"
    print R.size
    print R.shape    
