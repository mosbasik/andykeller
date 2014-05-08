import sys
sys.path.append('/shared/andykeller/python')
from nflix_io import *


if __name__ == '__main__':
    U = load_mm("/shared/out/als/base_U.mm")
    V = load_mm("/shared/out/als/base_V.mm")

    print "U:"
    print U.size
    print U.shape

    print "V:"
    print V.size
    print V.shape

    R = np.multiply(U, V.T)

    print "R:"
    print R.size
    print R.shape