import sys
sys.path.append('/shared/andykeller/python')
from nflix_io import *


if __name__ == '__main__':
    U = load_mm("/shared/out/als/base_U.mm")
    V = load_mm("/shared/out/als/base_V.mm")

    #U = np.zeros((5, 2))
    #V = np.zeros((10, 2))

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

    R = np.dot(U, V.T)

    print "R:"
    print R
    print type(R)
    print R.size
    print R.shape
    print ''