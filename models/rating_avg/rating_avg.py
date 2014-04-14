import sys
sys.path.append('/media/phenry/Data/Stored Documents/Schoolwork Archives/03 Junior Year/Term 3/CS-EE 156B/andykeller')
from nflix_io import *


if __name__ == '__main__':

    DATAPATH = './data/sliced_data/base.dta'
    data = load_slice(DATAPATH)
    
    #TODO