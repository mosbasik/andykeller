import sys
sys.path.append('/media/phenry/Data/Stored Documents/Schoolwork Archives/03 Junior Year/Term 3/CS-EE 156B/andykeller')
from nflix_io import *


if __name__ == '__main__':

    DATAPATH = './data/sliced_data/qual.dta'
    data = load_slice(DATAPATH)

    # get the number of rows from the data file
    rows = data.shape[0]

    # initialize the raw output
    raw = np.zeros((rows, 1))

    # put threes in every entry in the raw output
    for i in range(rows):
        raw[i][0] = 3

    # save the raw output
    np.save('models/threes/raw_result', raw)