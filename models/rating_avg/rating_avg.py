import sys
sys.path.append('/media/phenry/Data/Stored Documents/Schoolwork Archives/03 Junior Year/Term 3/CS-EE 156B/andykeller')
from nflix_io import *


if __name__ == '__main__':

    # "LEARNING" PHASE

    DATAPATH = './data/sliced_data/base.dta'
    base = load_slice(DATAPATH)
    
    base_ratings = np.zeros(base.shape[0])
    
    for i, line in enumerate(base):
        base_ratings[i] = line[3]

    average = np.average(base_ratings)
    print "average rating is: %s" % average


    # SAVING PHASE

    DATAPATH = './data/sliced_data/qual.dta'
    qual = load_slice(DATAPATH)

    raw = np.zeros(qual.shape[0])

    for i in range(qual.shape[0]):
        raw[i] = average

    # save the raw output
    np.save('models/rating_avg/raw_result', raw)