import sys
sys.path.append('/media/phenry/Data/Stored Documents/Schoolwork Archives/03 Junior Year/Term 3/CS-EE 156B/andykeller')
from nflix_io import *


def run_model(training_path, saving_paths):
    '''
    Takes a list of this form, with each object being string containing a filepath:
        [training_data, (valid_data, valid_results), (qual_data, qual_results)]
    
    '''

    # "LEARNING" PHASE
    # ----------------

    base = load_slice(training_path)
    base_shape = base.shape
    base_ratings = np.zeros(base_shape[0])
    for i, line in enumerate(base):
        base_ratings[i] = line[3]
    average = np.average(base_ratings)
    print "average rating of training set is: %s" % average

    # SAVING PHASE
    # ------------

    for paths in saving_paths:
        datapath, savepath = paths
        data = load_slice(datapath)
        shape = data.shape

        # initialize result horizontal vector
        raw = np.zeros(shape[0])

        # save threes in every space in result
        for i in range(shape[0]):
            raw[i] = average

        # save result as file
        np.save(savepath, raw)


if __name__ == '__main__':

    training_path = 'data/sliced_data/base.dta'
    saving_paths = [('data/sliced_data/hidden.dta', 'models/rating_avg/validation.npy'),
                    ('data/sliced_data/qual.dta', 'models/rating_avg/raw.npy')]

    run_model(training_path, saving_paths)








# if __name__ == '__main__':

#     # "LEARNING" PHASE

#     DATAPATH = './data/sliced_data/base.dta'
#     base = load_slice(DATAPATH)
    
#     base_ratings = np.zeros(base.shape[0])
    
#     for i, line in enumerate(base):
#         base_ratings[i] = line[3]

#     average = np.average(base_ratings)
#     print "average rating is: %s" % average


#     # SAVING PHASE

#     DATAPATH = './data/sliced_data/qual.dta'
#     qual = load_slice(DATAPATH)

#     raw = np.zeros(qual.shape[0])

#     for i in range(qual.shape[0]):
#         raw[i] = average

#     # save the raw output
#     np.save('models/rating_avg/raw_result', raw)