import sys
sys.path.append('/media/phenry/Data/Stored Documents/Schoolwork Archives/03 Junior Year/Term 3/CS-EE 156B/andykeller')
from nflix_io import *


def run_model(training_path, saving_paths):
    '''
    Takes a list of this form, with each object being string containing a filepath:
        [training_data, (valid_data, valid_results), (qual_data, qual_results)]
    
    '''

    for paths in saving_paths:
        datapath, savepath = paths
        data = load_slice(datapath)
        shape = data.shape

        # initialize result horizontal vector
        raw = np.zeros(shape[0])

        # save threes in every space in result
        for i in range(shape[0]):
            raw[i] = 3

        # save result as file
        np.save(savepath, raw)


if __name__ == '__main__':

    training_path = 'data/sliced_data/base.dta'
    saving_paths = [('data/sliced_data/hidden.dta', 'models/threes/validation.npy'),
                    ('data/sliced_data/qual.dta', 'models/threes/raw.npy')]

    run_model(training_path, saving_paths)


# if __name__ == '__main__':

#     DATAPATH = 'data/sliced_data/qual.dta'
#     data = load_slice(DATAPATH)

#     # get the number of rows from the data file
#     data_rows = data.shape[0]

#     # initialize the raw output (horizontal vector)
#     raw = np.zeros(data_rows)

#     # put threes in every entry in the raw output
#     for i in range(data_rows):
#         raw[i] = 3

#     # save the raw output
#     np.save('models/threes/raw_result', raw)