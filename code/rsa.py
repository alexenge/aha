from mne import read_epochs
import numpy as np
import mne_rsa
import pandas as pd

epochs = read_epochs("../data/preprocessed/sub-01_epo.fif")

epochs_part = epochs["part == 'II'"].pick_types(eeg=True)

# # Sort items
# item_idxs = np.array(epochs_part.metadata["item_id"])
# dat_part = dat_part[item_idxs - 1]


behav_file = "../data/raw/rt/exp1/VP_01_Tag1.txt"
behav = pd.read_csv(behav_file, delimiter="\t", usecols=range(14), index_col=False)
behav_part = behav[behav["Wdh"] == 212]

response = behav_part["Tastencode"]

dsm_model = mne_rsa.compute_dsm(response, metric="sqeuclidean")
dsm_model.shape

item_ids = epochs_part.metadata["item_id"]
epochs_part.events[:, 2] = item_ids
epochs_part.events_id = dict(zip(item_ids, item_ids))

rsa = mne_rsa.rsa_epochs(epochs_part, dsm_model, spatial_radius=None)

rsa.plot()
