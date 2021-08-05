import numpy as np
import pandas as pd

# Read evoked *power* for all subjects
subject_id = ["sub-" + "{:02d}".format(sid) for sid in range(1, 49)]
fname_evo_tfr = ["../data/preprocessed/" + sub + "_tfr-ave.csv" for sub in subject_id]
evo_tfr = [pd.read_csv(fname) for fname in fname_evo_tfr]
evo_tfr = pd.concat(evo_tfr)

# Compute difference in power between conditions
id_vars = ["part", "condition", "subject_id", "time", "freq"]
electrodes = [col for col in evo_tfr.columns if (col not in id_vars)]
evo_tfr_diff = (
    evo_tfr.query("condition == 'Informed'")[electrodes]
    - evo_tfr.query("condition == 'Naive'")[electrodes].values
)
evo_tfr_diff[id_vars] = evo_tfr.query("condition == 'Informed'")[id_vars]

# Compute flat average across conditions
id_vars.remove("condition")
evo_tfr_flat = evo_tfr.groupby(id_vars, as_index=False).mean()

# Permutation test on the difference between conditions
import numpy as np
from mne import read_epochs
from mne.channels import find_ch_adjacency
from mne.stats import permutation_cluster_1samp_test
epochs = read_epochs("../data/preprocessed/sub-01_epo.fif", preload=False)
adjacency, ch_names = find_ch_adjacency(epochs.info, ch_type="eeg")

# # Average frequencies in the lower gamma range
# tmp = evo_tfr_diff[evo_tfr_diff["part"] == "II"].drop(["part", "condition"], axis=1)
# tmp = tmp[(tmp["time"] >= 200) & (tmp["time"] <= 400)]
# tmp = tmp[(tmp["freq"] >= 34) & (tmp["freq"] <= 48)]
# id_vars = ["subject_id", "time"]
# tmp = tmp.groupby(id_vars, as_index=False).mean().drop("freq", axis=1)
# tmp = tmp.set_index(["subject_id", "time"])

# m, n = len(tmp.index.levels[0]), len(tmp.index.levels[1])
# X = tmp.values.reshape(m, n, -1)

# cluster_stats = spatio_temporal_cluster_1samp_test(X, n_permutations = 1000, adjacency=adjacency)
# t_obs, clusters, p_values, _ = cluster_stats
# good_cluster_inds = np.where(p_values < 0.05)[0]


# Subset
tmp = evo_tfr_diff[evo_tfr_diff["part"] == "III"].drop(["part", "condition"], axis=1)
tmp = tmp[tmp["subject_id"] != "sub-18"]
tmp = tmp[(tmp["time"] >= -200) & (tmp["time"] <= 800)]

id_vars = ["subject_id", "time", "freq"]
tmp = tmp.groupby(id_vars, as_index=False).mean()
tmp = tmp.set_index(["subject_id", "time", "freq"])

m, n, o = len(tmp.index.levels[0]), len(tmp.index.levels[1]), len(tmp.index.levels[2])
X = tmp.values.reshape(m, n, o, -1)

import matplotlib.pyplot as plt
from mne.stats import combine_adjacency
from scipy import sparse

# # plot each subject
# for sid in range(48):
#     subject_id = "sub-" + str(sid + 1)
#     print(subject_id)
#     plt.imshow(X.mean(axis=3)[sid, :, :].T, aspect="auto", vmin=0, vmax=20)
#     plt.xlabel("time (ms)")
#     plt.ylabel("freq (Hz)")
#     plt.xticks(ticks=np.arange(0, 401, 100), labels=np.arange(0, 801, 200))
#     plt.show()

# plot subject average
plt.imshow(X.mean(axis=(0, 3)).T, aspect="auto", vmin=-0.5, vmax=0.5); plt.show()

adjacency_freq = sparse.csr_matrix(np.ones((24, 24)))
adjacency_chan, ch_names = find_ch_adjacency(epochs.info, ch_type="eeg")
adjacency = combine_adjacency(24, adjacency_chan)
plt.imshow(adjacency.toarray(), cmap='gray', origin='lower', interpolation='nearest')

# adjacency_chan, ch_names = read_ch_adjacency("easycapM1")
# picks = [ch_names.index(ch) for ch in epochs.ch_names if ch in ch_names]
# adjacency_chan, ch_names = read_ch_adjacency("easycapM1", picks = picks)
# adjacency_freq = sparse.csr_matrix(np.zeros((24, 24)))
# adjacency = combine_adjacency(adjacency_freq, adjacency_chan)

cluster_stats = permutation_cluster_1samp_test(X, adjacency=adjacency)
t_obs, clusters, p_values, _ = cluster_stats

# Plot a single cluster
cl_ind = int(np.where(p_values == p_values.min())[0])
time_inds, freq_inds, space_inds = np.squeeze(clusters[cl_ind])
for ch in range(62):
    print(ch_names[ch])
    plt.figure()
    plt.imshow(t_obs[:, :, ch].T, aspect="auto", vmin=-4, vmax=4)
    ind_in_cluster = np.where(space_inds == ch)
    plt.scatter(time_inds[ind_in_cluster], freq_inds[ind_in_cluster], color="red", marker="o")
    plt.show()
