# Load modules
from mne import read_epochs, grand_average
from mne.channels import combine_channels, find_ch_adjacency
from mne.stats import spatio_temporal_cluster_test
from mne.time_frequency import read_tfrs, tfr_morlet
import numpy as np
import matplotlib.pyplot as plt

# Define frequencies and number of cycles
freqs = np.arange(6, 51, step=2)
# n_cycles = np.linspace(3, 10, num=len(freqs))
n_cycles = freqs / 2.

# Load epochs
epochs = read_epochs("data/processed/aha-unf-epo.fif")

# # Remove irrelevant conditions
# epochs = epochs["condition == ['Informed', 'Naive']"]

# Downsample and crop
epochs.resample(sfreq=250)
epochs.crop(tmin=-0.25, tmax=0.85)

# Define custom function to compute single trial power for eqch frequency
def compute_power(epochs, freq, n_cycles):
    tfr = tfr_morlet(
        epochs,
        freq,
        n_cycles,
        use_fft=True,
        return_itc=False,
        average=False
    )
    tfr.apply_baseline(baseline=(None, 0), mode="percent")
    return(np.float32(tfr.data))

# Apply to our epochs
tfr = [
    compute_power(epochs, [freq], n_cycles) for freq, n_cycles in zip(freqs, n_cycles)
]

# Combine into one big array
tfr = np.array(tfr, dtype="float32")

# Compute single trial power
tfr = tfr_morlet(epochs, freqs, n_cycles, use_fft=True, return_itc=False, average=False)
del epochs

# Reduce memory by changing the data type
tfr.data = np.float32(tfr.data)

# Backup
tfr.save("data/processed/aha-tfr.h5")

# Read preprocessed power
tfr = read_tfrs("data/processed/aha-tfr.h5")[0]

# Baseline correction
tfr.apply_baseline(baseline=(None, 0), mode="percent")

# Define helper function to extract averaged TFR data
def tfr_to_array(tfr, query, axis_flatten=(0, 1)):
    return(np.mean(tfr[query].data, axis=axis_flatten))

# Compute averages for each part
parts = ["I", "II", "III"]
avgs = [tfr_to_array(tfr, query="part == '" + part + "'") for part in parts]
avgs = dict(zip(parts, avgs))

# Compute differences between parts
diffs = {"II_I": avgs["II"] - avgs["I"], "III_I": avgs["III"] - avgs["I"]}

# Compute differences betwen conditions within each part
conds = [
    tfr_to_array(tfr, query="part == '" + part + "' & condition == 'Informed'")
    - tfr_to_array(tfr, query="part == '" + part + "' & condition == 'Naive'")
    for part in parts
]
conds = dict(zip(parts, conds))

# Plot
plt.imshow(avgs["II"], aspect='auto', extent=[-200, 800, 6, 50], vmin=-2, vmax=2, origin="lower"); plt.show()
plt.imshow(diffs["II_I"], aspect='auto', extent=[-200, 800, 6, 50], vmin=-0.5, vmax=0.5, origin="lower"); plt.show()
plt.imshow(conds["II"], aspect='auto', extent=[-200, 800, 6, 50], vmin=-2, vmax=2, origin="lower"); plt.show()

test = tfr.copy()
test.data = test["part == 'II' & condition == 'Informed'"].average().apply_baseline(baseline=(None, 0), mode="percent").data - test["part == 'II' & condition == 'Naive'"].average().apply_baseline(baseline=(None, 0), mode="percent").data

tor = tfr["part == 'II'"].data

np.where((tfr.times > 0.25) & (tfr.times < 0.35))

dat_inf = tfr["part == 'II' & condition == 'Informed'"].crop(tmin=0.25, tmax=0.35).data
dat_inf = np.mean(dat_inf[:, :, 7:9, :], axis=(0, 2, 3))
dat_nai = tfr["part == 'II' & condition == 'Naive'"].crop(tmin=0.25, tmax=0.35).data
dat_nai = np.mean(dat_nai[:, :, 7:9, :], axis=(0, 2, 3))
dat_diff = dat_inf - dat_nai

plot_topomap(dat_diff, tfr.info); plt.show()

########################################################################################

# # Get list of participant IDs
# ps = np.unique(epochs.metadata["participant"])

# Compute TFR difference for each part
#for pt in ["I", "II", "III"]:

    pt = "II"
    # Extract relevant conditions
    epochs_a = epochs["part == '" + pt + "' & condition == ['Inforemd', 'Naive']"]
    tfr = tfr_morlet(epochs_a, freqs, n_cycles, return_itc=False, average=False)
    
    
    
    epochs_i = epochs["part == '" + pt + "' & condition == 'Informed'"]
    epochs_n = epochs["part == '" + pt + "' & condition == 'Naive'"]

    # Compute by-participant condition averages for power and ITC
    tfrs = {condition : [] for condition in ["a", "i", "n", "d"]}
    for ix in range(len(ps)):
        query = "participant == '" + ps[ix] + "'"
        tfrs["a"].append(tfr_morlet(epochs_a[query], freqs, n_cycles, return_itc=False))
        tfrs["i"].append(tfr_morlet(epochs_i[query], freqs, n_cycles, return_itc=False))
        tfrs["n"].append(tfr_morlet(epochs_n[query], freqs, n_cycles, return_itc=False))
        
        tfrs["a"] = [tfr.apply_baseline(mode="mean", baseline=(None, 0)) for tfr in tfrs["a"]]
        tfrs["i"] = [tfr.apply_baseline(mode="mean", baseline=(None, 0)) for tfr in tfrs["i"]]
        tfrs["n"] = [tfr.apply_baseline(mode="mean", baseline=(None, 0)) for tfr in tfrs["n"]]
        
        tfrs["d"].append(tfrs["i"][ix] - tfrs["n"][ix])
    
    # # Baseline correction
    # tfrs["a"] = [tfrs["a"][p].apply_baseline(mode='ratio', baseline=(None, 0)) for p in ps]
    # tfrs["d"] = [tfrs["d"][p].apply_baseline(mode='ratio', baseline=(None, 0)) for p in ps]

    # Grand average across participants
    gavgs_a = grand_average(tfrs["a"])
    gavgs_d = grand_average(tfrs["d"])
    
    gavgs_a.apply_baseline(mode="ratio", baseline=(None, 0))
    gavgs_d.apply_baseline(mode="ratio", baseline=(None, 0))
    
    # Plot
    gavgs_d.plot_topo()
    




gavg.plot_topo(vmin=-5, vmax=5)

# Extract data for one specific frequency
ix_freq = 3
print(freqs[ix_freq], n_cycles[ix_freq])
tfrs_i_freq = [tfr.data[:, ix_freq, :] for tfr in tfrs_i]
tfrs_n_freq = [tfr.data[:, ix_freq, :] for tfr in tfrs_n]
tfrs_i_freq = np.array(tfrs_i_freq)
tfrs_n_freq = np.array(tfrs_n_freq)

# Convert to correct array shape
X1 = np.transpose(tfrs_i_freq, [0, 2, 1])
X2 = np.transpose(tfrs_n_freq, [0, 2, 1])
X = [X1, X2]

# Find channel adjacencies
adjacency, ch_names = find_ch_adjacency(epochs.info, ch_type="eeg")

# Comput CBPT
cluster_stats = spatio_temporal_cluster_test(X=X, adjacency=adjacency)

# Extract significant clusters
p_accept = 0.05
T_obs, clusters, p_values, _ = cluster_stats
good_cluster_inds = np.where(p_values < p_accept)[0]
