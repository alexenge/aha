# %%
import matplotlib.pyplot as plt
import numpy as np
from mne import read_epochs
from scipy.stats import sem, spearmanr

# %%
# General settings
tmin, tmax = (-0.2, 0.8)

# Prepare emtpy dictionaries
parts = ["I", "II", "III"]
rdms_eeg = {part: [] for part in parts}
rdms_response = {part: [] for part in parts}
corrs_eeg_response = {part: [] for part in parts}

combinations = [("I", "II"), ("I", "III"), ("II", "III")]
corrs_eeg_parts = {combination: [] for combination in combinations}

# Define helper function to correlate two RDMs
def correlate_rdms(rdm_1, rdm_2):
    n_items = rdm_1.shape[0]
    ixs_lower_triangle = np.tril_indices(n_items, -1)
    lower_triangles = [rdm[ixs_lower_triangle] for rdm in [rdm_1, rdm_2]]
    corr = spearmanr(*lower_triangles)[0]
    return corr


# %%
# Loop over subjects
n_subjects = 48
subject_id = ["sub-" + "{:02d}".format(n) for n in range(1, n_subjects + 1)]
for sid in subject_id:

    # Read epochs for the current subject
    fname = f"../data/preprocessed/{sid}_epo.fif"
    epochs_subject = read_epochs(fname, verbose=True)

    # Crop to time window of interest
    _ = epochs_subject.crop(tmin, tmax)

    # Loop over epochs for each part
    rdms_parts = {part: [] for part in parts}
    for part in parts:
        epochs = epochs_subject[f"part == '{part}'"]

        # Compute RDM from behavioral responses
        response = np.array(epochs.metadata["response"])
        n_items = len(response)
        rdm_response = np.zeros((n_items, n_items))
        for ix_item_1 in range(n_items):
            for ix_item_2 in range(n_items):
                dissimilarity = response[ix_item_1] - response[ix_item_2]
                rdm_response[ix_item_1, ix_item_2] = np.abs(dissimilarity)
        rdms_response[part].append(rdm_response)

        # Loop over timepoints in the ERP
        times = epochs.times
        rdms_times = []
        corrs_times_response = []
        for ix, time in enumerate(times):

            # Compute brain RDM for the current timepoint
            data_time = epochs.get_data()[:, :, ix]
            rdm_time = 1 - np.corrcoef(data_time)  # This is where the magic happens
            rdms_times.append(rdm_time)

            # Compute its correlation with the behavioral response
            corr_time_response = correlate_rdms(rdm_time, rdm_response)
            corrs_times_response.append(corr_time_response)

        # Convert lists to arrays
        rdms_times = np.array(rdms_times)
        corrs_times_response = np.array(corrs_times_response)

        # Append to lists for all subjects
        rdms_eeg[part].append(rdms_times)
        corrs_eeg_response[part].append(corrs_times_response)
        rdms_parts[part] = rdms_times

    # Compute brain correlations across parts
    n_combinations = len(combinations)
    n_times = len(times)
    for ix_combination, combination in enumerate(combinations):
        rdms_part_a = rdms_parts[combination[0]]
        rdms_part_b = rdms_parts[combination[1]]
        corrs = np.zeros((n_times, n_times))
        for ix_part_a, rdm_part_a in enumerate(rdms_part_a):
            for ix_part_b, rdm_part_b in enumerate(rdms_part_b):
                corrs[ix_part_a, ix_part_b] = correlate_rdms(rdm_part_a, rdm_part_b)
        corrs_eeg_parts[combination].append(corrs)

# %%
# Plot average correlations with behavior across subjects
plt.figure()
for part in parts:
    corrs = np.array(corrs_eeg_response[part])
    means = np.mean(corrs, axis=0)
    sems = sem(corrs, axis=0)
    plt.plot(times, means, label=f"Part {part}")
    plt.fill_between(times, means - sems, means + sems, alpha=0.3)
plt.axhline(y=0, color="black", linestyle="dashed")
plt.legend()
plt.show()

# %%
# Plot average correlations between parts across subjects
fig, axs = plt.subplots(nrows=1, ncols=n_combinations, figsize=(12, 4))
extent = [times[0], times[-1], times[0], times[-1]]
for ix, (combination, ax) in enumerate(zip(combinations, axs)):
    corrs = np.array(corrs_eeg_parts[combination])
    means = np.mean(corrs, axis=0)
    im = ax.imshow(means, origin="lower", vmin=-0.05, vmax=0.05, extent=extent)
    ax.set_xlabel(f"Part {combination[0]} (ms)")
    ax.set_ylabel(f"Part {combination[1]} (ms)")
fig.tight_layout()
fig.subplots_adjust(right=0.8)
cbar_ax = fig.add_axes([0.85, 0.15, 0.02, 0.7])
fig.colorbar(im, cax=cbar_ax, label="Correlation")
plt.show()

# %%
# Average RDMs within groups of subjects


# %%
# # Average RDMs across subjects
# rdms_eeg_group = {part: np.mean(rdms, axis=0) for part, rdms in rdms_eeg.items()}
# rdms_response_group = {
#     part: np.mean(rdms, axis=0) for part, rdms in rdms_response.items()
# }

# # Compute correlations from averaged RDMs
# corrs_response = {part: [] for part in parts}
# for part in parts:
#     for ix, time in enumerate(times):
#         rdm_eeg_group = rdms_eeg_group[part][ix]
#         rdm_response_group = rdms_response_group[part]
#         corr_time = correlate_rdms(rdm_eeg_group, rdm_response_group)
#         corrs_response[part].append(corr_time)

# # Plot
# fig = plt.figure()
# for part in parts:
#     plt.plot(times, corrs_response[part], label=f'Part {part}')
# plt.legend()
# plt.show()

# combinations = [("I", "II"), ("I", "III"), ("II", "III")]
# n_combinations = len(combinations)
# n_times = len(times)
# corrs_eeg_parts = np.zeros((n_combinations, n_times, n_times))

# for ix_combination, combination in enumerate(combinations):
#     rdms_part_a = rdms_eeg_group[combination[0]]
#     rdms_part_b = rdms_eeg_group[combination[1]]
#     for ix_part_a, rdm_part_a in enumerate(rdms_part_a):
#         for ix_part_b, rdm_part_b in enumerate(rdms_part_b):
#             corr = correlate_rdms(rdm_part_a, rdm_part_b)
#             corrs_eeg_parts[ix_combination, ix_part_a, ix_part_b] = corr

fig, axs = plt.subplots(nrows=1, ncols=n_combinations, figsize=(12, 4))
extent = [times[0], times[-1], times[0], times[-1]]
for ix, (combination, ax) in enumerate(zip(combinations, axs)):
    im = ax.imshow(corrs_parts[ix], origin="lower", vmin=-0.2, vmax=0.2, extent=extent)
    ax.set_xlabel(f"Part {combination[0]} (ms)")
    ax.set_ylabel(f"Part {combination[1]} (ms)")
fig.subplots_adjust(right=0.8)
cbar_ax = fig.add_axes([0.85, 0.15, 0.05, 0.7])
fig.colorbar(im, cax=cbar_ax, label="Correlation")
fig.savefig("combinations.pdf")
plt.show()
