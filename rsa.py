from natsort import natsorted
from glob import glob
import numpy as np
import matplotlib.pyplot as plt
import scipy
from mne import read_epochs


def rsa_spearman(rdm1, rdm2):
    lt_rdm1 = get_lower_triangular(rdm1)
    lt_rdm2 = get_lower_triangular(rdm2)
    return scipy.stats.spearmanr(lt_rdm1, lt_rdm2)[0]


def get_lower_triangular(rdm):
    num_conditions = rdm.shape[0]
    return rdm[np.tril_indices(num_conditions, -1)]


# Process each participant's epochs separately
epochs_files = sorted(glob("output/epochs/*/*_epo.fif"))
n_participants = len(epochs_files)
condition_rdms_list = []
erp_rdms_list = []
corrs_list = []
for participant_ix, epochs_file in enumerate(epochs_files):

    # Read data
    all_epochs = read_epochs(epochs_file)
    epochs = all_epochs['condition in ["Informed", "Naive"]']

    # Prepare empty model RDMs for item conditions
    phases = ["Pre-insight", "Insight", "Post-insight"]
    n_phases = len(phases)
    items = np.unique(epochs.metadata["item_id"])
    n_items = len(items)
    condition_rdms = np.zeros((n_phases, n_items, n_items))

    # Prepare empty neural RDMs for ERPs
    times = epochs.times
    n_times = len(times)
    erp_rdms = np.zeros((n_phases, n_times, n_items, n_items))

    # Prepare empty matrix for RSA results
    corrs = np.zeros((n_times, n_phases))

    # Fill RDMs separately for each phase of the epxeriment
    for phase_ix, phase in enumerate(phases):

        # Sort items so they are in the same order for all participants
        trials = epochs.metadata.query(f'phase == "{phase}"').reset_index()
        sort_ixs = np.argsort(np.array(trials["item_id"], dtype=int))
        items = np.array(trials.iloc[sort_ixs, ]["item_id"])
        item_conditions = np.array(trials.iloc[sort_ixs, ]["condition"])

        # Fill model RDM based on item conditions (same = 1, different = 0)
        for row_ix in sort_ixs:
            for col_ix in sort_ixs:
                if item_conditions[row_ix] == item_conditions[col_ix]:
                    condition_rdms[phase_ix, row_ix, col_ix] = 1
        condition_rdm = condition_rdms[phase_ix]

        # Fill neural RDM based on single trial ERP data
        erps = epochs[sort_ixs].pick_types(eeg=True).get_data()
        for time_ix in range(n_times):
            erp_rdm = 1 - np.corrcoef(erps[:, :, time_ix])
            erp_rdms[phase_ix, time_ix] = erp_rdm

            # # Plot neural RDM
            # plt.imshow(erp_rdm, vmin=0, vmax=2)
            # cbar = plt.colorbar()
            # plt.xlabel('Stimuli')
            # plt.ylabel('Stimuli')
            # cbar.ax.get_yaxis().labelpad = 15
            # cbar.ax.set_ylabel('1 - correlation', rotation=270)
            # plt.show()

            # Compare neural RDM and model RDM at the current time point
            corrs[time_ix, phase_ix] = rsa_spearman(condition_rdm, erp_rdm)

    # Add to lists across participants
    condition_rdms_list.append(condition_rdms)
    erp_rdms_list.append(erp_rdms)
    corrs_list.append(corrs)

# Average across participants and plot
corrs_grand = np.stack(corrs_list).mean(axis=0)
plt.plot(times, corrs_grand)
plt.axhline(y=0.0, color="k")
plt.legend(labels=phases)
