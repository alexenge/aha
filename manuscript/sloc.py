import os.path as op
import pickle

import mne
import numpy as np
from mne import VolSourceEstimate, compute_covariance, read_epochs
from mne.datasets import fetch_fsaverage
from mne.evoked import combine_evoked
from mne.minimum_norm import apply_inverse, make_inverse_operator

from preprocessing import _equalize_trial_counts

# Load preprocessed epochs for all subjects
subject_id = ["sub-" + "{:02d}".format(n) for n in range(1, 49)]
fnames_epo = [f"../data/preprocessed/{sid}_epo.fif" for sid in subject_id]
epochs_all_subjects = [read_epochs(fname, verbose=False) for fname in fnames_epo]

# Download standard MRI template and source/head model
fs_dir = fetch_fsaverage(verbose=False)
subjects_dir = op.dirname(fs_dir)
subject = "fsaverage"
trans = "fsaverage"  # MNE has a built-in fsaverage transformation
src = op.join(fs_dir, "bem", "fsaverage-ico-5-src.fif")
bem = op.join(fs_dir, "bem", "fsaverage-5120-5120-5120-bem-sol.fif")

# Compute standard forward solution for all subjects
info = epochs_all_subjects[0].info  # Info object is similar for all subjects

# Define common parameters for inverse solution
method = "dSPM"
snr = 3.0
lambda2 = 1.0 / snr ** 2

# Setup volumn source space
mri = op.join(fs_dir, "mri", "T1.mgz")
vol_src = mne.setup_volume_source_space(
    subject,
    mri=mri,
    pos=10.0,
    bem=bem,
    subjects_dir=subjects_dir,
    add_interpolator=True,
    verbose=True,
)
fwd = mne.make_forward_solution(info, trans, vol_src, bem, eeg=True, mindist=5.0)

# Setup empty dictionary to store the source time courses for all parts and conditions
parts = ["I", "II", "III"]
parts_dict = {}
for part in parts:
    parts_dict[part] = {}
    for condition in ["Informed", "Naive", "Combined", "Difference"]:
        parts_dict[part][condition] = []

# Loop overs subjects
for epochs in epochs_all_subjects:

    # Exclude any non-EEG channels
    epochs.pick_types(eeg=True)

    # Pick relevant conditions and channels
    epochs.pick_types(eeg=True)

    # Compute noise covariance matrix from pre-stimulus baseline
    noise_cov = compute_covariance(
        epochs,
        tmin=-0.2,
        tmax=0.0,
        method=["shrunk", "empirical"],
        rank=None,
        verbose=True,
    )

    # Equalize number of items per condition before averaging
    epochs = _equalize_trial_counts(
        epochs,
        condition_col="condition",
        item_col="item_id",
        drop_conditions=["Excl_informed", "Excl_naive", "Excl_known"],
    )

    # Compute inverse solution
    inverse_operator = make_inverse_operator(
        epochs.info, fwd, noise_cov, loose=1.0, depth=3.5
    )

    # Loop over parts
    for part in parts:

        # Select epochs for the current part
        epochs_part = epochs[f"part == '{part}'"]

        # Compute evoked potentials for each condition seperately
        evoked_dict = {}
        evoked_dict["Informed"] = epochs_part["condition == 'Informed'"].average()
        evoked_dict["Naive"] = epochs_part["condition == 'Naive'"].average()

        # Compute evoked potentials combined across conditions
        evoked_dict["Combined"] = combine_evoked(
            [evoked_dict["Informed"], evoked_dict["Naive"]], weights=[0.5, 0.5]
        )

        # Make sure to that all evokeds have an average reference
        evoked_dict = {
            key: value.set_eeg_reference("average", projection=True)
            for key, value in evoked_dict.items()
        }

        # Compute inverse solution for all evokeds
        stc_dict = {
            key: apply_inverse(
                value,
                inverse_operator,
                lambda2,
                method=method,
                pick_ori=None,
            )
            for key, value in evoked_dict.items()
        }

        # Compute difference in source time course between conditions
        data_informed = stc_dict["Informed"].data
        data_naive = stc_dict["Naive"].data
        stc_dict["Difference"] = stc_dict["Informed"]  # Just a copy
        stc_dict["Difference"].data = data_informed - data_naive

        # Append to global dict
        for condition, stc in stc_dict.items():
            parts_dict[part][condition].append(stc)

# Compute group averages for each condition
group_dict = {"I": {}, "II": {}, "III": {}}
for part, condition_dict in parts_dict.items():

    # Extract condition dict for the current part
    for condition, stcs in condition_dict.items():

        # Average source time course data across subjects
        stcs_data = [np.array(stc.data, dtype="float32") for stc in stcs]
        average_data = np.average(stcs_data, axis=0)

        # Put back into an SourceEstimate object
        group_stc = VolSourceEstimate(
            average_data,
            stcs[0].vertices,
            stcs[0].tmin,
            stcs[0].tstep,
            stcs[0].subject,
        )

        # Update the group dictionary
        group_dict[part][condition] = group_stc

# Save to results directory
pickle_filename = "../results/group_stcs.pickle"
pickle_file = open(pickle_filename, "wb")
pickle.dump(group_dict, pickle_file)
