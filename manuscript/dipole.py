import os.path as op

import numpy as np
from mne import dipole, fit_dipole, make_ad_hoc_cov
from mne.datasets import fetch_fsaverage
from mne.evoked import combine_evoked, read_evokeds
import matplotlib.pyplot as plt

# Prepare an emparty dict to store the evokeds for all subjects
parts = ["I", "II", "III"]
conditions = ["Informed", "Naive", "Combined", "Difference"]
subjects_dict = {part: {condition: [] for condition in conditions} for part in parts}

# Loop over subject IDs
n_subs = 5  # max = 48
subject_id = ["sub-" + "{:02d}".format(s) for s in range(1, n_subs + 1)]
for sid in subject_id:

    # Read list of evokeds for the current subejct
    evokeds = read_evokeds(f"../data/preprocessed/{sid}_epo-ave.fif", verbose=False)

    # Search which evokeds belong to the current part
    for part in parts:
        for ev in evokeds:

            # Remove non-EEG channels
            ev = ev.pick_types(eeg=True)

            # Append to the dict for the correct condition
            if f"part == '{part}' & condition == 'Informed'" in ev.comment:
                ev_informed = ev
                subjects_dict[part]["Informed"].append(ev)
            elif f"part == '{part}' & condition == 'Naive'" in ev.comment:
                ev_naive = ev
                subjects_dict[part]["Naive"].append(ev)

        # Compute flat average across both conditions
        ev_combined = combine_evoked([ev_informed, ev_naive], weights=[0.5, 0.5])
        subjects_dict[part]["Combined"].append(ev_combined)

        # Compute difference wave between conditions
        ev_difference = combine_evoked([ev_informed, ev_naive], weights=[0.5, -0.5])
        subjects_dict[part]["Difference"].append(ev_difference)

# Compute group averages
group_dict = {
    part: {
        condition: combine_evoked(subjects_dict[part][condition], weights="nave")
        for condition in conditions
    }
    for part in parts
}

# Download standard MRI template and source/head model
fs_dir = fetch_fsaverage(verbose=False)
subjects_dir = op.dirname(fs_dir)
subject = "fsaverage"
trans = "fsaverage"  # MNE has a built-in fsaverage transformation
src = op.join(fs_dir, "bem", "fsaverage-ico-5-src.fif")
bem = op.join(fs_dir, "bem", "fsaverage-5120-5120-5120-bem-sol.fif")

# Get ad hoc noise covariance
info = group_dict["I"]["Informed"].info  # Would be the same for all evokeds
noise_cov = make_ad_hoc_cov(info)

# Create emtpy dictionaries to store dipoles and residuals
dipole_dict = {part: {condition: {} for condition in conditions} for part in parts}
resid_dict = {part: {condition: {} for condition in conditions} for part in parts}

# Compute one dipole for each part, condition, and component
components = {"P1": (0.100, 0.150), "N170": (0.150, 0.200), "N400": (0.400, 0.700)}
for part in parts:
    for condition in conditions:
        for component, (tmin, tmax) in components.items():

            # Extract a copy of the relevant evoked
            evoked = group_dict[part][condition].copy()

            # Get average evoked over time window
            evoked.crop(tmin, tmax)
            evoked.data = evoked.data.mean(axis=1)
            evoked.data = np.expand_dims(evoked.data, axis=1)  # Keep it 2D
            evoked.times = np.array([(tmin + tmax) / 2])

            # Fit the dipole
            dip, resid = fit_dipole(evoked, noise_cov, bem, trans)

            # Update the dictionaries
            dipole_dict[part][condition][component] = dip
            resid_dict[part][condition][component] = resid

# Plot all dipoles
condition = "Combined"
fig = plt.figure()
nrows = len(components)
ncols = len(parts)
ax_ix = 0
for component in components:
    for part in parts:
        ax_ix += 1
        ax = fig.add_subplot(nrows, ncols, ax_ix, projection="3d")
        dip = dipole_dict[part][condition][component]
        dip.plot_locations(trans, "fsaverage", subjects_dir, ax=ax, show=False)
fig.tight_layout()
fig.savefig("dipoles.pdf")

# Plot a single dipole
dip = dipole_dict["II"]["Combined"]["N170"]
dip.plot_locations(trans, "fsaverage", subjects_dir, mode="orthoview")
