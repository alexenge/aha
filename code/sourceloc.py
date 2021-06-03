# %%
import os.path as op

import groupmne
import matplotlib.pyplot as plt
import mne
import numpy as np

# Prepare an empty dict to store the evokeds for all subjects
evokeds_dict = {
    "I": {"Informed": [], "Naive": [], "Difference": []},
    "II": {"Informed": [], "Naive": [], "Difference": []},
    "III": {"Informed": [], "Naive": [], "Difference": []},
}

# How many subjects to include (min = 1, max = 48)
nsubs = 48

# Read evokeds for all subjects and organize the relevant conditions in a dict
subject_id = ["sub-" + "{:02d}".format(s) for s in range(1, nsubs + 1)]
for s in subject_id:
    evokeds = mne.read_evokeds("../data/preprocessed/" + s + "_ave.fif", verbose=False)
    for pt in ["I", "II", "III"]:
        for ev in evokeds:
            #ev = ev.copy().drop_channels(["P1", "N170", "N400"])
            if "part == '" + pt + "' & condition == 'Informed'" in ev.comment:
                ev_informed = ev
                evokeds_dict[pt]["Informed"].append(ev)
            elif "part == '" + pt + "' & condition == 'Naive'" in ev.comment:
                ev_naive = ev
                evokeds_dict[pt]["Naive"].append(ev)

        # Compute difference wave between conditions
        evokeds_dict[pt]["Difference"].append(
            mne.combine_evoked([ev_informed, ev_naive], weights=[1, -1])
        )

# # Compute grand averages across subjects and plot
# for pt in ["I", "II", "III"]:
#     for cn in ["Informed", "Naive", "Difference"]:
#         evokeds_dict[pt][cn] = mne.combine_evoked(evokeds_dict[pt][cn], weights="equal")
#         evokeds_dict[pt][cn].comment = "part-" + pt + "_condition-" + cn
#         print("Grand average, part " + pt + ", condition " + cn)
#         evokeds_dict[pt][cn].plot(picks=["P1", "N170", "N400"], hline=[0], show=True)

# Download fsaverage MRI template
fs_dir = mne.datasets.fetch_fsaverage(verbose=False)
subjects_dir = op.dirname(fs_dir)
subject = "fsaverage"
trans = "fsaverage"  # MNE has a built-in fsaverage transformation
src = op.join(fs_dir, "bem", "fsaverage-ico-5-src.fif")
bem = op.join(fs_dir, "bem", "fsaverage-5120-5120-5120-bem-sol.fif")

# # Check that the locations of EEG electrodes is correct with respect to MRI
# set_3d_backend("notebook")
# plot_alignment(
#     evokeds_dict["I"]["Informed"].info,
#     src=src,
#     eeg=["original", "projected"],
#     trans=trans,
#     show_axes=True,
#     mri_fiducials=True,
#     dig="fiducials",
# )

# For which evokeds to compute the source time course
part = "II"
evokeds_informed = evokeds_dict[part]["Informed"]
evokeds_naive = evokeds_dict[part]["Naive"]
info = evokeds_informed[0].info

# %%
#######################################
## MNE ##
#######################################

# Compute standard forward solution
fwd = mne.make_forward_solution(info, trans, src, bem, meg=False, eeg=True, mindist=5.0)

# # Use fwd to compute the sensitivity map for illustration purposes
# eeg_map = mne.sensitivity_map(fwd, ch_type="eeg", mode="fixed")
# eeg_map.plot(
#     time_label="EEG sensitivity",
#     subjects_dir=subjects_dir,
#     clim=dict(lims=[5, 50, 100]),
#     backend="matplotlib",
# )

# Initialize parameters for inverse solution
method = "dSPM"
snr = 3.0
lambda2 = 1.0 / snr ** 2
stcs = {"Informed": [], "Naive": [], "Difference": []}

# Compute inverse solution for each subject
for sid, ev_i, ev_n in zip(subject_id, evokeds_informed, evokeds_naive):
    fname_epochs = "../data/preprocessed/" + sid + "_epo.fif"
    epochs = mne.read_epochs(fname_epochs, preload=True)
    epochs.drop_channels(["P1", "N170", "N400"])
    ev_i.set_eeg_reference("average", projection=True)
    ev_n.set_eeg_reference("average", projection=True)
    noise_cov = mne.compute_covariance(epochs, tmax=0.0, method=["shrunk", "empirical"])
    inverse_operator = mne.minimum_norm.make_inverse_operator(
        info, fwd, noise_cov, loose=0.2, depth=3.5
    )
    stc_i = mne.minimum_norm.apply_inverse(
        ev_i,
        inverse_operator=inverse_operator,
        lambda2=lambda2,
        method=method,
        pick_ori=None,
        return_residual=False,
        verbose=True,
    )
    stc_n = mne.minimum_norm.apply_inverse(
        ev_n,
        inverse_operator=inverse_operator,
        lambda2=lambda2,
        method=method,
        pick_ori=None,
        return_residual=False,
        verbose=True,
    )
    stc_i.crop(-0.2, 0.8)
    stc_n.crop(-0.2, 0.8)
    stc_d = stc_i.copy()
    stc_d.data = stc_i.data - stc_n.data
    stcs["Informed"].append(stc_i)
    stcs["Naive"].append(stc_n)
    stcs["Difference"].append(stc_d)

# Compute group average
condition = "Difference"
data = np.array([stc.data for stc in stcs[condition]], dtype="float32")
data = data.mean(axis=0)
stc = mne.SourceEstimate(
    data=data,
    vertices=stcs[condition][0].vertices,
    tmin=stcs[condition][0].tmin,
    tstep=stcs[condition][0].tstep,
    subject=stcs[condition][0].subject
)

# Average across time window and plot
tmin = 0.150
tmax = 0.200
stc_twin = stc.copy().crop(tmin, tmax).mean()
stc_twin.plot(
    hemi="lh",
    clim={"kind": "values", "lims": [1, 2, 3]},
    backend="matplotlib",
)

# # Save full source time course
# stc.save("../results/stc_part-" + part + "_condition-" + condition)

# %%
#######################################
## GROUPMNE ##
#######################################

# Read preprocessed epochs
fnames_epochs = ["../data/preprocessed/" + sid + "_epo.fif" for sid in subject_id]
epochs = [
    mne.read_epochs(fname, preload=True).drop_channels(["P1", "N170", "N400"])
    for fname in fnames_epochs
]

# Compute noise covariance matrices
noise_covs = [
    mne.compute_covariance(ep, tmax=0, method=["shrunk", "empirical"], rank=None)
    for ep in epochs
]

# Compute source reference space
resolution = 5
spacing = "oct%d" % resolution
src_ref = mne.setup_source_space(
    subject="fsaverage", spacing=spacing, subjects_dir=subjects_dir, add_dist=False
)

# Compute a common standard forward solution for every subject
trans = "fsaverage"
bem = op.join(fs_dir, "bem", "fsaverage-5120-5120-5120-bem-sol.fif")
fwds_ = [
    groupmne.compute_fwd(
        subject="fsaverage",
        src_ref=src_ref,
        info=ev.info,
        trans_fname=trans,
        bem_fname=bem,
        meg=False,
        eeg=True,
    )
    for ev in evokeds
]
fwds = groupmne.prepare_fwds(fwds_, src_ref, copy=False)

# Compute forward models with a reference source space (multikllasso)
stcs = groupmne.compute_group_inverse(
    fwds=fwds,
    evokeds=evokeds,
    noise_covs=noise_covs,
    method="multitasklasso",
    spatiotemporal=True,
    alpha=0.8,
)

# Compute forward models with a reference source space (mkw)
stcs = groupmne.compute_group_inverse(
    fwds=fwds,
    evokeds=evokeds,
    noise_covs=noise_covs,
    method="mtw",
    spatiotemporal=False,
    alpha=1.0,
    beta=0.05,
    n_jobs=2,
)

# Compute group average
data = np.average([stc.data for stc in stcs], axis=0)
stc = mne.SourceEstimate(
    data=data,
    vertices=stcs["Informed"][0].vertices,
    tmin=stcs["Informed"][0].tmin,
    tstep=stcs["Informed"][0].tstep,
    subject="fsaverage",
)

# Plot & save
t = 0.5
stc.plot(
    hemi="lh",
    clim={"kind": "value", "lims": [0.001, 0.004, 0.007]},
    initial_time=t,
    backend="matplotlib",
)
stc.save("../results/stc_part-II_condition-difference_groupmne")
