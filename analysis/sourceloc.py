import os.path as op
import mne
import numpy as np
from mne.minimum_norm import make_inverse_operator, apply_inverse
from pyvistaqt import BackgroundPlotter
import matplotlib.pyplot as plt

# Download fsaverage files
fs_dir = mne.datasets.fetch_fsaverage(verbose=True)
subjects_dir = op.dirname(fs_dir)

# The files live in:
subject = 'fsaverage'
trans = 'fsaverage'  # MNE has a built-in fsaverage transformation
src = op.join(fs_dir, 'bem', 'fsaverage-ico-5-src.fif')
bem = op.join(fs_dir, 'bem', 'fsaverage-5120-5120-5120-bem-sol.fif')

# Load and process EEG data
vhdr_fname = "../data/eeg/exp1/Vp0004.vhdr"
raw = mne.io.read_raw_brainvision(vhdr_fname, scale=1e6, preload=True)
raw = mne.set_bipolar_reference(raw, "Auge_u", "Fp1", ch_name="VEOG", drop_refs=False)
raw = mne.set_bipolar_reference(raw, "F9", "F10", ch_name="HEOG", drop_refs=False)
raw.set_channel_types(mapping={"VEOG": "eog", "HEOG": "eog"})
montage = mne.channels.make_standard_montage("easycap-M1")
raw.drop_channels(list(set(raw.ch_names) - set(montage.ch_names) - set(["VEOG", "HEOG"])))
raw.set_montage(montage=montage)
raw.set_eeg_reference("average", projection=True)

# # Check that the locations of EEG electrodes is correct with respect to MRI
# mne.viz.plot_alignment(
#     raw.info, src=src, eeg=['original', 'projected'], trans=trans,
#     show_axes=True, mri_fiducials=True, dig='fiducials')

fwd = mne.make_forward_solution(raw.info, trans=trans, src=src, bem=bem, meg=False, eeg=True, n_jobs=1)
print(fwd)

# Use fwd to compute the sensitivity map for illustration purposes
eeg_map = mne.sensitivity_map(fwd, ch_type='eeg', mode='fixed')
brain = eeg_map.plot(time_label='EEG sensitivity', subjects_dir=subjects_dir,clim=dict(lims=[5, 50, 100]))

# Read epochs
epochs = mne.read_epochs("export/exp1-epo.fif")
#epochs = epochs["participant == 'VP_04'"]
epochs = epochs.crop(tmin=-0.2, tmax=0.3)
epochs.set_eeg_reference("average", projection=True)

noise_cov = mne.compute_covariance(epochs, tmax=0., method=['shrunk', 'empirical'], rank=None, verbose=True)
fig_cov, fig_spectra = mne.viz.plot_cov(noise_cov, raw.info)

evoked = epochs.average().pick('eeg')
evoked.plot(time_unit='s')
evoked.plot_topomap(times=np.linspace(0.05, 0.15, 5), ch_type='eeg', time_unit='s')
evoked.plot_white(noise_cov, time_unit='s')

inverse_operator = make_inverse_operator(evoked.info, fwd, noise_cov, loose=0.2, depth=0.8)

method = "dSPM"
snr = 3.
lambda2 = 1. / snr ** 2
stc, residual = apply_inverse(evoked, inverse_operator, lambda2,
                              method=method, pick_ori=None,
                              return_residual=True, verbose=True)
fig, ax = plt.subplots()
ax.plot(1e3 * stc.times, stc.data[::100, :].T)
ax.set(xlabel='time (ms)', ylabel='%s value' % method)
fig, ax = plt.subplots()
evoked.plot(axes=ax)
ax.texts = []
for line in ax.lines:
    line.set_color('#98df81')
residual.plot(axes=ax)
vertno_max, time_max = stc.get_peak(hemi='rh')
surfer_kwargs = dict(
    hemi='rh', subjects_dir=subjects_dir, views='lateral',
    initial_time=time_max, time_unit='s', size=(800, 800), smoothing_steps=10)
brain = stc.plot(**surfer_kwargs)
brain.add_foci(vertno_max, coords_as_verts=True, hemi='rh', color='blue',
               scale_factor=0.6, alpha=0.5)
brain.add_text(0.1, 0.9, 'dSPM (plus location of maximal activation)', 'title',
               font_size=14)
brain.save_movie(filename="movie.mp4", tmin=-0.2, tmax=0.3, interpolation='linear',
                 time_dilation=10, framerate=60, time_viewer=True)

evoked.crop(0.4, 0.7)
dip = mne.fit_dipole(evoked, cov=noise_cov, bem=bem, trans=trans)[0]
dip.plot_locations(trans, subject, subjects_dir, mode='orthoview', show_all=True)
dip.plot_amplitudes()
