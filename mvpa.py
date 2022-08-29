from glob import glob

import matplotlib.pyplot as plt
import numpy as np
from mne import read_epochs
from mne.decoding import SlidingEstimator, cross_val_multiscore
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import LinearSVC

# Do decoding separately for each participant
epochs_files = sorted(glob("output/epochs/*/*_epo.fif"))
scores_list = []
for epochs_file in epochs_files:

    # Do decoding separately for each phase of the experiment
    all_epochs = read_epochs(epochs_file)['condition in ["Informed", "Naive"]']
    scores_participant_list = []
    phases = ["Pre-insight", "Insight", "Post-insight"]
    for phase in phases:

        # Extract data and labels for the current phase
        bad_ixs = np.isnan(all_epochs.metadata["N170"])
        epochs = all_epochs[~bad_ixs][f'phase == "{phase}"']
        X = epochs.pick_types(eeg=True).get_data()
        y = epochs.events[:, 2]

        # Run decoding over time
        # clf = make_pipeline(StandardScaler(), LogisticRegression(solver="liblinear"))
        clf = make_pipeline(StandardScaler(), LinearSVC(max_iter=10000))
        time_decod = SlidingEstimator(clf, n_jobs=None, scoring="roc_auc", verbose=True)
        scores = cross_val_multiscore(time_decod, X, y, cv=5, n_jobs=5)

        # Compute mean scores across cross-validation splits
        scores = np.mean(scores, axis=0)
        scores_participant_list.append(scores)

    # Combine scores for the three phases into a single array
    scores_participant = np.stack(scores_participant_list, axis=1)
    scores_list.append(scores_participant)

# Average scores per phase across participants
mean_scores = np.stack(scores_list).mean(axis=0)

# Plot decoding scores for all three phases
fig, ax = plt.subplots()
ax.plot(epochs.times, mean_scores, label="score")
ax.axhline(0.5, color="k", linestyle="--")
ax.set_xlabel("Times")
ax.set_ylabel("AUC")  # Area Under the Curve
plt.legend(labels=phases)
ax.axvline(0.0, color="k", linestyle="-")
ax.set_title("Sensor space decoding")
