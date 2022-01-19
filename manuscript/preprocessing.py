"""
PREPROCESSING FUNCTIONS

Created 2021 by Alexander Enge (alexander.enge@hu-berlin.de)

Contains one big function and some smaller helper functions to preprocess the EEG data
for a single participant. Using the MNE-Python module and friends, it reads the raw EEG
and behavioral data and extracts (a) the EEG epochs around the presentation of each
unfamiliar object (b) the single trial mean ERP amplitudes at certain regions of
interest, (c) averaged evoked potentials from at each electrode, and (d) single trial
EEG power at a pre-specified range of frequencies.

"""

from os import makedirs
from pathlib import Path

import mne
import numpy as np
import pandas as pd
from mne.channels import combine_channels, make_standard_montage
from mne.epochs import Epochs
from mne.io import read_raw_brainvision
from mne.preprocessing import ICA
from mne.time_frequency import tfr_morlet
from mne.time_frequency.tfr import EpochsTFR


def preprocess(
    fname_vhdr=None,
    fname_log=None,
    subject_id=None,
    output_dir=None,
    resample_sfreq=250,
    eog_names=["HEOG", "VEOG"],
    eog_anodes=["F9", "Auge_u"],
    eog_cathodes=["F10", "Fp1"],
    montage_kind="easycap-M1",
    ica_components=15,
    ica_method="fastica",
    random_seed=None,
    hipass=0.1,
    lowpass=30,
    event_id=None,
    tmin=-0.5,
    tmax=1.489,
    baseline=(-0.2, 0),
    erp_components=None,
    reject={"eeg": 200e-6},
    average_by=None,
    tfr_baseline=None,
    tfr_equalize=False,
    condition_col=None,
    item_col=None,
    drop_conditions=[],
    tfr_freqs=None,
    tfr_cycles=None,
    tfr_crop=(-0.2, 0.8),
):
    """
    Computes epochs, evoked potentials, and single trial power from a raw EEG data set.
    """

    # Create output directory if it doesn't exist
    makedirs(output_dir, exist_ok=True)

    # Read raw EEG data
    raw = read_raw_brainvision(fname_vhdr, preload=True)

    # Downsample
    raw = raw.resample(resample_sfreq)

    # Create virtual EOG channels
    raw = mne.set_bipolar_reference(
        raw,
        anode=eog_anodes,
        cathode=eog_cathodes,
        ch_name=eog_names,
        drop_refs=False,
    )
    mapping = dict(zip(eog_names, ["eog"] * len(eog_names)))
    raw.set_channel_types(mapping)

    # Set standard montage, removing excessive channels
    montage = make_standard_montage(kind=montage_kind)
    drops = list(set(raw.ch_names) - set(montage.ch_names) - set(eog_names))
    raw.drop_channels(drops)
    raw.set_montage(montage=montage)

    # Re-reference to common average
    raw, _ = mne.set_eeg_reference(raw, "average")

    # Run ICA on a copy of the data
    raw_filt_ica = raw.copy()
    raw_filt_ica.load_data().filter(l_freq=1, h_freq=None)
    ica = ICA(
        ica_components, random_state=random_seed, method=ica_method, max_iter="auto"
    )
    ica.fit(raw_filt_ica)

    # Remove bad components from the raw data
    eog_indices, eog_scores = ica.find_bads_eog(raw)
    ica.exclude = eog_indices
    raw = ica.apply(raw)

    # Apply band-pass filter
    raw_filt = raw.filter(hipass, lowpass)

    # Epoching including baseline correction
    events, _ = mne.events_from_annotations(raw, verbose=False)
    epochs = Epochs(raw_filt, events, event_id, tmin, tmax, baseline, preload=True)

    # Read behavioral metadata
    epochs.metadata = read_log(fname_log, subject_id)

    # Compute single trial mean amplitude for ERP components of interest
    for _, component in erp_components.iterrows():
        epochs = _single_trial_erps_from_epochs(
            epochs,
            name=component["name"],
            tmin=component["tmin"],
            tmax=component["tmax"],
            roi=component["roi"],
        )

    # Save epochs to output folder
    prefix = output_dir + "/" + subject_id + "_"
    epochs.save(prefix + "epo.fif")

    # Reject bad epochs
    metadata_backup = epochs.metadata  # We'll need this later for TFR
    epochs.drop_bad(reject)

    # Retrieve indices of good epochs
    good_epochs = [x for x in epochs.drop_log if not x == ("IGNORED",)]
    good_epochs = [i for i, x in enumerate(good_epochs) if x == ()]

    # Save trials to output folder
    epochs.metadata.to_csv(prefix + "trials.csv", float_format="%.3f", index=False)

    # Compute evoked potentials
    evokeds_list, evokeds_df = _evokeds_df_from_epochs(epochs, average_by)
    mne.write_evokeds(prefix + "epo-ave.fif", evokeds_list)
    evokeds_df.to_csv(prefix + "epo-ave.csv", float_format="%.3f", index=False)

    # Create unfiltered epochs for time-frequency analysis
    epochs_unf = Epochs(raw, events, event_id, tmin, tmax, baseline=None, preload=True)
    epochs_unf.metadata = metadata_backup

    # Select only good epochs based on ERP analysis
    epochs_unf = epochs_unf[good_epochs]

    # Equalize trial count if requested
    if tfr_equalize:
        epochs_unf = _equalize_trial_counts(
            epochs_unf, condition_col, item_col, drop_conditions, random_seed
        )

    # Compute single trial power
    del epochs, evokeds_list, evokeds_df, raw, raw_filt, raw_filt_ica  # Frees RAM
    tfr = tfr_morlet(
        epochs_unf,
        tfr_freqs,
        tfr_cycles,
        use_fft=True,
        return_itc=False,
        average=False,
    )
    tfr.apply_baseline(tfr_baseline, mode="percent")

    # Reduce size and save
    if tfr_crop is not None:
        tfr.crop(*tfr_crop)
    tfr.data = np.float32(tfr.data)
    tfr.save(prefix + "tfr.h5")

    # Compute evoked power
    _, evokeds_tfr_df = _evokeds_df_from_epochs(tfr, average_by)
    evokeds_tfr_df.to_csv(prefix + "tfr-ave.csv", float_format="%.3f", index=False)
    del epochs_unf, evokeds_tfr_df, tfr


# def repair_log(fname_log):
#     """
#     Removes the last column from the log file since it's producing errors.
#     """
#     log = pd.read_csv(fname_log, delimiter="\t", usecols=range(15),
#                       encoding="ISO-8859-1")
#     log.to_csv(fname_log, sep="\t", index=False, encoding="ISO-8859-1")


def read_log(fname_log=None, subject_id=None):
    """
    Extracts the relevant information from a behavioral log file into a DataFrame.
    """

    # Read the file
    log = pd.read_csv(fname_log, delimiter="\t", usecols=range(14), index_col=False)
    cols = {"StimID": "item_id", "VPNummer": "subject_id"}
    log.rename(columns=cols, inplace=True)

    # Use BIDS style subject ID
    log["subject_id"] = subject_id

    # Remove filler items
    log.query("bek_unbek == 'unbekannt'", inplace=True)

    # Create new column for part
    log["part"] = pd.Categorical(log["Wdh"])
    log["part"].cat.rename_categories({211: "I", 212: "II", 213: "III"}, inplace=True)

    # Recode button presses
    # 201: "I know what this is or have a strong assumption"    -> Recode as 3
    # 202: "I have an assumption what this is"                  -> Recode as 2
    # 203: "I have rather no assumption what this is"           -> Recode as 1
    # 204: "I don't know what this is and have no assumption."  -> Recode as 0
    log["response"] = 4 - (log["Tastencode"] - 200)

    # Extract conditions based on manipulations and button presses
    conditions = ["Excl_known", "Excl_informed", "Excl_naive", "Informed", "Naive"]
    queries = [
        "part == 'I' & Tastencode == 201",
        "part == 'II' & Bed == 'richtig' & Tastencode != [201, 202]",
        "part == 'II' & Bed == 'falsch' & Tastencode != [203, 204]",
        "part == 'II' & Bed == 'richtig' & Tastencode == [201, 202]",
        "part == 'II' & Bed == 'falsch' & Tastencode == [203, 204]",
    ]
    items_per_cond = [log.query(query)["item_id"].to_list() for query in queries]
    choicelist = [log["item_id"].isin(items) for items in items_per_cond]
    log["condition"] = np.select(choicelist, conditions)

    return log[["part", "condition", "subject_id", "item_id", "response", "RT"]]


def _single_trial_erps_from_epochs(epochs, name, tmin, tmax, roi):
    """
    Computes single-trial mean amplitude in a time window and region of interest.
    """

    # Create average channel for region of interest
    roi_dict = {name: mne.pick_channels(epochs.ch_names, roi)}
    epochs_roi = combine_channels(epochs, roi_dict)
    epochs.add_channels([epochs_roi], force_update_info=True)
    epochs.set_channel_types({name: "misc"})

    # Extract mean amplitudes by averaging across the relevant time window
    epochs_roi.crop(tmin, tmax)
    df = epochs_roi.to_data_frame()
    mean_amp = df.groupby("epoch")[name].mean()

    # Add as a new column to the original metadata
    epochs.metadata.reset_index(drop=True, inplace=True)
    epochs.metadata = pd.concat([epochs.metadata, mean_amp], axis=1)

    return epochs


def _evokeds_df_from_epochs(epochs, average_by):
    """
    Computes per-condition evoked potentials by averaging the relevant epochs.
    """

    # Get unique combinations of conditions
    conditions = epochs.metadata[average_by].drop_duplicates()

    # Prepare emtpy lists
    evokeds_list = list()
    evokeds_dfs = list()

    # Compute evoked averages for each condition
    for _, condition in conditions.iterrows():

        # Convert current condition to query for extracting the relevant epochs
        condition = pd.DataFrame({"value": condition})
        condition["key"] = condition.index
        condition["query"] = condition["key"] + " == '" + condition["value"] + "'"
        query = condition["query"].to_list()
        query = " & ".join(query)

        # Average epochs belonging to this condition and convert to DataFrame
        if isinstance(epochs, Epochs):
            evokeds = epochs[query].average(picks=["eeg", "misc"])
            scalings = {"eeg": 1e6, "misc": 1e6}
            evokeds_df = evokeds.to_data_frame(scalings=scalings)
        elif isinstance(epochs, EpochsTFR):
            evokeds = epochs[query].average()
            evokeds_df = evokeds.to_data_frame()
        evokeds.comment = query

        # Add info about the current conditions
        condition_df = pd.concat([condition[["value"]].transpose()] * len(evokeds_df))
        condition_df.reset_index(inplace=True, drop=True)
        evokeds_df = pd.concat([condition_df, evokeds_df], axis=1)

        # Add current condition to the lists
        evokeds_list.append(evokeds)
        evokeds_dfs.append(evokeds_df)

    # Concatenate the list of DataFrames into one big one
    evoked_df = pd.concat(evokeds_dfs)

    return evokeds_list, evoked_df


def _equalize_trial_counts(
    epochs, condition_col, item_col, drop_conditions=[], random_seed=None
):
    """
    Equalize the number of items (and, thus, trials) for the relevant conditions.
    """

    # Set random seed (can be None)
    np.random.seed(random_seed)

    # Create a copy of the epochs
    epochs_eq = epochs.copy()

    # Drop irrelevant conditions
    drop_bool = epochs_eq.metadata[condition_col].isin(drop_conditions)
    epochs_eq.drop(drop_bool, reason="drop_conditions")

    # Find no. of items per condition plus its minimum
    metadata = epochs_eq.metadata
    conds = np.unique(metadata[condition_col])
    items_per_cond = [
        metadata.loc[metadata[condition_col] == cond, item_col] for cond in conds
    ]
    items_per_cond = [np.unique(items) for items in items_per_cond]
    n_items_min = min([len(items) for items in items_per_cond])

    # For all conditions, remove a random subset of items to the minimum
    for items in items_per_cond:
        n_items = len(items)
        n_drop = n_items - n_items_min
        drop_items = np.random.choice(items, size=n_drop, replace=False)
        drop_bool = epochs_eq.metadata[item_col].isin(drop_items)
        epochs_eq.drop(drop_bool, reason="equalize_trial_counts")

    return epochs_eq
