"""
PREPROCESSING FUNCTIONS

Created May 2021 by Alexander Enge (alexander.enge@hu-berlin.de)

Contains one big function and some smaller helper functions to preprocess the EEG data
for a single participant. Using the MNE-Python module and friends, it reads the raw EEG
and behavioral data and extracts (a) the EEG epochs around the presentation of each
unfamiliar object (b) the single trial mean ERP amplitudes at certain regions of
interest, (c) averaged evoked potentials from at each electrode, and (d) single trial
EEG power at a pre-specified range of frequencies.

"""

from glob import glob
from os import makedirs, removedirs

import numpy as np
import pandas as pd
from mne import Epochs, events_from_annotations, pick_channels, set_bipolar_reference
from mne.channels import combine_channels, make_standard_montage
from mne.io import read_raw_brainvision
from mne.preprocessing import ICA
from mne.time_frequency import tfr_morlet


def preprocess(
    fname_vhdr=None,
    fname_log=None,
    subject_id=None,
    output_dir=None,
    eog_names=["HEOG", "VEOG"],
    eog_anodes=["F9", "Auge_u"],
    eog_cathodes=["F10", "Fp1"],
    montage_kind="easycap-M1",
    ica_components=15,
    ica_method="fastica",
    ica_seed=1234,
    hipass=0.1,
    lowpass=30,
    event_id=None,
    tmin=-0.5,
    tmax=1.489,
    baseline=(-0.2, 0),
    erp_components=None,
    reject={"eeg": 200e-6},
    condition_cols=["part", "condition"],
    tfr_freqs=None,
    tfr_cycles=None,
    tfr_sfreq_downsampled=250,
    tfr_crop=(-0.2, 0.8),
):
    """
    Computes epochs, evoked potentials, and single trial power from a raw EEG data set.
    """

    # Create output directory if it doesn't exist
    makedirs(output_dir, exist_ok=True)

    # Read raw data in microvolts
    raw = read_raw_brainvision(fname_vhdr, preload=True)

    # Create virtual EOG channels
    raw = set_bipolar_reference(
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
    drops = list(set(raw.ch_names) - set(montage.ch_names) - set(["VEOG", "HEOG"]))
    raw.drop_channels(drops)
    raw.set_montage(montage=montage)

    # Run ICA on a copy of the data
    raw_filt_ica = raw.copy()
    raw_filt_ica.load_data().filter(l_freq=1, h_freq=None)
    ica = ICA(n_components=ica_components, random_state=ica_seed, method=ica_method)
    ica.fit(raw_filt_ica)

    # Remove bad components from the raw data
    eog_indices, eog_scores = ica.find_bads_eog(raw)
    ica.exclude = eog_indices
    raw = ica.apply(raw)

    # Apply band-pass filter
    raw_filt = raw.filter(l_freq=hipass, h_freq=lowpass)

    # Epoching and baseline correction
    events, _ = events_from_annotations(raw, verbose=False)
    epochs = Epochs(raw_filt, events, event_id, tmin, tmax, baseline, preload=True)

    # Read behavioral metadata
    epochs.metadata = _read_log(fname_log, subject_id)

    # Compute single trial mean amplitude for ERP components of interest
    for _, component in erp_components.iterrows():
        epochs = _single_trial_erps_from_epochs(
            epochs,
            name=component["name"],
            tmin=component["tmin"],
            tmax=component["tmax"],
            roi=component["roi"],
        )

    # Reject bad epochs
    metadata_backup = epochs.metadata
    epochs.drop_bad(reject=reject)

    # Save epochs and trials to output folder
    prefix = output_dir + "/" + subject_id + "_"
    epochs.save(prefix + "epo.fif")
    epochs.metadata.to_csv(prefix + "trials.csv", float_format="%.3f", index=False)

    # Compute evoked potentials
    evokeds = _evokeds_df_from_epochs(epochs, condition_cols)
    evokeds.to_csv(prefix + "evo.csv", float_format="%.3f", index=False)

    # Compute single trial power from unfiltered epochs
    epochs_unf = Epochs(raw, events, event_id, tmin, tmax, baseline, preload=True)
    del epochs, evokeds, raw, raw_filt, raw_filt_ica
    epochs_unf.metadata = metadata_backup
    tfr_decim = int(epochs_unf.info["sfreq"] / tfr_sfreq_downsampled)
    tfr = tfr_morlet(
        epochs_unf,
        tfr_freqs,
        tfr_cycles,
        use_fft=True,
        return_itc=False,
        decim=tfr_decim,
        average=False,
    )
    tfr.apply_baseline(baseline, mode="percent")
    tfr.crop(*tfr_crop)
    tfr.data = np.float32(tfr.data)
    tfr.save(prefix + "tfr.h5")
    del tfr


def _read_log(fname_log=None, subject_id=None):
    """
    Extracts the relevant information from a behavioral log file into a DataFrame.
    """

    # Read the file
    log = pd.read_csv(fname_log, delimiter="\t", usecols=range(14), index_col=False)
    columns = {"StimID": "item", "VPNummer": "participant"}
    log.rename(columns=columns, inplace=True)

    # Use BIDS style subject ID
    log["participant"] = subject_id

    # Remove filler items
    log.query("bek_unbek == 'unbekannt'", inplace=True)

    # Create new column for part
    log["part"] = pd.Categorical(log["Wdh"])
    log["part"].cat.rename_categories({211: "I", 212: "II", 213: "III"}, inplace=True)

    # Extract conditions based on manipulations and button presses
    conditions = ["Excl_known", "Excl_informed", "Excl_naive", "Informed", "Naive"]
    queries = [
        "part == 'I' & Tastencode == 201",
        "part == 'II' & Bed == 'richtig' & Tastencode != [201, 202]",
        "part == 'II' & Bed == 'falsch' & Tastencode != [203, 204]",
        "part == 'II' & Bed == 'richtig' & Tastencode == [201, 202]",
        "part == 'II' & Bed == 'falsch' & Tastencode == [203, 204]",
    ]
    items_per_cond = [log.query(query)["item"].to_list() for query in queries]
    choicelist = [log["item"].isin(items) for items in items_per_cond]
    log["condition"] = np.select(choicelist, conditions)

    return log[["part", "condition", "participant", "item", "RT"]]


def _single_trial_erps_from_epochs(epochs, name, tmin, tmax, roi):
    """
    Computes single-trial mean amplitude in a time window and region of interest.
    """

    # Create average channel for region of interest
    roi_dict = {name: pick_channels(epochs.ch_names, roi)}
    epochs_roi = combine_channels(epochs, roi_dict)
    epochs.add_channels([epochs_roi], force_update_info=True)

    # Extract mean amplitudes by averaging across the relevant time window
    epochs_roi.crop(tmin, tmax)
    df = epochs_roi.to_data_frame()
    mean_amp = df.groupby("epoch")[name].mean()

    # Add as a new column to the original metadata
    epochs.metadata.reset_index(drop=True, inplace=True)
    epochs.metadata = pd.concat([epochs.metadata, mean_amp], axis=1)

    return epochs


def _evokeds_df_from_epochs(epochs, condition_cols):
    """
    Computes per-condition evoked potentials by averaging the relevant epochs.
    """

    # Get unique combinations of conditions
    conditions = epochs.metadata[condition_cols].drop_duplicates()

    # Create a list of evoked DataFrames for each condition
    evokeds_dfs = list()
    for _, condition in conditions.iterrows():

        # Convert current condition to query for extracting epochs
        condition = pd.DataFrame({"value": condition})
        condition["key"] = condition.index
        condition["query"] = condition["key"] + " == '" + condition["value"] + "'"
        query = condition["query"].to_list()
        query = " & ".join(query)

        # Average epochs belonging to the current condition
        evokeds = epochs[query].average()

        # Convert to DataFrame and append condition columns
        evokeds_df = evokeds.to_data_frame()
        condition_df = pd.concat([condition[["value"]].transpose()] * len(evokeds_df))
        condition_df.reset_index(inplace=True, drop=True)
        evokeds_df = pd.concat([condition_df, evokeds_df], axis=1)

        # Add to the list
        evokeds_dfs.append(evokeds_df)

    # Concatenate into one big DataFrame
    return pd.concat(evokeds_dfs)
