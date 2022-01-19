% Load FieldTrip and EEGLAB
addpath /Users/alexander/Documents/MATLAB/Toolboxes/fieldtrip
addpath /Users/alexander/Documents/MATLAB/Toolboxes/eeglab
ft_defaults;
eeglab;

% List preprocessed epoch files from MNE
preprocessed_dir = '/Users/alexander/Research/aha/data/preprocessed';

kinds = {'flat', 'diff'};
n_kinds = length(kinds);

% Create empty structure for storing ERPs
erps = {};
erps.I.flat = {};
erps.I.diff = {};
erps.II.flat = {};
erps.II.diff = {};
erps.III.flat = {};
erps.III.diff = {};

% Loop over subjects
for ix = 1:48
    
    % Load epochs
    fname_epo = sprintf('%s/sub-%02d_epo.fif', preprocessed_dir, ix);
    hdr = ft_read_header(fname_epo);
    dat = ft_read_data(fname_epo);
    
    % Convert to EEGLAB
    EEG = fieldtrip2eeglab(hdr, dat);
    
    % Load standard locations for EEG channels
    EEG = pop_select(EEG, 'channel', 1:62);
    EEG = pop_chanedit(EEG, 'lookup', 'standard-10-5-cap385.elp');
    
    % Load single-trial behavioral data
    fname_trials = sprintf('%s/sub-%02d_trials.csv', preprocessed_dir, ix);
    trials = readtable(fname_trials);
    
    % Extract ERPs for each part
    parts = {'I', 'II', 'III'};
    n_parts = length(parts);
    for ix_part = 1:n_parts
        
        % Get trial indices from behavioral data
        curr_part = parts{ix_part};
        is_part = strcmp(trials.part, curr_part);
        
        % Compute averaged ERP across all conditions
        erp_flat = mean(EEG.data(:, :, is_part), 3);
        erps.(curr_part).flat(end + 1) = {erp_flat};
        
        % Compute averaged ERP for the difference between conditions
        is_informed = is_part & strcmp(trials.condition, 'Informed');
        is_naive = is_part & strcmp(trials.condition, 'Naive');
        erp_informed = mean(EEG.data(:, :, is_informed), 3);
        erp_naive = mean(EEG.data(:, :, is_naive), 3);
        erp_diff = erp_informed - erp_naive;
        erps.(curr_part).diff(end + 1) = {erp_diff};
        
    end
end

% Prepare BEM MNI model
dipfitdefs;
bem_model = template_models(2);
EEG = pop_dipfit_settings(...
    EEG, ...
    'hdmfile', bem_model.hdmfile, ...
    'mrifile', bem_model.mrifile, ...
    'coordformat', bem_model.coordformat, ...
    'chanfile', bem_model.chanfile, ...
    'chansel', [1:62], ...
    'coord_transform', [0 0 0 0 0 -1.5708 1 1 1]);

% Define components of interest
components = {};
components.P1 = {};
components.P1.tmin = 0.1;
components.P1.tmax = 0.15;
components.N1 = {};
components.N1.tmin = 0.15;
components.N1.tmax = 0.2;

% Average ERPs across subjects and compute dipoles
erps_grand = {};
for ix_part = 1:n_parts
    curr_part = parts{ix_part};
    for ix_kind = 1:length(kinds)
        
        % Grand averaging
        curr_kind = kinds{ix_kind};
        curr_erps = erps.(curr_part).(curr_kind);
        curr_erps = cat(3, curr_erps{:});
        curr_erps = mean(curr_erps, 3);
        erps_grand.(curr_part).(curr_kind) = curr_erps;
        
        % Fit dipoles for P1 and N1
        component_names = fieldnames(components);
        for ix_component = 1:length(component_names)
            
            % Get time window for the current component
            curr_component = component_names(ix_component);
            curr_component = curr_component{:};
            tmin = components.(curr_component).tmin;
            tmax = components.(curr_component).tmax;
            ix_min = round((tmin - EEG.xmin) * EEG.srate);
            ix_max = round((tmax - EEG.xmin) * EEG.srate);
            
            % Average ERPs in time window
            erps_timewindow = curr_erps(:, ix_min:ix_max);
            erps_timewindow = mean(erps_timewindow, 2);
            
            % Fit dipoles
            [dipole, model, TMPEEG] = dipfit_erpeeg(...
                erps_timewindow, ...
                EEG.chanlocs, ...
                'settings', EEG.dipfit, ...
                'dipoles', 2, ...
                'threshold', 100);
            
            % Save to structure
            curr_struct = {};
            curr_struct.dipole = dipole;
            curr_struct.model = model;
            curr_struct.TMPEEG = TMPEEG;
            components.(curr_component).(curr_part).(curr_kind) = curr_struct;
            
        end
    end
end

% Plot dipole on MRI slice
pop_dipplot(components.P1.III.diff.TMPEEG, 1, 'normlen', 'on');

% Plot dipole with topography
figure; pop_topoplot(components.N1.II.diff.TMPEEG, 0, 1, ...
    ['ERP fit with a single dipole (RV ' num2str(components.N1.II.diff.dipole(1).rv*100,2) '%)'], ...
    0, 1);
