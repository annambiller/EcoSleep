# -*- coding: utf-8 -*-
'''
Date: 22.03.2024
Author: LH
Function: Script to load sleep data recorded with Mentalab Explore in BDF format, and perform automated sleep staging using YASA (Vallat & Walker, 2021)
Sources:
    https://mne.tools/stable/index.html
    https://raphaelvallat.com/yasa/build/html/index.html
'''


# %%  Libraries

import mne # EEG processing
import yasa # automated sleep staging & feature extraction
import pandas as pd # creating dataframes & exporting to CSV
import matplotlib.pyplot as plt # plots



# %% Variables to modify

# Filename of the EEG recording in BDF format; r before full path to accept backlash
filename_bdf = r'C:\Users\Mitarbeiter\OneDrive - TUM\TUM PhD\Team\EcoSleep\Python_YASA_training\test_ExG.bdf'

# Path and filename to store hypnogram
filename_hypnogram = r'C:\Users\Mitarbeiter\OneDrive - TUM\TUM PhD\Team\EcoSleep\Python_YASA_training\hypnogram.png'

# Path and filename to store sleep statistics
filename_stats = r'C:\Users\Mitarbeiter\OneDrive - TUM\TUM PhD\Team\EcoSleep\Python_YASA_training\sleep_statistics.csv'

# Decide whether to use left or right side electrodes
electrodes_side = 'left' # 'left' or 'right'

# Define sex of the pilot participant (coded in boolean for YASA) and age
male = False # True for male, False for female
age = 30 



# %% Import and format data

# Load the BDF file into memory
raw_original = mne.io.read_raw_bdf(filename_bdf, preload=True)

# Show information of original data file
print(raw_original.info)

# Drop unnecessary timestamp channel
raw_original.drop_channels(['TimeStamp'])

# Rename channels (REF channel, A1, is not listed, as it is the system reference for all these channels)
raw_original.rename_channels({'ch1':'A2', 'ch2':'Fp1', 'ch3':'Fp2', 'ch4':'EOG1', 'ch5':'EOG2', 'ch6':'EMG1', 'ch7':'EMG2', 'ch8':'FpZ'})

if electrodes_side == 'right':
    # Get the relevant channel subset
    raw = raw_original.pick_channels(['Fp2','EOG2','EMG2'])
    # Specify EEG, EOG, and EMG channels to use for sleep scoring
    eeg = 'Fp2'
    eog = 'EOG2'
    emg = 'EMG2'
elif electrodes_side == 'left':
    # Get the relevant channel subset
    raw = raw_original.pick_channels(['Fp1','EOG1','EMG1','A2'])
    # Define EEG, EOG, and EMG channels to use for sleep scoring
    eeg = 'Fp1'
    eog = 'EOG1'
    emg = 'EMG1'
    # A1 is the system reference, so for right-side electrodes, it is correct. For left-side electrodes, we change the reference to the contralateral A2
    raw, _ = mne.set_eeg_reference(raw, ref_channels=['A2'])

# Downsample to 100 data points per second, for faster computation
new_sampling_rate = 100
raw.resample(new_sampling_rate)

# Apply bandpass filters
raw.filter(0.1, 45) # high-pass = 0.1 Hz; low-pass = 45 Hz

# Print dataset infos to confirm
print(raw.info)



# %% Sleep staging + plot

# Create object based on electrodes on right side of the head
sls = yasa.SleepStaging(raw, eeg_name=eeg, eog_name=eog, emg_name=emg, metadata=dict(age=age, male=male))

# Predict the sleep stages using YASA algorithm; by default, sleep data is divided into 30 second epochs, so this array has one value per 30 seconds of data
hypno_pred = sls.predict()

# Convert stage names into numbers: wake=0, N1=1, N2=2, N3=3, REM=4
hypno_pred = yasa.hypno_str_to_int(hypno_pred)

# Upsample hypnogram to 100 Hz for plot
hypno_up = yasa.hypno_upsample_to_data(hypno_pred, sf_hypno=1/30, data=raw)

# Get raw data for spectrogram
data_eeg = raw.get_data(eeg) * 1e6

# Plot hypnogram + spectrogram
yasa.plot_spectrogram(data_eeg[0], new_sampling_rate, hypno_up, fmin=0.5, fmax=25)

# Save plot
plt.savefig(filename_hypnogram)



# %% Sleep metrics

# Predicted probabilities of each sleep stage at each epoch
sls.predict_proba()

# Extract a confidence level (ranging from 0 to 1) for each epoch; plot how it changes overnight
confidence = sls.predict_proba().max(1)
plt.plot(confidence)

# Sleep statistics (definitions: https://raphaelvallat.com/yasa/build/html/generated/yasa.sleep_statistics.html)
stats = yasa.sleep_statistics(hypno_pred, sf_hyp=1/30)

# Convert dict to pandas dataframe
stats = pd.DataFrame.from_dict(stats, orient='index')

# Save sleep statistics as CSV
stats.to_csv(filename_stats, header=False)

# Sleep stages transition matrix
# NOTE: probs is the probability transition matrix, i.e. given that the current sleep stage is A, what is the probability that the next sleep stage is B
counts, probs = yasa.transition_matrix(hypno_pred)
print(probs.round(2)) # print the matrix, rounded to 2 decimal values

