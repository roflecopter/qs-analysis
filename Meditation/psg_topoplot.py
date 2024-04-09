import mne
import yasa
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

bdf_files = [
    # ('#1 5m REST','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-03-24_15-11-04-max-OBCI_F9.TXT.bdf', 60, 60+300, 'Pz'),
    # ('#1 5-10m REST','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-03-24_15-11-04-max-OBCI_F9.TXT.bdf', 640-300, 640, 'Pz'),
    # ('#1 5m NSDR','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-03-21_21-47-20-max-OBCI_F8.TXT.bdf', 82, 82+300, 'Pz'),
    # ('#1 5-10m NSDR','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-03-21_21-47-20-max-OBCI_F8.TXT.bdf', 672-300, 672, 'Pz'),
    # ('#2 5m REST','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-03-24_21-21-43-max-OBCI_FA.TXT.bdf', 60, 60+300, 'Pz'),
    # ('#2 5-10m REST','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-03-24_21-21-43-max-OBCI_FA.TXT.bdf', 640-300, 640, 'Pz'),
    # ('#2 5m NSDR','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-03-24_21-40-11-max-OBCI_FB.TXT.bdf', 86, 86+300, 'Pz'),
    # ('#2 5-10m NSDR','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-03-24_21-40-11-max-OBCI_FB.TXT.bdf', 640-300, 640, 'Pz'),
    ('#3 5m REST','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-08_19-28-26-max-OBCI_FD.TXT.bdf', 112, 112+300, 'Oz'),
    ('#3 5-10m REST','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-08_19-28-26-max-OBCI_FD.TXT.bdf', 680-300, 680, 'Oz'),
    # ('#3 5m NSDR','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-08_19-50-06-max-OBCI_FE.TXT.bdf', 60, 60+300, 'Oz'),
    # ('#3 5-10m NSDR','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-08_19-50-06-max-OBCI_FE.TXT.bdf', 600-300, 600, 'Oz'),
    ('#3 5m Chant','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-08_20-10-42-max-OBCI_FF.TXT.bdf', 90, 90+300, 'Oz'),
    ('#3 5-10m chant','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-08_20-10-42-max-OBCI_FF.TXT.bdf', 690-300, 690, 'Oz'),
    # ('#4 5m REST ','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-09_11-38-14-max-OBCI_01.TXT.bdf', 50, 50+250, 'M1'),
    # ('#4 5m NSDR ','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-09_11-38-14-max-OBCI_01.TXT.bdf', 360, 360+300, 'M1'),
    # ('#4 5-10m NSDR ','/Volumes/Data/Storage/Self/!raw-1mOBCI/2024-04-09_11-38-14-max-OBCI_01.TXT.bdf', 860-300, 860, 'M1'),
    ]

# raw = mne.io.read_raw_bdf(bdf_files[len(bdf_files)-1][1], preload=True, verbose=True)
# raw.plot()
# p_type = 'Absolute'

p_type = 'Relative'
bp_bands = [
    (1, 4, "Delta"),
    (4, 8, "Theta"),
    (8, 12, "Alpha"),
    (12, 16, "Sigma"),
    (16, 30, "Beta"),
    (30, 45, "Gamma 1"),
    (55, 80, "Gamma 2")
]

fig, axes = plt.subplots(len(bdf_files), len(bp_bands), 
     figsize=(len(bp_bands)*4, len(bp_bands)*4)); axes = axes.flatten()
old_fontsize = plt.rcParams["font.size"]
plt.rcParams.update({"font.size": 24})
fig.suptitle(p_type + ' Band Power')

for index, bdf_file in enumerate(bdf_files):
    ref = bdf_file[4]
    raw = mne.io.read_raw_bdf(bdf_file[1], preload=True, verbose=True)
    ch = raw.ch_names.copy()
    ch = [x.replace('-'+ref, '') for x in ch]
    raw.rename_channels(dict(zip(raw.ch_names, ch)))
    ch.remove('ACC_X'); ch.remove('ACC_Y'); ch.remove('ACC_Z')
    raw.pick(ch)
    raw.notch_filter(freqs=50, notch_widths=1, n_jobs=4)
    raw.filter(.5, 100, n_jobs=4)
    raw.crop(tmin=bdf_file[2], tmax=bdf_file[3])
    
    raw.set_eeg_reference(ref_channels = 'average')
    ch = raw.ch_names
    ten_twenty_montage = mne.channels.make_standard_montage('standard_1020')
    raw.set_montage(ten_twenty_montage  , match_case=False, on_missing="ignore")
    bp_r = yasa.bandpower(raw.get_data(units='uV'), raw.info["sfreq"], ch_names = raw.ch_names, bandpass=True, relative=True, bands=bp_bands)
    bp_a = yasa.bandpower(raw.get_data(units='uV'), raw.info["sfreq"], ch_names = raw.ch_names, bandpass=True, relative=False, bands=bp_bands)
    
    for r in [p_type]:
        for i in range(len(bp_bands)):
            if r == 'Relative': 
                bp = bp_r 
            else:
                bp = bp_a
            p_band = bp.columns[i]
            data = pd.Series(bp[p_band], index = ch)
            axe = axes[index*len(bp_bands) + i]
            mne.viz.plot_topomap(data, pos=raw.info, names=ch, size = 20, show=False, axes=axe, vlim=(min(data), max(data)), cmap='RdBu_r', cnorm=None)
            axe.set_title([p_band if i != 0 else bdf_file[0] + ' ' + p_band][0])
    plt.tight_layout()
plt.show()
plt.rcParams.update({"font.size": old_fontsize})