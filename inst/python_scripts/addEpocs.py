import os
import tdt
import pandas as pd
import copy


# Function to merge video and fiber data:

# Add two columns to the behavioral data : 1 is the onset and the other is the offset

def addEpocs(path_to_fiber_data, behavioral_data, file_name = "Behavioral_Data.csv", destination_path = 1):
    
    fiber_data = tdt.read_block(path_to_fiber_data)
    
    if destination_path == 1:
        destination_path = path_to_fiber_data
        
    n_frames = len(fiber_data.epocs.Cam1.onset)
    behavior_n_frames = len(behavioral_data[["ID"]])

    if len(behavioral_data[["ID"]]) > n_frames:
        df = copy.deepcopy(behavioral_data.iloc[0:n_frames])

    else:
        df = copy.deepcopy(behavioral_data)

    df['onset'] = fiber_data.epocs.Cam1.onset[0:behavior_n_frames]
    df['offset'] = fiber_data.epocs.Cam1.offset[0:behavior_n_frames]
    
    df.to_csv(os.path.join(destination_path,file_name))

    return df