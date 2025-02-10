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
    df = copy.deepcopy(behavioral_data.iloc[0:n_frames])
    df['onset'] = fiber_data.epocs.Cam1.onset
    df['offset'] = fiber_data.epocs.Cam1.offset
    
    df.to_csv(os.path.join(destination_path,file_name))
    
    return df