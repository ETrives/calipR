import os
import pandas as pd
import tdt
import numpy as np

# Function to retrieve paths for all tanks (directories containing tdt data for a given animal) in a directory:

def extractFiles(path):
    path_list = []
    for i in os.listdir(path):
        path_list.append(os.path.join(path,i))
    return path_list

# Function to extract tdt data from one tank (one animal) in a folder.
# This function outputs a dataframe containing 3 columns : the isosbestic trace,
# the calcium trace and the animal ID

def extract_tdt_data(path, isos_name = '_405A', ca_name = '_465A'):
    data = tdt.read_block(path)
    isos = data.streams[isos_name]
    ca = data.streams[ca_name]
    mouse = data.info.blockname
    frame_rate = data.streams[isos_name].fs
    time = np.linspace(0,len(data.streams[isos_name].data),len(data.streams[isos_name].data)) / frame_rate
    df = pd.DataFrame({"CA_TRACE" : ca.data, "ISOS_TRACE" : isos.data, "ID" : mouse, "TIME_SECONDS" : time})
    return df


# Function to extract all relevant tdt data from all tanks in a folder.
# This function outputs a dataframe containing 3 columns : the isosbestic trace for each animal, the calcium trace for each animal and the animal IDs

def extract_all_tdt_data(path, isos_name = '_405A', ca_name = '_465A', file_name = 'full_Extracted_TDT_Data.csv', destination_path = 1):
    
    path_list = extractFiles(path)
    df_list = []
    
    for i in path_list:
        df_list.append(extract_tdt_data(i, isos_name, ca_name))
    
    full = pd.concat(df_list)
    
    if destination_path == 1:
        destination_path = path 
    
    full.to_csv(os.path.join(destination_path,file_name))
    
    return full
