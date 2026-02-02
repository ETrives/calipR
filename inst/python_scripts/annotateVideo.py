
# Function to launch a video and annotate it.
# It is a programmable video player with 3 annotation keys that can be entered (e.g. 'm', 'e', 't').
# On top of these keys, the space bar pauses the video, the '+' accelerates it or allows to relaunch it after a pause. The '-' allows to decrease the # speed and the 'r' allows to return 100 frames before if you missed an event or if you want to reannotate this portion.

# It returns a list containing all annotations ('nothing' if no pressed key and the corresponding key when it is pressed)

# it requires cv2 :

import cv2
import os
import pandas as pd

def extractAllVideoPath(path):
    
    path_list = extractFiles(path)
    video_path = []
    for i in path_list:
        for file in os.listdir(i):
            if file.endswith(".avi"):
                video_path.append(os.path.join(i,file))
           
    return video_path




def annotateVideo(path, destination_path, key_list, state_list, speed = 20):
    
    init_speed = speed
    current_speed = speed
    cap = cv2.VideoCapture(path)

    current_state = ['NA' for i in range(0,len(key_list))]
    annotation_lists = [[] for i in range(0,len(key_list))]
    frame_number = 0
    key_counter = [1 for i in range(0,len(key_list))]

    while(True):
        ret, frame = cap.read()
        if not ret:
            break
        
        cv2.imshow('frame', frame)
        
        pressedKey = cv2.waitKey(speed)

        for key in range(0,len(key_list)):
            if state_list[key] == False and current_state[key] ==  {key_list[key] : 1}:
                current_state[key] = 'NA'

            if pressedKey == ord(key_list[key]) and state_list[key] == True:
                key_counter[key] = key_counter[key] + 1
                if key_counter[key] % 2 == 0:
                    current_state[key] = {key_list[key] : 'start'}
                    print(current_state)
                else:
                    current_state[key] = 'NA'
                    print(current_state)
            if pressedKey == ord(key_list[key]) and state_list[key] == False:
                current_state[key] = {key_list[key] : 1}
                print(current_state)
            annotation_lists[key].append(current_state[key])
        if pressedKey == ord('+'):
            if speed > 5:
                 speed = speed - 5
            if speed == 0:
                if current_speed != 0:
                    speed = current_speed
                else:
                    speed = init_speed
        if pressedKey == ord('-'):
            speed = speed + 5
        if pressedKey == ord(' '):
            current_speed = speed
            speed = 0
        if pressedKey == ord('r'):
            if frame_number <= 100 :
                frame_number = 0
                annotation_lists = [[] for i in range(0,len(key_list))]
                cap.set(cv2.CAP_PROP_POS_FRAMES, frame_number)
                current_state = ['NA' for i in range(0,len(key_list))]
                
            else:
                frame_number = frame_number -100
                for annotations in range(0,len(key_list)):
                    annotation_lists[annotations] = annotation_lists[annotations][0:frame_number]
                    
                cap.set(cv2.CAP_PROP_POS_FRAMES, frame_number)
                current_state = ['NA' for i in range(0,len(key_list))]
        if pressedKey == ord('q'):
            break
        
        frame_number = frame_number+1
    cap.release()
    cv2.destroyAllWindows()
    
    df_list = [[] for i in range(0,len(key_list))]
    
    for i in range(len(key_list)):
        df_list[i] = pd.DataFrame({key_list[i] : annotation_lists[i]})
        
    final_df = pd.concat(df_list, axis = 1)
    final_df = final_df.applymap(lambda x: list(x.values())[0] if isinstance(x, dict) else x)
    final_df.to_csv(destination_path)

    return final_df

