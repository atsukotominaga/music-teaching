#!/usr/local/bin/python

# Created: 12/06/2019
# This script generates images for each trial

#%% Experimental Stimuli
# Outputs: trialNr(1:8)_cond(t, p)_stim_(a, d, m).png

# Import necessary packages
import os
from PIL import Image, ImageDraw, ImageFont

#%%
# Create a stimuli folder if not exist (save created stimuli into the folder)
if not os.path.exists("stimuli"):
    os.makedirs("stimuli")
    

# Create a black screen
bg = Image.new("RGB",(2560, 1440),"black") # Black 2560 px x 1440 px
savename = "./stimuli/black.png"
bg.save(savename, quality = 100)

ls_file = [f for f in os.listdir("./cropped/") if f.endswith('.png')] # Read the list of image files

for image in ls_file:
    fontpath = "System/Library/Fonts/HelveticaNeue.ttc" # !!!Set the location of a font file!!!
    font = ImageFont.truetype(fontpath, 120) # Fonttype and fontsize
    bg = Image.new("RGB",(2560, 1440),"black") # Background - Black 2560 px x 1440 px
    imgname = "./cropped/" + image
    img = Image.open(imgname) # Open a stimulus image
    # Make copies of each image and combine them for each condition
    stim = bg.copy()
    img_copy = img.copy()
    stim.paste(img_copy, (154, 320))
    for cond in ["teaching", "performing"]: # Each condition
        for i in range(8): # Each trial
            stim_current = stim.copy()
            draw_current = ImageDraw.Draw(stim_current)
            trial = str(i+1) + " / 8 trial"
            if cond == "teaching":
                draw_current.text((590, 100),"Do your best as a teacher", 
                                  fill = "yellow", font = font) # teacher
                draw_current.text((1050, 1200), trial, fill = "yellow", font = font) # trial
                savename = "./stimuli/" + str(i+1) + "_" + 't' + "_" + image
                stim_current.save(savename, quality = 100)
            elif cond == "performing":
                draw_current.text((540, 100),"Do your best as a performer", 
                                  fill = "yellow", font = font) # performer
                draw_current.text((1050, 1200), trial, fill = "yellow", font = font) # trial
                savename = "./stimuli/" + str(i+1) + "_" + 'p' + "_" + image
                stim_current.save(savename, quality = 100)

print("Stimuli: Done!")

#%% Practice Stimuli
# Outputs: prac_stim_(a, d, m, n).png

ls_file = os.listdir("./cropped/practice/") # Read the list of image files

# Create a practice folder if not exist (save created pictures into the folder)
if not os.path.exists("practice"):
    os.makedirs("practice")
    
for image in ls_file:
    fontpath = "System/Library/Fonts/HelveticaNeue.ttc" # !!!Set the location of a font file!!!
    font = ImageFont.truetype(fontpath, 120) # Fonttype and fontsize
    bg = Image.new("RGB",(2560, 1440),"black") # Background - Black 2560 px x 1440 px
    imgname = "./cropped/practice/" + image
    img = Image.open(imgname) # Open a stimulus image
    # Make copies of each image and combine them for each condition
    stim = bg.copy()
    img_copy = img.copy()
    stim.paste(img_copy, (154, 320))
    if image == "stim_n.png":
        draw_current = ImageDraw.Draw(stim)
        draw_current.text((300, 120),"Please practice the piece as notated.", fill = "yellow", font = font)
        savename = "./practice/" + "prac_" + image
        stim.save(savename, quality = 100)
    else:
        savename = "./practice/" + "prac_" + image
        stim.save(savename, quality = 100)

print("Practice: Done!")