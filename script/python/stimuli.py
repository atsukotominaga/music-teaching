#!/usr/local/bin/python

# Created: 12/06/2019
# This script generates images for each trial
# Outputs: trialNr(1:8)_cond(t, p)_stim_(a, d, m).png

# Import necessary packages
import os
from PIL import Image, ImageDraw, ImageFont

# Create a stimuli folder if not exist (save created stimuli into the folder)
if not os.path.exists("stimuli"):
    os.makedirs("stimuli")
    

# Create a black screen
bg = Image.new("RGB",(2560, 1440),"black") # Black 2560 px x 1440 px
savename = "./stimuli/black.png"
bg.save(savename, quality = 100)

ls_file = os.listdir("./cropped/") # Read the list of image files

for image in ls_file:
    fontpath = "System/Library/Fonts/HelveticaNeue.ttc" # !!!Set the location of a font file!!!
    font = ImageFont.truetype(fontpath, 120) # Fonttype and fontsize
    bg = Image.new("RGB",(2560, 1440),"black") # Background - Black 2560 px x 1440 px
    wh = Image.new("RGB",(2250, 600), "white") # White square 2250 px x 600px
    imgname = "./cropped/" + image
    img = Image.open(imgname) # Open a stimulus image
    # Make copies of each image and combine them for each condition
    stim = bg.copy()
    stim.paste(wh.copy(), (154, 440))
    img_copy = img.copy()
    stim.paste(img_copy, (154, 520))
    for cond in ["teaching", "performing"]: # Each condition
        for i in range(8): # Each trial
            stim_current = stim.copy()
            draw_current = ImageDraw.Draw(stim_current)
            trial = str(i+1) + " / 8 trial"
            if cond == "teaching":
                draw_current.text((590, 250),"Do your best as a teacher", 
                                  fill = "yellow", font = font) # teacher
                draw_current.text((1050, 1100), trial, fill = "yellow", font = font) # trial
                savename = "./stimuli/" + str(i+1) + "_" + 't' + "_" + image
                stim_current.save(savename, quality = 100)
            elif cond == "performing":
                draw_current.text((540, 250),"Do your best as a performer", 
                                  fill = "yellow", font = font) # performer
                draw_current.text((1050, 1100), trial, fill = "yellow", font = font) # trial
                savename = "./stimuli/" + str(i+1) + "_" + 'p' + "_" + image
                stim_current.save(savename, quality = 100)

print("Done!")

