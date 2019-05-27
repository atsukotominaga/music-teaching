# The Sound of Teaching Music project
This repo contains scripts and materials necessary for a Musical experiment with expert pianists.

### Open Science Framework
[https://osf.io/35ryj/](https://osf.io/35ryj/)


## Overview
- [Script](#Script)
    + [Max](#Max)
    + [R](#R)
    + [Rmd](#Rmd)
    + [Jupyter notebook](#Jupyter-notebook)
    + [Lilypond](#Lilypond)
    
- [Material](#Material)
    + Instruction (psd)
    + Checklist (xlsx)

# Script
## Environment
All of the programme scripts were written and tested in the following environment.

- ProductName:	Mac OS X  
- ProductVersion:	10.14.3

## Max
> For an experimetal programme.

### Prerequisite
- Install the Shell package (https://github.com/jeremybernstein/shell/releases) into the package folder of Max 7.
- The Shell package is used to generate necessary folders to store collected data. If it does not work in your environment, please create the following 4 folders manually.
    + data (path: ~/script/Max/data)
        + practice (path: ~/script/Max/data/practice) - within the data folder
    + midi (path: ~/script/Max/midi)
        + practice (path: ~/script/Max/midi/practice) - within the midi folder
        
### Patchers
- mainPatch.maxpat: for the experiment.
- practicePatch.maxpat: for practice trials.
- imagePatch.maxpat: display stimuli on the screen in front of a participant.

### SubPatchers
- MIDInoteRecording: acquire and save MIDI data.
- dataFileNaming: create a file name.
- condFileReading: read a condition file to determine the order of conditions for each participant.

### Others
- image: contains experimental stimuli
- cond: contains a csv file and an excel file about the order of conditions for each participant.

## R
> For data analysis

- 1_filtering.R: clean data and remove performance errors. >> outputs: 1_filtered folder
    + In the original study, there was an coding error in Max for labelling the name of skills (either articulation or dynamics). This was corrected based on what was actually displayed to participants by checking the name of a stimulus image. See line 65.
- 2_trimming.R: calculate dependent variables and remove outliers. >> outputs: 2_trimmed folder
- ioi_art.R: data analysis for IOIs with articulation **>> outputs: 3_stats folder
- ioi_dyn.R: data analysis for IOIs with dynamics >> outputs: 3_stats folder
- kot_art.R: data analysis for KOT with articulation >> outputs: 3_stats folder
- kot_dyn.R: data analysis for KOT with dynamics >> outputs: 3_stats folder
- vel_art.R: data analysis for KV with articulation >> outputs: 3_stats folder
- vel_art.R: data analysis for KV with dynamics >> outputs: 3_stats folder
- ideal.csv: ideal performance data / used to remove performance errors.

## Rmd
> For generating an instruction sheet

- instruction.Rmd: RMarkdown for a instruction sheet for an experimenter >> output: html file
- image: figures used in the instruction sheet

## Jupyter notebook
> For generating stimuli

- image_generator.ipynb: Python script to generate experimental stimuli (see more information on jupyter notebook) >> outputs: stimuli and practice folders
- image: used for stimuli generation

## Lilypond
> For generating sheet music (output:pdf)

- ly files: sheet music compiled by lilypond >> outputs: pdf files

# Material

- instruction: psd files for making instruction images >> outputs: png files
- cropped: original sheet music
- checklist.xlsx: a list to check each procedure during testing (for an experimenter)
