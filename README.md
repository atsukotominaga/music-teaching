This repo contains scripts and materials necessary for a musical experiment with expert pianists.

Open Science Framework: [https://osf.io/8nbjh/](https://osf.io/8nbjh/) (Raw data are available here)

# Overview
- [Script](#Script)
    + [Max (Experiment)](#Max)
    + [R (Filtering/Trimming)](#R)
    + [R Notebook (Data analysis)](#R-Notebook)
   
- [Material](#Material)
    + [Instruction](#Instruction)
    + [Lilypond (Stimuli - sheet music)](#Lilypond)
    + [Jupyter Notebook (Stimuli - image processing)](#Jupyter-notebook)
    + [Others](#Others)

# Script
## Environment
The OS version used for the experimental programme:
- ProductName: Mac OS X  
- ProductVersion: 10.14.3

Others:
- ProductName:	Mac OS X
- ProductVersion:	10.15.7


## Max
- Version 7
### Prerequisite
- Install the Shell package (https://github.com/jeremybernstein/shell/releases) into the package folder of Max 7.

- The Shell package is used to generate necessary folders to store collected data. If it does not work in your environment, please create the following 4 folders manually.
    + data (path: ~/script/Max/data)
        + practice (path: ~/script/Max/data/practice) - within the data folder
    + midi (path: ~/script/Max/midi)
        + practice (path: ~/script/Max/midi/practice) - within the midi folder
        
### Main Patchers
- `mainPatch.maxpat`: for the experiment
- `practicePatch.maxpat`: for practice trials
- `imagePatch.maxpat`: to display stimuli on the screen in front of a participant

### SubPatchers (used by Main Patchers)
- `MIDInoteRecording.maxpat`: to acquire and save MIDI data
- `dataFileNaming.maxpat`: to create a file name
- `condFileReading.maxpat`: to read a condition file to determine the order of conditions for each participant

### Others
- `image`: experimental stimuli
- `cond`: csv file and an excel file about the order of conditions for each participant

## R
### 1. Preprosessor
You need *raw_data* to run the scripts below (get the original data from [OSF](https://osf.io/8nbjh/))

- `filtering.R`: to clean data and remove performance errors >> output: filtered folder
    + *In the original study, there was an coding error in Max for labelling the name of skills (either articulation or dynamics). This was corrected based on what was actually displayed to participants by checking the name of a stimulus image. See line 48.*
- `trimming.R`: to calculate dependent variables and remove outliers >> output: trimmed folder
- `function.R`: to detect pitch errors in a performance / insert NAs
- `ideal.txt`: the ideal sequence of the piece (used for filtering)
- `error_summary.Rmd`: summary for errors >> html file

### 2. Analysis
- `ioi.Rmd`: analysis and plots for IOIs >> output: html file
- `articulation.Rmd`: analysis and plots for KOT/KOR >> output: html file
- `dynamics.Rmd`: analysis and plots for KV/KV-Diff >> output: html file

You need *questionnaire.csv* to run the script below (get the file from [OSF](https://osf.io/8nbjh/)).
- `questionnaire.Rmd` >> output: html file

# Material
## Instruction
- `instruction.Rmd`: instruction sheet for an experimenter >> output: html file
- `image`: figures used in the instruction sheet
- `psd files`: images used in the instruction >> output: png files

## Lilypond
- `ly files`: sheet music compiled by lilypond >> output: pdf files

## Jupyter Notebook
- `image_generator.ipynb`: to generate experimental stimuli (see more information on jupyter notebook) >> output: stimuli and practice folders
- `image`: used for stimuli generation

## Others
- `cropped`: original sheet music
- `checklist.xlsx`: a list to check each procedure during testing (for an experimenter)
