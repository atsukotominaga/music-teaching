This repo contains scripts and materials necessary for a musical experiment with expert pianists.

Open Science Framework: [https://osf.io/35ryj/](https://osf.io/35ryj/) (Raw data are available in OSF (Experiment 2))

# Overview
- [Script](#script)
    + [Max (Experiment)](#Max)
    + [R (Filtering/Trimming)](#R)
    + [R Notebook (Data analysis)](#R-Notebook)
    + [Lilypond (Stimuli - sheet music)](#Lilypond)
    + [Python (Stimuli - image processing)](#Python)

- [Material](#material)
    + [Instruction](#Instruction)
    + [Others](#Others)

# Script
## Environment
All of the programme scripts were written and tested in the following environment.

- ProductName: Mac OS X  
- ProductVersion: 10.14.3

## Max
### Prerequisite
- Install the Shell package (https://github.com/jeremybernstein/shell/releases) into the package folder of Max 8.
- The Shell package is used to generate necessary folders to store collected data. If it does not work in your environment, please create the following 4 folders manually.
    + data (path: ~/script/Max/data)
        + practice (path: ~/script/Max/data/practice) - within the data folder
    + midi (path: ~/script/Max/midi)
        + practice (path: ~/script/Max/midi/practice) - within the midi folder
   
### Patchers
- EXPERIMENT.maxpat: controller for all patchers
- main.maxpat: for the experiment
- practice.maxpat: for practice trials
- display.maxpat: display stimuli on the screen in front of a participant

### SubPatchers
- sub(pra)MIDIRec.maxpat: acquire and save MIDI data
- sub(pra)FileName.maxpat: create a file name
- sub(pra)Cond.maxpat: read a condition file to determine the order of conditions for each participant

### Others
- stimuli: contains experimental stimuli
- cond: contains a csv file and an excel file about the order of conditions for each participant

## R
### pre_process
- filtering.R: removing pitch errors >> outputs: csv files
- trimming.R (ioi, kot/kor, vel): removing outliers >> outputs: csv/png files

### condfile
counterbalance sheet generator >> output: csv file

## R Notebook
You need filtered and trimmed folders to run the scripts below (outputs from filtering.R and trimming.R)
- ioi.Rmd: analysis and plots for IOIs >> output: html file
- articulation.Rmd: analysis and plots for KOT/KOR >> output: html file
- dynamics.Rmd: analysis and plots for KV/KV-Diff >> output: html file

## Lilypond
- ly files: sheet music compiled by lilypond >> output: pdf files

## Python
- stimuli.py: generate experimental stimuli >> output: stimuli and practice folders
- cropped: used for stimuli generation

# Material
## Instruction
- instruction.Rmd: a instruction sheet for an experimenter >> output: html file
- image: figures used in the instruction sheet
- psd files: images used in the instruction >> output: png files

## Others
- cropped: original sheet music
- checklist.xlsx: a list to check each procedure during testing (for an experimenter)
