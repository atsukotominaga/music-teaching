This repo contains scripts and materials necessary for a musical experiment with expert pianists.

Open Science Framework: [https://osf.io/uemk5/](https://osf.io/uemk5/) (Raw data are available in OSF)

# Overview
- [Script](#script)
    + [Max (Experiment)](#Max)
    + [R (Filtering/Trimming)](#R)
    + [R Notebook (Data analysis)](#R-Notebook)

- [Material](#material)
    + [Instruction](#Instruction)
    + [Lilypond (Stimuli - sheet music)](#Lilypond)
    + [Python (Stimuli - image processing)](#Python)
    + [Others](#Others)

# Script
## Environment
All of the programme scripts were written and tested in the following environment.

- ProductName: Mac OS X  
- ProductVersion: 10.14.3

## Max (version 8)
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
You need raw_data folder to run the scripts below (get the folder from [OSF](https://osf.io/uemk5/))
### preprocessor
- filtering.R: removing pitch errors >> outputs: csv files
- trimming.R (ioi, kot/kor, vel): removing outliers >> outputs: csv/png files

### analysis
- export.R (extract R scripts from R Notebook) >> R file

### condfile
counterbalance sheet generator >> output: csv file

## R Notebook
You need filtered and trimmed folders to run the scripts below (outputs from filtering.R and trimming.R)
- ioi_1.Rmd: analysis and plots for IOIs >> output: html file
    + outliers: 1. > +- 3SD across the conditions (all together; see [detail](https://github.com/atsukotominaga/teaching-v2.0/tree/master/script/R/preprocessor))
- ioi_3.Rmd: analysis and plots for IOIs >> output: html file
    + outliers: 3. > +- 3SD actross the conditions (separately for Subcomponent Change; see [detail](https://github.com/atsukotominaga/teaching-v2.0/tree/master/script/R/preprocessor))
- articulation.Rmd: analysis and plots for KOT/KOR >> output: html file
- dynamics.Rmd: analysis and plots for KV/KV-Diff >> output: html file

You need questionnaire.csv to run the script below (get the file from [OSF](https://osf.io/uemk5/)).
- questionnaire.Rmd >> output: html file

# Material
## Instruction
- instruction.Rmd: a instruction sheet for an experimenter >> output: html file
- image: figures used in the instruction sheet
- psd files: images used in the instruction >> output: png files

## Lilypond
- ly files: sheet music compiled by lilypond >> output: pdf files

## Python
- stimuli.py: generate experimental stimuli >> output: stimuli and practice folders
- cropped: used for stimuli generation

## Others
- cropped: original sheet music
- checklist.xlsx: a list to check each procedure during testing (for an experimenter)
