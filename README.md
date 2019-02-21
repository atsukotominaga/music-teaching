# Overview
This repo contains scripts and materials necessary for a Musical experiment with expert pianists.
- script
    + Max 7 for an experimental programme
    + R for data analysis
    + Rmd for generating an instruction sheet for an experimenter (html)
    + Jupyter notebook for generating stimuli (png)
    + Lilypond for generating sheet music (pdf)
    
- material
    + Photoshop files for stimuli (psd)
    + Checklist (xlsx)

## Project
### Title
The Sound of Teaching Music: Experts’ sound modulation for novices

### Open Science Framework
[https://osf.io/wu5ge/](https://osf.io/wu5ge/)

## Script
### Environment
All of the programme scripts were written and tested in the following environment.

ProductName:	Mac OS X  
ProductVersion:	10.14.3

### Max
#### Prerequisite
- Install the Shell package (https://github.com/jeremybernstein/shell/releases) into the package folder of Max 7.
- The Shell package is used to generate necessary folders to store collected data. If it does not work in your environment, please create the following 4 folders manually.
    + data (path: ~/script/Max/data)
        + practice (path: ~/script/Max/data/practice) - within the data folder
    + midi (path: ~/script/Max/midi)
        + practice (path: ~/script/Max/midi/practice) - within the midi folder
        
#### Patchers
- mainPatch.maxpat: the main Max patcher for the experiment.
- practicePatch.maxpat: the Max patcher for practice trials.
- imagePatch.maxpat: the Max patcher to show stimuli on the screen in front of a participant.

#### SubPatchers
- MIDInoteRecording: acquire and save MIDI data.
- dataFileNaming: create a file name.
- condFileReading: read a condition file to determine the order of conditions for each participant.

#### Others
- image: contains experimental stimuli
- cond: contains a csv file and an excel file about the order of conditions for each participant.

### R
- cleaning.R: clean data and remove performance errors.
- data_analysis.R: aggregate data and plot them. (+ Stats / not yet)
- ideal.csv: ideal performance data / used to remove performance errors.

### Rmd
- instruction.Rmd: RMarkdown for a instruction sheet for an experimenter (html)

### Jupyter notebook
- image_generator.ipynb: Python script to generate experimental stimuli (png; see more information on jupyter notebook)
- image: used for stimuli generation

### Lilypond
- ly files: sheet music compiled by lilypond (pdf)

## Material
### Photoshop
- instruction: contains psd files for instructions for participants

### Checklist
- for an experimenter to check each procedure during testing