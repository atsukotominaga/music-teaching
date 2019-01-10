# Overview
This repo contains scripts and materials necessary for a Musical experiment with expert pianists.
- script
    + Max 7 for an experimental programme
    + R for data analysis
    + Rmd for generating an instruction sheet (html)
    + Jupyter notebook for generating stimuli (png)
    + Lilypond for generating sheet music (pdf)
    
- material
    + Photoshop files for stimuli (psd)

## Project
### Title
Teaching behaviour in musical skill transmission

### Preregistration
TBC

## Script
### Environment
All of the programme scripts were tested in macOS 10.14.2.

### Max
#### Prerequisite
- Install the Shell package (https://github.com/jeremybernstein/shell/releases) into the package folder of Max 7.
- The Shell package is used to generate necessary folders to store data. If it does not work in your environment, please create the following 4 folders manually.
    + data (path: ~/script/Max/data)
        + practice (path: ~/script/Max/data/practice) - in the data folder
    + midi (path: ~/script/Max/midi)
        + practice (path: ~/script/Max/midi/practice) - in the midi folder
        
#### Patchers
- mainPatch.maxpat: the main Max patcher for the experiment.
- practicePatch.maxpat: the Max patcher for practice trials.
- imagePatch.maxpat: the Max patcher to show stimuli on the screen in front of the piano.