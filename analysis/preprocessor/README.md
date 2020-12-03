# Workflow

1. filter with `filtering.R`
2. check the details of errors and remove some participants if needed (this script: `error_summary.Rmd`)
3. trim data (i.e., remove outliers) with `trimming_ioi.R`, `trimming_kot_kor.R`, `trimming_vel.R`
4. check trimmed data and remove some participants if needed (`included_summary.Rmd`)

### Step1 - filtering:
script: `filtering.R`
- filtering for both onset and offset
- 3 steps
1. Check whether the current performance is identical to the ideal
2. Manually remove errors using editData function (**therefore, the process is not reproducible**)
    - if there are extra notes, remove them by looking at the plot and the Diff column.
    - if there are missing notes, insert NA to that missing location.
    - if participants accidentally hit a wrong key instead of the correct one, replace that with NA.
    - if there are complicated errors, look at the data and listen to the midi performance.

#### Outputs
1. Onsets
-  dt_correct_onset.txt: all valid trials for onsets
-  dt_correct_onset_1.txt: trials without error
- dt_correct_onset_2.txt: corrected trials because of extra notes
- dt_correct_onset_3.txt: corrected trials because of missing notes
- dt_correct_onset_4.txt: corrected trials because of substituted notes
- dt_correct_onset_5.txt: corrected trials because of idiosyncratic errors
- error_onset.txt: info about corrected and excluded trials

2. Offsets
- Same as onsets

3. missingTrials.txt: info about missing trials

### Step2 -trimming:
scripts: `trimming_ioi.R`, `trimming_kot_kor.R`, `trimming_vel.R`
#### IOI
- 3 options
1. Remove outliers outside 3SD across the conditions (strict)
2. Remove outliers outside per condition
    - there was little difference between 1 and 2
3. Remove outliers separately whether normIOIs are located on Boundary or not (loose)

I chose option 1 for data analysis.

#### KOT/KOR
Since each participant played the piece in a different tempo, KOR would be more suitable for analysis. However, I also calculated and removed outliers for KOT as well. I chose option 1 for IOIs to calculate mean IOIs for KOR computation.
- 1 option
1. Remove outliers outside per subcomponent

#### VEL/DIFF
- 1 option
1. Remove outliers outside per subcomponent
