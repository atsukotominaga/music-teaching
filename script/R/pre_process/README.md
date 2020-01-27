# Memo
- Please also consult with comments on each script.

### Step1 - filtering:
- filtering for both onset and offset
- 3 steps
1. Check whether the current performance is identical to the ideal
2. Manually remove errors using editData function
    - if there are extra notes, remove them by looking at the plot and the Diff column.
    - if there are missing notes, insert NA to that missing location.
    - if participants accidentally mishit a key, replace that with NA.
    - if there are complicated errors, look at the data and listen to the midi performance.

#### Outputs
df_correct_onset.csv, df_correct_offset.csv >> final outputs which can be used for data analysis

others >> corrected data for each error category (e.g., extra notes, missing notes)

### Step2 -trimming:
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
