
# CAP analysis files for risky decision-making task
Brief description of each file in this directory listed below

Any files added to this folder should be included in this list along with the relative link (./) of that file

## 1) [RDMsetup.R](./RDMsetup.R)
-   Making datasets analysis ready!
-   Including applying exclusion criteria, dealing with missed trials and creating variables (e.g. past outcome)
#### Output:
  ##### 1)  RDM_subID_missTri_totTri.csv
   - 544 rows (for each participant) and 9 variables that include: 
     - subject ID
     - missed gain trials phase 1
     - missed gain trials phase 2
     - total gain trials phase 1
     - total gain trials phase 2   
     - missed loss trials phase 1
     - missed loss trials phase 2
     - total loss trials phase 1
     - total loss trials phase 2      
## 2) [RDManalysis.Rmd](./RDManalysis.Rmd)
-   Sources RDMsetup.R script first
-   Analysis of risky decision-making data
