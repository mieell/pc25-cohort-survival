# Grade 7–10 Cohort Survival in the Philippines

**National Teachers College**  
**PC25 R Language w/Lab | Week 17 Final Terminal Assessment**  
**Vonn Christian Miel P. Meneses | BSIT 4.9**

This student-success project uses published DepEd and PSA rates to compare junior high school cohort survival, test whether lagged rates can flag sizeable declines, and produce a historical national forecast. The dashboard is a regional review tool for education staff. It uses public aggregates.

## Run

Tested with R 4.6.1 on Windows. Install Pandoc, or use the version bundled with RStudio. In the R console, install the required packages once.

```r
install.packages(c("ggplot2", "shiny", "bslib", "rmarkdown", "knitr"))
```

Set the working directory to the folder containing `04_Analysis_Pipeline.R`. Keep the original CSV at `data/raw/psa_jhs_cohort_survival.csv`. In the terminal:

```sh
Rscript 04_Analysis_Pipeline.R
```

This regenerates `05_Cleaned_Dataset.csv` and the quality tables in `outputs/`. Run the remaining commands in the R console, with the dashboard last.

```r
rmarkdown::render("01_Project_Report.Rmd", output_file = "02_Project_Report.html")
source("07_Generate_Presentation.R", local = new.env())
app <- source("03_Executive_Dashboard.R", local = new.env())$value
shiny::runApp(app, host = "127.0.0.1", launch.browser = TRUE)
```

The PDF, dashboard, and HTML use the same R pipeline. The data dictionary states the published units, lag rules, exclusions, and modeling split.

## Source

Department of Education, [PSA OpenSTAT SDG 4.1.s1 Cohort survival rate](https://openstat.psa.gov.ph/PXWeb/pxweb/en/DB/DB__3I__G04/0033I3C41S1.px), matrix 3I3C41S1. The unchanged CSV was retrieved through the [Philippine Data Explorer](https://statistics.bettergov.ph/datasets/657e4a4528f3129d022d) public API, dataset `657e4a4528f3129d022d`, mirror release `2026-09-22T10-54-38-a9e047f8723d`. The selection was Junior High School, all reported geographies, all sex categories, and all offered years. The PSA metadata marks copyright “No.” Credit PSA and DepEd as the data source and Philippine Data Explorer as the retrieval mirror.

Data values are percentages. The released numeric observations run through 2024. The original extraction retains blanks for unavailable observations. The national series comes directly from the national Both Sexes row, while the regression and classifier use regional female/male data. Regions affected by major geographic coverage changes are excluded from model comparisons as explained in the report.

Frozen source CSV SHA-256: `7c6de6a6709e9acb3e27b714544051cadbcb62a007a2958363395825d95db74d`.

## Limitations

The educational rates are reconstructed estimates and cannot trace individual students. The three-point decline cutoff is a project monitoring rule. Compare flags with local records and investigate changes in reporting and enrollment before proposing actions.

## Week 17 group and contributions

Group number, roster, and individual contributions are pending. These eight areas are available for assigning and documenting the work once the group is confirmed.

| Area | Member | Actual contribution |
| --- | --- | --- |
| Source data and dictionary | | |
| Cleaning, analysis, and validation | | |
| Executive dashboard | | |
| Technical report and interpretation | | |
| Report rendering and review | | |
| Presentation | | |
| Setup and reproducibility | | |
| Demo video and live walkthrough | | |

All members participate in the live presentation or demonstration and should be ready to explain their contribution. The guide recommends 12 minutes for the presentation and demo plus 8 minutes for Q&A, subject to the instructor's schedule.
