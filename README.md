﻿# <a href="https://pradhanhitesh.shinyapps.io/demographics-table-generator/">Baseline Characteristics Table Generator</a>

#### What is a baseline characteristics table?
According to <a href = "https://obgyn.onlinelibrary.wiley.com/doi/full/10.1111/1471-0528.15498#:~:text=Baseline%20tables%20show%20the%20characteristics,results%20could%20be%20generalised%20to.">Baseline 101 – who is who?</a> by Nadine Marlin and John Allotey, baseline tables show the characteristics of research subjects included in a study. Depending on the study design they may have specific purposes but generally they show if the population included conforms to the eligibility criteria of the study. They also indicate what population the results could be generalised to. Baseline characteristics table are very important very in large cohort studies and clinical trials. You can read more about requirements of baseline characteristics <a href = "https://prsinfo.clinicaltrials.gov/results_definitions.html">here</a>.

#### How to use Characteristics Table Generator?

1. Upload your .csv file which should typically consists of subject IDs, grouping variable (categorical), and other variables (categorical or continuous). The <a href = "https://github.com/pradhanhitesh/Characterisitcs-Table/blob/main/data/BMI_SampleData.csv">sample data</a> consists of subject IDs, grouping variable (BMI_Group), and other variables (Age, Height, Weight, BMI).

<p align="center">
    <img src="./data/images/SampleData.png" width="620" height="550">
</p>

<p align="center">
    <img src="./data/images/UploadFile_DialougeBox.png" width="630" height="250">
</p>

2. Now, we have to choose:
 
    a. Categorical grouping variable: BMI_Group
    
    b. Categorical Variables: Gender

    c. Excluding Variables: Sub_ID

    Here, specify your grouping column, categorical variable columns and columns to be excluded. We need not specify the continuous variables. 
<p align="center">
    <img src="./data/images/SelectVariables_UI.png" width="620" height="850">
</p>

Here is the output data. <a href="https://github.com/pradhanhitesh/Characterisitcs-Table/blob/main/data/Flextable_2024-02-152024-02-15%2023_55_14.261959.docx">Click here</a>

# Acknowledgement
The tool was developed for internal use within the <a href = "https://cbr.iisc.ac.in/research/flagship-projects/sanscog/"> SANSCOG </a> study. I would like to thank <a href = "https://www.linkedin.com/in/suhrudp/">Suhrud Panchwagh</a> for conceptualizing the idea and writing the backend R code for conducting statistical analysis and generating flextables. My contribution to the project was optimizing the backend code for flexible analysis and deploying the project on RShiny.
