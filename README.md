# <a href="https://pradhanhitesh.shinyapps.io/characteristics-table-generator/">Characteristics Table Generator</a>

<!-- START OF PROFILE STACK, DO NOT REMOVE -->
| **Sub_ID** | **Age** | **Gender** | **Height (cm)** | **Weight (kg)** | **BMI Category**
| - | - | - | - | - | - |
| Sub_001 | 18 | Male | 170 | 55 | Underweight
| Sub_002 | 20 | Female | 160 | 50 | Healthy
| .. | .. | .. | .. | .. | .. |
| Sub_XXX | .. | .. | .. | .. | .. |

<!-- END OF PROFILE STACK, DO NOT REMOVE -->

In this example, let us say that we want to see the age, gender, height and weight distribution of subjects across different BMI categories (4 categories), i.e., underweight, healthy, overweight, and obesse. To do so, we can use the Characteristics Table Generator to perform simple statistical tests, such as Student's T-test or ANOVA (continuous) and Chi-Square Test (categorical) and compare across BMI groups. 

#### How to use Characteristics Table Generator?

1. Upload your .csv file which should typically consists of subject IDs, grouping variable (categorical), and other variables (categorical or continuous). The sample data consists of subject IDs, grouping variable (BMI_Group), and other variables (Age, Height, Weight, BMI).
<p align="center">
    <img src="./data/images/SampleData.png" width="620" height="550">
</p>

<p align="center">
    <img src="./data/images/UploadFile_DialougeBox.png" width="620" height="250">
</p>

2. Now, we have to choose:
 
    a. Categorical grouping variable: BMI_Group
    
    b. Categorical Variables: Gender

    c. Excluding Variables: Sub_ID

    Here, we need to specify your grouping column, categorical variable columns and columns to be excluded. 
<p align="center">
    <img src="./data/images/SelectVariables_UI.png" width="620" height="850">
</p>

Find the link to sample data. 