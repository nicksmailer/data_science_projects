Predicting Playoff Teams

The goal of my project was to use historic basketball data to determine if playoff appearance could be predicted through a variety of methods. I used both player and team data to make predictions. Each dataset was cleaned, then stored in data warehouses. The JSON files are the tables from each warehouse. The data was then queried from the warehouses and used to model with primarily the Random Forest algorithm and k-means clustering. My main findings were that 42 and 50 wins are good historic indicators of playoff appearance at 74% and 97% respectively and that all-stars are imperative to reaching the playoffs- 75% chance with 1+ all-star. Using those values to set binary thresholds, it was possible to accurately predict which teams would be above the threshold, thus making the playoffs. The team data was able to achieve > 90% accuracy with both thresholds, while the player data stayed around 75% - 80% accuracy. As a result, the application I created only operates using the team data. I was also able to use clustering to determine basic criteria for being an all-star- roughly at least 30 TOTAL points per game, assists per game, and rebounds per game. All of this and plenty of visualizations are in the Final_scripts file. 

In order to run my the Final_scripts file, there are three crucial steps:
1. Download and install RStudio: https://rstudio-education.github.io/hopr/starting.html 
2. Download the JSON files stored in the data folder
3. Adjust the file paths to reflect where the JSON files are stored locally

This will allow everything to run successfully. The final scripts file contains everything after upload to the data warehouses.
The capstone_project_cleaning file is what was used to clean the initial CSV files before storage in the data warehouses. To run that portion, the CSV files from the Data folder would need to be downloaded and file paths adjusted. The KNIME ETL files are what was used to create the data warehouse. In order for them to be used, a matching data warehouse is required. As a result, these are more just to show the process.
In order to run the application (Team_modeling.rmd), assuming RStudio and the JSON files have been downloaded, just adjust the file paths and install the packages. Then run everything except the final line which actually calls the app. Once it is done, run the final line to call the app. For more detailed instructions on the app, see the week 14 section of my project description. 


