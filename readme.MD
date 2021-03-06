The script for run_analysis.R should:
1.	Merge the training and the test sets to create one data set.
2.	Extract only the measurements on the mean and standard deviation for each measurement. 
3.	Use descriptive activity names to name the activities in the data set
4.	Appropriately label the data set with descriptive variable names. 
5.	From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
For this script to run, you will need to install two packages (if they are not already installed in your R program): plyr and dplyr. Please install in that order, otherwise you will get error messages.
This script assumes that you have downloaded and unzipped the folder containing the UCI HAR Dataset. However, you do not need to do anything else for the code to run.
The first step of the script will read in the training group and test group data files and merge them together to form one dataset. The training group has 21 participants and the test group has 9. The first step will merge together the file X_train.txt, y_train.txt, X_test.txt, y_test.txt, subject_train, subject_test (which includes the identifying subject numbers for the training and test groups respectively) and the activity file. This step also renames the first two variables to be the subject and activity code variable (called �activity�).
The second step of the script reads all of the column names in the big data set and extracts only those columns that include the string �mean� or �std�. There should be 86 columns with either of these strings included.
The third step uses a left join function to rewrite the code in the �activity� column with the plain English word describing the activity (walking, walking upstairs, walking downstairs, sitting, standing, or laying).
The fourth step in the function apples more descriptive column names to the column in the dataset. The variables can be interpreted using the codebook provided in the folder.
The fifth step should write out a text file that shows the means for each of the variables selected above by subject and activity (averaged over all of the readings).

