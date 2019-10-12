# SET THE WORKING DIRECTORY TO BE THE FOLDER WHERE THIS FILE IS LOCATED

# Step 1 - Read and merge the datasets
# Read the datasets, trim the whitespace at the start and end, and replace double spaces with single spaces

# Read the files
trainingXData <- read.table("UCI HAR Dataset/train/X_train.txt")
trainingYData <- read.table("UCI HAR Dataset/train/y_train.txt")
trainingSubjectData <- read.table("UCI HAR Dataset/train/subject_train.txt")

# Set the column names for later
names(trainingYData) <- 'Activity'
names(trainingSubjectData) <- 'Subject'

# Attach the columns to the main datatable
midMergedTrainingData <- cbind(trainingXData, trainingYData)
mergedTrainingData <- cbind(midMergedTrainingData, trainingSubjectData)

# Read the files
testingXData <- read.table("UCI HAR Dataset/test/X_test.txt")
testingYData <- read.table("UCI HAR Dataset/test/y_test.txt")
testingSubjectData <- read.table("UCI HAR Dataset/test/subject_test.txt")

# Set the column names for later
names(testingYData) <- 'Activity'
names(testingSubjectData) <- 'Subject'

# Attach the columns to the main datatable
midMergedTestingData <- cbind(testingXData, testingYData)
mergedTestingData <- cbind(midMergedTestingData, testingSubjectData)

# Attach training and testing datasets together
completeMerge <- rbind(mergedTrainingData, mergedTestingData)


# Step 2 - Extract only the mean and std columns
# Extract the mean and standard deviation columns from the merged dataset. Also include the Activity IDs and the labels
dataMeanStd <- completeMerge[, c("V1","V2","V3","V4","V5","V6","V41","V42","V43","V44","V45","V46","V81","V82","V83","V84","V85","V86","V121","V122","V123","V124","V125","V126","V161","V162","V163","V164","V165","V166","V201","V202","V214","V215","V227","V228","V240","V241","V253","V254","V266","V267","V268","V269","V270","V271","V345","V346","V347","V348","V349","V350","V424","V425","V426","V427","V428","V429","V503","V504","V516","V517","V529","V530","V542","V543","Activity","Subject")]

# Step 3 - Merge the Activity names into the dataset
# Read the activity labels file
activities <- read.table("UCI HAR Dataset/activity_labels.txt")

# Create lookup table
activityLookup <- activities$V2
names(activityLookup) <- activities$V1

# Look activities up into new column
ActivityDescription <- activityLookup[as.integer(dataMeanStd$Activity)]
names(ActivityDescription) <- "ActivityDescription"

# Attach new column
dataMeanStd <- cbind(dataMeanStd, ActivityDescription)

# Step 4 - Replace column names with the proper feature name
# Read features file
features <- read.table("UCI HAR Dataset/features.txt")

# Create lookup table
featureLookup <- features$V2
names(featureLookup) <- features$V1

# For each feature, replace characters to make it more readable (- by ., t by Time, f by Freq, remove ( and ))
featureLookup <- sapply(featureLookup, function(x) str_replace_all(x, c("-" = ".", "^t" = "Time", "^f" = "Freq", "\\(" = "", "\\)" = "")))

# Remove V from each column name to only keep the number
colnames(dataMeanStd) <- sapply(colnames(dataMeanStd), function(x) str_replace(x, "V", ""))

# For each column name, if the name is a numeric value, look it up in the features table. Else just keep the name
names(dataMeanStd) <- sapply(colnames(dataMeanStd), function(x) if(!is.na(as.numeric(x))) { featureLookup[as.integer(x)] }else {x})
write.table(dataMeanStd, "Step4.txt")

# Step 5 - Create second dataset with averages of each column for each subject and activity
# Group the dataset by subject, then activity. Then apply the mean to each column
dataMeanStd <- read.table("Step4.txt")

results <- dataMeanStd %>% group_by(Subject, Activity) %>% summarise_all(mean)

write.table(results, "Step5.txt")