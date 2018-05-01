run_analysis <- function() {
        
        # PART 1
        # Merges the training and the test sets to create one data set.
        
        # Read the two X-files.
        # Files X_test.txt and X_train.txt with 561 entries in each row.
        # Each entry is of fixed length, 16 columns (including spaces).
        
        X_test <- read.fwf( "test/X_test.txt", widths=c(rep(16,561)) )
        X_train <- read.fwf( "train/X_train.txt", widths=c(rep(16,561)) )
        
        # Merge data from the two X-fsiles.
        
        X_all <- rbind(X_train, X_test)
        
        # Read the two Y-files.
        
        Y_test <- read.csv("test/y_test.txt", header=FALSE)
        Y_train <- read.csv("train/y_train.txt", header=FALSE)
        
        # Merge data from the two Y-files.
        
        Y_all <- rbind(Y_train, Y_test)        

        # Read the two subject-files.
        
        subj_test <- read.csv("test/subject_test.txt", header=FALSE)
        subj_train <- read.csv("train/subject_train.txt", header=FALSE)
        
        # Merge data from the two subject-files.
        
        subj_all <- rbind(subj_train, subj_test)    
                
        # Read the Feature names from file
        
        feat_file <- read.csv("features.txt", header=FALSE, sep="")
        
        feat <- as.vector(t(feat_file[2]))
        
        # PART 2
        # Extracts only the measurements on the mean and standard deviation 
        # for each measurement. 
        
        # Get mean and standard deviation lines 
        
        selection <- grep("[Mm]ean|[Ss]td",feat)
        
        X <- X_all[,selection]
        
        # PART 4
        # Appropriately labels the data set with descriptive variable names. 
        
        # Set names for columns
        
        colnames(subj_all) <- "Subject"
        
        colnames(Y_all) <- "Activity"
        
        colnames(X) <- feat[selection]
        
        # PART 3
        # Uses descriptive activity names to name the activities in the data set.
        
        activ <- factor( c( "Walking", "WalkingUpstairs", "WalkingDownstairs", "Sitting", "Standing", "Laying", rep("Walking", times = 10293) ) )
        
        data_all <- cbind(subj_all,activ,X)
        
        colnames(data_all)[2] = "Activity"

        for (i in 1:10299) {
                
                if (Y_all[i,1]==1) {
                        data_all[i,2] = "Walking"
                } else if (Y_all[i,1]==2) {
                        data_all[i,2] = "WalkingUpstairs"
                } else if (Y_all[i,1]==3) {
                        data_all[i,2] = "WalkingDownstairs"
                } else if (Y_all[i,1]==4) {
                        data_all[i,2] = "Sitting"
                } else if (Y_all[i,1]==5) {
                        data_all[i,2] = "Standing"
                } else {
                        data_all[i,2] = "Laying"
                } 
        }
        
        # PART 5
        # From the data set in step 4, creates a second, independent tidy 
        # data set with the average of each variable for each activity and 
        # each subject.
        
        library(reshape2)
        
        data_means <- aggregate(. ~ Subject + Activity, data = data_all, mean)
        
        # Write output files
        
        write.table(data_all, file="data_all.txt", row.name=FALSE)        
        
        write.table(data_means, file="data_means.txt", row.name=FALSE)
        
}