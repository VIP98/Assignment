##First step

train <- read.table("UCI HAR Dataset/train/X_train.txt")
test <- read.table("UCI HAR Dataset/test/X_test.txt") 
cuccos <- rbind(test,train)
train_labels <- read.table("UCI HAR Dataset/train/y_train.txt") 
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt")  
Activity <- rbind(test_labels, train_labels) 
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt") 
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
Subject <- rbind(test_subject, train_subject)
First_step <- cbind(cuccos,Activity, Subject)

##Second step

katódsugárcső <- function(x) {
     ## the function does the given measurments and exports it in a new
     ## dataframe (called tablazat)
     tablazat <- data.frame()
     j <- nrow(x)
     for (i in 1:j) {
          Mean <- mean(as.numeric(x[i,1:561]))
          Standard_Deviation <- sd(as.numeric(x[i,1:561]))
          Activity <- x[i,562]
          Subject <- x[i,563]
          egyben <- cbind(Mean,Standard_Deviation,Activity, Subject)
          tablazat <- rbind(tablazat, egyben)
     }
     return(tablazat)
}
Second_step <- katódsugárcső(First_step) 

##Third step


betonkeverő <- function(x) {
     ## the function is using simple funcions, but works well and pretty quick
     ## the funcion changes the all value to the corresponding string and saves that a data frame called tablazat
     j <- nrow(x)
     tablazat <- data.frame()
     for (i in 1:j) {
          mostani <- as.numeric(x[i,3])
          if (mostani == 1) {
               Activity <- c("Walking")
          }
          else if (mostani == 2) {
               Activity <- c("Walking_upstairs")
          }
          else if (mostani == 3) {
               Activity <- c("Walking_downstairs")
          }
          else if (mostani == 4) {
               Activity <- c("Sitting")
          }
          else if (mostani == 5) {
               Activity <- c("Standing")
          }
          else if (mostani == 6) {
               Activity <- c("Laying")
          }
          else {
               Activity <- c("Error!")
          }
          Mean <- x[i,1]
          Standard_Deviation <- x[i,2]
          Subject <- x[i,4]
          egyben <- cbind(Mean,Standard_Deviation,Activity,Subject)
          tablazat <- rbind(tablazat, egyben)
     }
     return(tablazat)
}
Third_step <- betonkeverő(Second_step)


##Fourth step

## I have already done that point, I paid attention to name the variables descriptive
## otherwise it should be done by the 'rename' function from the 'dplyr' package
Fourth_step <- Third_step

##Fifth step

## I can't do that, this command isn't working well:
## asd <- group_by(Fourth_step, Subject)
## summarize(asd, Mean = mean(Mean), SD = mean(Standard_Deviation))
## This is not the full code, but it is important to working and it doesn't working.
## I get a data but that is not the mean! It's more than 1, and 1 is the biggest value...

