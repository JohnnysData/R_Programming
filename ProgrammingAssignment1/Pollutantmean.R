#To calculate the mean of a pollutant (sulfate or nitrate) across a specified list of monitors.

#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' 
#reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant 
#across all of the monitors, ignoring any missing values coded as NA. 


pollutantmean <- function(directory, pollutant, id = 1:332)
{
  #create a list of files
  files_full <- list.files(directory, full.names = TRUE) 
  # create an empty data frame
  dat <- data.frame()
  for (i in id)
   {
       #add files to main data
       dat <- rbind(dat, read.csv(files_full[i]))
       
   }
    
  #Calulate mean
    mean_data <- mean(dat[, pollutant], na.rm = TRUE)
    return(mean_data)
}

#OUTPUT
#pollutantmean("specdata", "sulfate", 1:10)
#[1] 4.064128

#pollutantmean("specdata", "nitrate", 70:72)
#[1] 1.706047

#pollutantmean("specdata", "nitrate", 23)
#[1] 1.280833
