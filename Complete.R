complete <- function(directory, id = 1:332)
{
 #Create a list of files
 files_full <- list.files(directory, full.names= TRUE)
 # Create empty data frame 
 dat <- data.frame()
 for (i in id)
 {
   # Read files
   temp <- read.csv(files_full[i])
   # nobs are sum of all complete cases
   # Complete cases:often used to identify complete rows of a data frame(have no missing value)
  
   nobs <-sum(complete.cases(temp))
   
   # Enamurtates complete cass by index
   dat <-rbind(dat, data.frame(i, nobs))
   
}
  colnames(dat) <- c("id", "nobs")
  return(dat)
}


#OUTPUT
#complete("specdata", c(2, 4, 8, 10, 12))
 # id nobs
#1  2 1041
#2  4  474
#3  8  192
#4 10  148
#5 12   96

#cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
#print(cc$nobs)
#228, 148, 124, 165, 104, 460, 232

#cc <- complete("specdata", 54)
#print(cc$nobs)
#219

#RNGversion("3.5.1")  
#set.seed(42)
#cc <- complete("specdata", 332:1)
#use <- sample(332, 10)
#print(cc[use, "nobs"])
#711, 135, 74, 445, 178, 73, 49, 0, 687, 237


