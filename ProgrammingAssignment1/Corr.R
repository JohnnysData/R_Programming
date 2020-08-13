#Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between 
#sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0

corr <- function(directory, threshold = 0)
{
 #create list of all files
  files_full <- list.files(directory, full.names= TRUE)
 # create empty data set
  dat <- vector(mode = "numeric", length = 0)
  for(i in 1:length(files_full))
  {
      # Read File
       tmp <- read.csv(files_full[i])
   
       #Calculate csum    
       csum <- sum((!is.na(tmp$sulfate)) & (!is.na(tmp$nitrate)))
      if (csum > threshold)
      {
        #Extract data of nitrate and sulfate and calculate correlation between them
         sul <- tmp[which(!is.na(tmp$sulfate)), ]
         nit <- sul[which(!is.na(sul$nitrate)), ]
         dat <- c(dat, cor(nit$sulfate, nit$nitrate))
      }
  }
   
    dat
}

#OUTPUT

# cr <- corr("specdata", 150)
# head(cr)
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
# summary(cr)
 #   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21060 -0.04999  0.09463  0.12530  0.26840  0.76310 


# cr <- corr("specdata", 400)
# head(cr)
#[1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
# summary(cr)
 #   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.17620 -0.03109  0.10020  0.13970  0.26850  0.76310 


# cr <- corr("specdata", 5000)
# summary(cr)
 #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#length (cr)
#[1] 0

# cr <- corr("specdata")
# summary(cr)
 #   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.00000 -0.05282  0.10720  0.13680  0.27830  1.00000 
#length(cr)
#[1] 323


#What value is printed at end of the following code?
#cr <- corr("specdata")                
#cr <- sort(cr)   
#RNGversion("3.5.1")
#set.seed(868)                
#out <- round(cr[sample(length(cr), 5)], 4)
#print(out)

#What value is printed at end of the following code?
#cr <- corr("specdata", 129)                
#cr <- sort(cr)                
#n <- length(cr)    
#RNGversion("3.5.1")
#set.seed(197)                
#out <- c(n, round(cr[sample(n, 5)], 4))
#print(out)

#What value is printed at end of the following code?
#cr <- corr("specdata", 2000)                
#n <- length(cr)                
#cr <- corr("specdata", 1000)                
#cr <- sort(cr)
#print(c(n, round(cr, 4)))
