pollutantmean <- function(directory  = "scripts/assignment1/specdata/", pollutant, id = 1:332) {
        file_list <- list.files(path = directory)
        dataset <- data.frame()
        for (i in id) {
                temp_data <- read.csv(paste(directory, file_list[i], sep = ""))
                dataset <- rbind(dataset, temp_data)
        }
        
        mean(dataset[[pollutant]], na.rm = TRUE)
}


complete <- function(directory = "scripts/assignment1/specdata/", id = 1:332) {
        file_list <- list.files(path = directory)
        dataset <- data.frame()
        for (i in id) {
                i_data <- read.csv(paste(directory, file_list[i], sep = ""))
                i_nobs <- c(i,nrow(i_data[complete.cases(i_data),]))
                dataset <- rbind(dataset, i_nobs)
                colnames(dataset) <- c("id", "nobs")
        }
        dataset
}

corr <- function(directory = "scripts/assignment1/specdata/", threshold = 0){
        id = 1:332
        file_list <- list.files(path = directory)
        dataset <-  c()
        for (i in id) {
                temp_data <- read.csv(paste(directory, file_list[i], sep = ""))
                clean_data <- temp_data[complete.cases(temp_data),]
                if (nrow(clean_data) > threshold){
                        dataset <- c(dataset, cor(clean_data$nitrate, clean_data$sulfate))
                }
        }
        dataset
}