# Functions to read in the CSV table that contains all of the raw data.
# Before running these functions, make sure the file "all-data.csv" is
# in the local directory.
# Also, within the R environment, change the working directory to the directory
# that contains the data file using the toolbar menu:
# File -> Change dir
#

# Read the data from the csv file.
processors <- read.csv("all-data.csv")


################################################################
#
# This function returns the data from the desired column.
# Example:  clock<-get_column("Fp2000","Processor.Clock..MHz.")

get_column <- function(x,y) {
  
  # x = string with the name of the desired benchmark
  # y = desired column
  #
  # Find the indices of all rows that have an entry for the  
  # indicated benchmark
  benchmark <- paste(paste("Spec",x,sep=""),"..average.base.",
                     sep="")
  ix <- !is.na(processors[,benchmark])
  return(processors[ix,y])
}
################################################################




################################################################
# This function extracts the interesting data columns for the given benchmark
# program and returns a dataframe with these columns.

extract_data <- function(benchmark) {
  
  temp <- paste(paste("Spec",benchmark,sep=""),"..average.base.", sep="")
  
  # perf = the performance reported in the database
  perf <- get_column(benchmark,temp)
  
  #nperf = performance normalized to the overall range
  max_perf <- max(perf)
  min_perf <- min(perf)
  range <- max_perf - min_perf
  nperf <- 100 * (perf - min_perf) / range
  
  clock <- get_column(benchmark,"Processor.Clock..MHz.")
  threads <- get_column(benchmark,"Threads.core")
  cores <- get_column(benchmark,"Cores")
  TDP <- get_column(benchmark,"TDP")
  transistors <- get_column(benchmark,"Transistors..millions.")
  dieSize <- get_column(benchmark,"Die.size..mm.2.")
  voltage <- get_column(benchmark,"Voltage..low.")
  featureSize <- get_column(benchmark,"Feature.Size..microns.")
  channel <- get_column(benchmark,"Channel.length..microns.")
  FO4delay <- get_column(benchmark,"FO4.Delay..ps.")
  L1icache <- get_column(benchmark,"L1..instruction...on.chip.")
  L1dcache <- get_column(benchmark,"L1..data...on.chip.")
  L2cache <- get_column(benchmark,"L2..on.chip.")
  L3cache <- get_column(benchmark,"L3..on.chip.")
  
  return(data.frame(nperf,perf,clock,threads,cores,TDP,transistors,dieSize,voltage,featureSize,channel,FO4delay,L1icache,L1dcache,L2cache,L3cache))
  
}
################################################################


# Extract a new data frame for each of the benchmark programs available in the data set.

int92.dat <- extract_data("Int1992")
fp92.dat <- extract_data("Fp1992")
int95.dat <- extract_data("Int1995")
fp95.dat <- extract_data("Fp1995")
int00.dat <- extract_data("Int2000")
fp00.dat <- extract_data("Fp2000")
int06.dat <- extract_data("Int2006")
fp06.dat <- extract_data("Fp2006")


###########################################################################

## [1]
D1_int92 <- sapply(int92.dat, na.rm = TRUE, mean)
D1_int95 <- sapply(int95.dat, na.rm = TRUE, mean)
D1_int00 <- sapply(int00.dat, na.rm = TRUE, mean)
D1_int06 <- sapply(int06.dat, na.rm = TRUE, mean)

D1_fp92 <- sapply(fp92.dat, na.rm = TRUE, mean)
D1_fp95 <- sapply(fp95.dat, na.rm = TRUE, mean)
D1_fp00 <- sapply(fp00.dat, na.rm = TRUE, mean)
D1_fp06 <- sapply(fp06.dat, na.rm = TRUE, mean)

mean = cbind(D1_int92, D1_int95, D1_int00, D1_int06, D1_fp92, D1_fp95, D1_fp00, D1_fp06)


## [2]
D2_int92 <- sapply(int92.dat, na.rm = TRUE, var)
D2_int95 <- sapply(int95.dat, na.rm = TRUE, var)
D2_int00 <- sapply(int00.dat, na.rm = TRUE, var)
D2_int06 <- sapply(int06.dat, na.rm = TRUE, var)

D2_fp92 <- sapply(fp92.dat, na.rm = TRUE, var)
D2_fp95 <- sapply(fp95.dat, na.rm = TRUE, var)
D2_fp00 <- sapply(fp00.dat, na.rm = TRUE, var)
D2_fp06 <- sapply(fp06.dat, na.rm = TRUE, var)

variance = cbind(D2_int92, D2_int95, D2_int00, D2_int06, D2_fp92, D2_fp95, D2_fp00, D2_fp06)


## [3]
D3_int92 <- sapply(int92.dat, na.rm = TRUE, min)
D3_int95 <- sapply(int95.dat, na.rm = TRUE, min)
D3_int00 <- sapply(int00.dat, na.rm = TRUE, min)
D3_int06 <- sapply(int06.dat, na.rm = TRUE, min)

D3_fp92 <- sapply(fp92.dat, na.rm = TRUE, min)
D3_fp95 <- sapply(fp95.dat, na.rm = TRUE, min)
D3_fp00 <- sapply(fp00.dat, na.rm = TRUE, min)
D3_fp06 <- sapply(fp06.dat, na.rm = TRUE, min)

minimum = cbind(D3_int92, D3_int95, D3_int00, D3_int06, D3_fp92, D3_fp95, D3_fp00, D3_fp06)



## [4]
D4_int92 <- sapply(int92.dat, na.rm = TRUE, var)
D4_int95 <- sapply(int95.dat, na.rm = TRUE, var)
D4_int00 <- sapply(int00.dat, na.rm = TRUE, var)
D4_int06 <- sapply(int06.dat, na.rm = TRUE, var)

D4_fp92 <- sapply(fp92.dat, na.rm = TRUE, max)
D4_fp95 <- sapply(fp95.dat, na.rm = TRUE, max)
D4_fp00 <- sapply(fp00.dat, na.rm = TRUE, max)
D4_fp06 <- sapply(fp06.dat, na.rm = TRUE, max)

maximum = cbind(D4_int92, D4_int95, D4_int00, D4_int06, D4_fp92, D4_fp95, D4_fp00, D4_fp06)


########################################################################################

colMeans_int92 = colMeans(is.na(int92.dat))
colMeans_int95 = colMeans(is.na(int95.dat))
colMeans_int00 = colMeans(is.na(int00.dat))
colMeans_int06 = colMeans(is.na(int06.dat))
colMeans_fp92 = colMeans(is.na(fp92.dat))
colMeans_fp95 = colMeans(is.na(fp95.dat))
colMeans_fp00 = colMeans(is.na(fp00.dat))
colMeans_fp06 = colMeans(is.na(fp06.dat))

colMeans_all <- cbind(colMeans_int92, colMeans_int95, colMeans_int00, colMeans_int06, colMeans_fp92, colMeans_fp95, colMeans_fp00, colMeans_fp06)


table_int92 <- sapply(int92.dat, table)
table_int95 <- sapply(int95.dat, table)
table_int00 <- sapply(int00.dat, table)
table_int06 <- sapply(int06.dat, table)

table_fp92 <- sapply(fp92.dat, table)
table_fp95 <- sapply(fp95.dat, table)
table_fp00 <- sapply(fp00.dat, table)
table_fp06 <- sapply(fp06.dat, table)



plot(int92.dat$nperf, int92.dat$clock, na.rm = TRUE, inf.rm = TRUE)
plot(int95.dat$nperf, int95.dat$clock, na.rm = TRUE, inf.rm = TRUE)
plot(int00.dat$nperf, int00.dat$clock, na.rm = TRUE, inf.rm = TRUE)
plot(int06.dat$nperf, int06.dat$clock, na.rm = TRUE, inf.rm = TRUE)

plot(fp92.dat$nperf, fp92.dat$clock, na.rm = TRUE, inf.rm = TRUE)
plot(fp95.dat$nperf, fp95.dat$clock, na.rm = TRUE, inf.rm = TRUE)
plot(fp00.dat$nperf, fp00.dat$clock, na.rm = TRUE, inf.rm = TRUE)
plot(fp06.dat$nperf, fp06.dat$clock, na.rm = TRUE, inf.rm = TRUE)
