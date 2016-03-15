################################## Data Preparation ##################################

## Walking Distance
W.Data <- read.xlsx("Data1.xlsx", 1)
W.GC <- read.xlsx("Data2.xlsx", 1)
W.Zipcodes <- read.xlsx("Data3.xlsx", 1)
W.Counts <- read.xlsx("Data4.xlsx", 1)

AllData <- read.xlsx("Data5.xlsx", 1)

## Commuters
C.Data <- read.xlsx("Data6.xlsx", 1)
C.GC <- read.xlsx("Data7.xlsx", 1)
C.Zipcodes <- read.xlsx("Data8.xlsx", 1)
C.Counts <- read.xlsx("Data9.xlsx", 1)

## Travelers
W.C.Zipcodes <- unique(rbind(W.Zipcodes, C.Zipcodes))
T.Data <- AllData[!AllData$zip5_cd %in% W.C.Zipcodes$KEY, ]
## Calculating T.Counts
T.transcount <- with(T.Data, tapply(trans_amt, segment_id, length))
segments_id <- as.data.frame(names(T.transcount))
names(T.transcount) <- 1:8
T.Data$unique_id <- as.character(T.Data$unique_id)
T.customercount <- with(T.Data[!duplicated(T.Data$unique_id),], tapply(trans_amt, segment_id, length))
names(T.customercount) <- 1:8
T.Counts <- cbind(segments_id, as.data.frame(T.transcount), as.data.frame(T.customercount))
names(T.Counts) <- c("segment_id", "Sum_Count", "Sum_CountDistinct_unique_id")


## DataAll % applied
DataAll <- rbind(W.Data, C.Data, T.Data)
GCData <- rbind(W.GC, C.GC)
CountAll <- rbind(W.Counts, C.Counts, T.Counts)


################################### Functions ###########################################
Sales <- function(salesData){
    
    sales <- as.data.frame.list(matrix(0, 9, 1))
    names(sales) <- c("S1", "S2", "S3", "S4", "S5", "S6","S7","S8","Total")
    x <- with(salesData, tapply(trans_amt, segment_id, sum))
            
    for (i in 1:8){
        sales[i] <- x[names(x) == names(sales[i])]
    }
    
    sales[is.na(sales)] <- 0
    sales$Total <- sum(sales[1, 1:8])
    
    sales
}


Transactions <- function(transData){
    
    transactions <- as.data.frame.list(matrix(0, 9, 1))
    names(transactions) <- c("S1", "S2", "S3", "S4", "S5", "S6","S7","S8","Total")
    x <- with(transData, tapply(Sum_Count, segment_id, sum))
    
    for (i in 1:8){
        transactions[i] <- x[names(x) == names(transactions[i])]
    }
    transactions[is.na(transactions)] <- 0
    transactions$Total <- sum(transactions[1, 1:8])
    transactions
    
}


Customers <- function(cusData){
    
    customers <- as.data.frame.list(matrix(0, 9, 1))
    names(customers) <- c("S1", "S2", "S3", "S4", "S5", "S6","S7","S8","Total")
    x <- with(cusData, tapply(Sum_CountDistinct_unique_id, segment_id, sum))
    
    for (i in 1:8){
        customers[i] <- x[names(x) == names(customers[i])]
    }
    customers[is.na(customers)] <- 0
    customers$Total <- sum(customers[1, 1:8])
    customers
    
}


PeakDays <- function(dataAll){
    ## Changing data classes
    dataAll$transdate <- as.Date(dataAll$transdate, "%m/%d/%Y")
    dataAll$segment_id <- as.character(dataAll$segment_id)
    
    ## Creating sum of revenue matrix
    SumOfRevenues <- data.frame(matrix(0, 9, 7), row.names = c("S1", "S2","S3","S4","S5","S6","S7","S8","Total"))
    names(SumOfRevenues) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    dataAll$weekdays <- weekdays(dataAll$transdate)
    i <- 1; j <- 1
    for (i in 1:8){
        for (j in 1:7){
            SumOfRevenues[i, j] <- sum(with(dataAll, dataAll[segment_id == rownames(SumOfRevenues)[i] & weekdays == names(SumOfRevenues)[j], "trans_amt"]))
        }
    }
    SumOfRevenues[9, ] <- sapply(SumOfRevenues, sum)
    
    ## Counting weekdays
    x <- as.data.frame.list(table(weekdays(unique(dataAll$transdate))))
    weekdayCount <- as.data.frame(matrix(0, 1, 7))
    names(weekdayCount) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    m <- 1
    for (m in 1:7){
        if (names(weekdayCount)[m] %in% names(x)){
            weekdayCount[1, m]<- x[1, names(weekdayCount)[m]]
        }
    }
    weekdayCountMatrix <- rbind(weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount)
    
    ## Creating average matrix
    Avgs <- SumOfRevenues/weekdayCountMatrix
    Avgs[is.na(Avgs)] <- 0
    
    ## Finding peak days
    locateMax <- max.col(Avgs)
    locateMax <- as.character(locateMax)
    a <- 1
    for (a in 1:length(locateMax)){
        if(sum(Avgs[a, ] == 0) == 7){
            locateMax[a] <- "None"
        }
    }
    n <- 1
    for (n in 1:length(locateMax)){
        if (locateMax[n]==1){locateMax[n] <- "Monday"}
        if (locateMax[n]==2){locateMax[n] <- "Tuesday"}
        if (locateMax[n]==3){locateMax[n] <- "Wednesday"}
        if (locateMax[n]==4){locateMax[n] <- "Thursday"}
        if (locateMax[n]==5){locateMax[n] <- "Friday"}
        if (locateMax[n]==6){locateMax[n] <- "Saturday"}
        if (locateMax[n]==7){locateMax[n] <- "Sunday"}
    }
    
    locateMax <- as.data.frame.list(locateMax)
    names(locateMax) <- c("S1", "S2","S3","S4","S5","S6","S7","S8","Total")
    locateMax
}


ValleyDays <- function(dataAll){
    ## Changing data classes
    dataAll$transdate <- as.Date(dataAll$transdate, "%m/%d/%Y")
    dataAll$segment_id <- as.character(dataAll$segment_id)
    
    ## Creating sum of revenue matrix
    SumOfRevenues <- data.frame(matrix(0, 9, 7), row.names = c("S1", "S2","S3","S4","S5","S6","S7","S8","Total"))
    names(SumOfRevenues) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    dataAll$weekdays <- weekdays(dataAll$transdate)
    i <- 1; j <- 1
    for (i in 1:8){
        for (j in 1:7){
            SumOfRevenues[i, j] <- sum(with(dataAll, dataAll[segment_id == rownames(SumOfRevenues)[i] & weekdays == names(SumOfRevenues)[j], "trans_amt"]))
        }
    }
    SumOfRevenues[9, ] <- sapply(SumOfRevenues, sum)
    
    ## Counting weekdays
    x <- as.data.frame.list(table(weekdays(unique(dataAll$transdate))))
    weekdayCount <- as.data.frame(matrix(0, 1, 7))
    names(weekdayCount) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    m <- 1
    for (m in 1:7){
        if (names(weekdayCount)[m] %in% names(x)){
            weekdayCount[1, m]<- x[1, names(weekdayCount)[m]]
        }
    }
    weekdayCountMatrix <- rbind(weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount, weekdayCount)
    
    ## Creating average matrix
    Avgs <- SumOfRevenues/weekdayCountMatrix
    Avgs[is.na(Avgs)] <- 0
    
    ## Finding peak days
    locateMin <- max.col(-Avgs)
    locateMin <- as.character(locateMin)
    a <- 1
    for (a in 1:length(locateMin)) {
        if (sum(Avgs[a, ] == 0) > 1){
            locateMin[a] <- "None"
        }
    }
    n <- 1
    for (n in 1:length(locateMin)){
        if (locateMin[n]==1){locateMin[n] <- "Monday"}
        if (locateMin[n]==2){locateMin[n] <- "Tuesday"}
        if (locateMin[n]==3){locateMin[n] <- "Wednesday"}
        if (locateMin[n]==4){locateMin[n] <- "Thursday"}
        if (locateMin[n]==5){locateMin[n] <- "Friday"}
        if (locateMin[n]==6){locateMin[n] <- "Saturday"}
        if (locateMin[n]==7){locateMin[n] <- "Sunday"}
    }

    locateMin <- as.data.frame.list(locateMin)
    names(locateMin) <- c("S1", "S2","S3","S4","S5","S6","S7","S8","Total")
    locateMin
}


GC <- function(GCData){
    
    GC <- as.data.frame.list(matrix(0, 9, 1))
    names(GC) <- c("S1", "S2", "S3", "S4", "S5", "S6","S7","S8","Total")
    x <- sapply(GCData[, 3:10], sum)
    
    for (i in 1:8){
        GC[i] <- x[names(x) == names(GC[i])]
    }
    GC[is.na(GC)] <- 0
    GC$Total <- sum(GC[1, 1:8])
    GC
}


################################### Outputs #######################################
## All Location Data
x1 <- Sales(DataAll) / Sales(DataAll)$Total; x1[is.na(x1)] <- 0
x2 <- Transactions(CountAll) / Transactions(CountAll)$Total; x2[is.na(x2)] <- 0
x3 <- Customers(CountAll) / Customers(CountAll)$Total; x3[is.na(x3)] <- 0
x4 <- Transactions(CountAll) / Customers(CountAll); x4[is.na(x4)] <- 0
x5 <- Sales(DataAll) / Transactions(CountAll); x5[is.na(x5)] <- 0
x6 <- x4 * x5
x7 <- PeakDays(DataAll)
x8 <- ValleyDays(DataAll)

## Market of Location
x9 <- GC(GCData); x9[is.na(x9)] <- 0
x10 <- GC(GCData) / GC(GCData)$Total; x10[is.na(x10)] <- 0
x11 <- (Transactions(W.Counts) + Transactions(C.Counts)) / Transactions(CountAll); x11[is.na(x11)] <- 0
x12 <- Transactions(T.Counts) / Transactions(CountAll); x12[is.na(x12)] <- 0

## Walking Distance
x13 <- Sales(W.Data) / Sales(W.Data)$Total; x13[is.na(x13)] <- 0
x14 <- Transactions(W.Counts) / Transactions(W.Counts)$Total; x14[is.na(x14)] <- 0
x15 <- Customers(W.Counts) / Customers(W.Counts)$Total; x15[is.na(x15)] <- 0
x10.1 <- GC(W.GC) / GC(W.GC)$Total; x10.1[is.na(x10.1)] <- 0
x16 <- (x15 / x10.1) * 100; x16$Total <- sum(x16[1, 1:8] * x13[1, 1:8]); x16[is.na(x16)] <- 0
x17 <- Transactions(W.Counts) / Customers(W.Counts); x17[is.na(x17)] <- 0
x18 <- Sales(W.Data) / Transactions(W.Counts); x18[is.na(x18)] <- 0
x19 <- x17 * x18
x20 <- PeakDays(W.Data)
x21 <- ValleyDays(W.Data)

## Commuters
x22 <- Sales(C.Data) / Sales(C.Data)$Total; x22[is.na(x22)] <- 0
x23 <- Transactions(C.Counts) / Transactions(C.Counts)$Total; x23[is.na(x23)] <- 0
x24 <- Customers(C.Counts) / Customers(C.Counts)$Total; x24[is.na(x24)] <- 0
x10.2 <- GC(C.GC) / GC(C.GC)$Total; x10.2[is.na(x10.2)] <- 0
x25 <- (x24 / x10.2) * 100; x25$Total <- sum(x25[1, 1:8] * x22[1, 1:8]); x25[is.na(x25)] <- 0
x26 <- Transactions(C.Counts) / Customers(C.Counts); x26[is.na(x26)] <- 0
x27 <- Sales(C.Data) / Transactions(C.Counts); x27[is.na(x27)] <- 0
x28 <- x26*x27
x29 <- PeakDays(C.Data)
x30 <- ValleyDays(C.Data)

## Travelers
x31 <- Sales(T.Data) / Sales(T.Data)$Total; x31[is.na(x31)] <- 0
x32 <- Transactions(T.Counts) / Transactions(T.Counts)$Total; x32[is.na(x32)] <- 0
x33 <- Customers(T.Counts) / Customers(T.Counts)$Total; x33[is.na(x33)] <- 0
x34 <- Transactions(T.Counts) / Customers(T.Counts); x34[is.na(x34)] <- 0
x35 <- Sales(T.Data) / Transactions(T.Counts); x35[is.na(x35)] <- 0
x36 <- x34 * x35
x37 <- PeakDays(T.Data)
x38 <- ValleyDays(T.Data)

OutputVersion <- rbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, 
                       x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26,
                       x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38)


