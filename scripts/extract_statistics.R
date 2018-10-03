get_per <- function(something, file_name,number) {
    #d <- read.csv(file_name,header=TRUE,sep=",")
    d=file_name
    #count the numbers of times the pair made the same decision
    ccnt=0
    for (i in d$decision[d$code_id == number]) {
        if (i == something) {
            ccnt= ccnt+1
        }
    }
    #print(ccnt)

    #count the number of times the agent wins after the three decision
    Tcnt=0
    for (j in d[(d$code_id==number & d$decision==something), c("win")]) {
        if (j == TRUE) {
            Tcnt= Tcnt+1
        }
    }
    #print(Tcnt)
    p <- Tcnt/ccnt
    return(p)
}

extract_statistics <- function( file_name) {
    #d <- read.csv(file_name,header=TRUE,sep=",")
    d= file_name
    #colnames(d)[1] <- "code_id"
    #colnames(d)[2] <- "decision"
    #colnames(d)[3] <- "win"
    str <- NULL
    for (i in 0:63) {
        per_c <- get_per("C", file_name,i)
        #print("next")
        per_d <- get_per("D", file_name,i)
        #print(per_c)
        #print(per_d)
        if (is.na(per_c) | is.na(per_d)) {
            #str <- paste(str, " ",sep="")
            next
        } else if (per_c > per_d) {
            str <- paste(str, "C",sep="")
        } else if (per_c < per_d) {
            str <-paste(str, "D",sep="")
        } else {
            str <-paste(str, "NA",sep="")
        }
    }
    return(str)
}

#extract_statistics("test_statistics.csv")
