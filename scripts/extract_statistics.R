get_per <- function(choice_evaluate,data_frame,number) {

    d = data_frame
    # Count the numbers of times the pair made the same choice we are evaluating
    num_chosen = 0
    for (i in d$decision[d$code_id == number]) {
        if (i == choice_evaluate) {
            num_chosen = num_chosen + 1
        }
    }

    # Count the number of times the agent wins by making that decision
    num_wins = 0
    for (j in d[ (d$code_id==number & d$decision==choice_evaluate) , c("win")]) {
        if (j == TRUE) {
            num_wins = num_wins + 1
        }
    }
    if (num_chosen == 0) {
        p <- 0
    } else {
        p <- num_wins/num_chosen
    }
    return(p)
}

extract_statistics <- function(data_frame) {
    #d <- read.csv(data_frame,header=TRUE,sep=",")
    d = data_frame
    #colnames(d)[1] <- "code_id"
    #colnames(d)[2] <- "decision"
    #colnames(d)[3] <- "win"
    str <- NULL
    for (i in 0:63) {

        # Gather statistics about choices
        per_c <- get_per("C",data_frame,i)
        per_d <- get_per("D",data_frame,i)
        # print(sprintf("I:%2.i     per_c:%.3f      per_d:%.3f",i,per_c,per_d))

        # Make decision for what agent should do based on choices
        if (per_c == per_d) {
            str <- paste(str, "C",sep="")
        } else if (per_c > per_d) {
            str <- paste(str, "C",sep="")
        } else if (per_c < per_d) {
            str <- paste(str, "D",sep="")
        } else {
            str <- paste(str, " ",sep="")
        }
    }
    return(str)
}

#extract_statistics("test_statistics.csv")
