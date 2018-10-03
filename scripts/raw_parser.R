raw_parser = function(file_name) {

    tourn_raw = read.csv(file_name,sep=";",stringsAsFactors = FALSE)
    num_agents = max(unique(tourn_raw$id1)) # Only works if IDs are sequential!
    outcome_sequences = array(data=NA,dim=c(num_agents,num_agents,60)) # 60 is wasteful. See improvement suggestion below.
    # typeof(outcome_sequences) ## just for testing

    ## To get data about the number of games played between each pair
    # seq_length_list = c()

    for (i_id1 in seq(num_agents-1)) { # Iterate through all the agents
        id1_index = which(tourn_raw$id1 %in% i_id1 | tourn_raw$id2 %in% i_id1) # Find where agent plays

        for (i_id2 in seq(i_id1+1,num_agents)) { # Iterate through remaining combinations
            id2_index = which(tourn_raw$id1 %in% i_id2 | tourn_raw$id2 %in% i_id2) # Find where agent #i_id2 plays

            ## The below can be an improvement, but doesn't quite work yet.
            ## The "list" cannot replace the single element.
            # The number of times i_id1 attacks i_id2 to help pre-allocate list
            match_id = which(id1_index %in% id2_index)
            seq_list_len = length(match_id)
            #outcome_sequences[i_id1,i_id2] = vector("list",seq_list_len) # pre-allocate

            # To get data about the number of games played between each pair
            #seq_length_list = c(seq_length_list,seq_list_len)

            for (i in seq(seq_list_len)){
                outcome_sequences[i_id1,i_id2,i] = axelrod_code(
                    tourn_raw$bid1[ id1_index [ match_id[i] ] ],
                    tourn_raw$bid2[ id1_index [ match_id[i] ] ])
            }
        }
    }
    return(outcome_sequences)
}

axelrod_code = function(bid1, bid2) {
    # Determines the letter code associated with an interaction
    if (is.na(bid1)) {
        next
    }
    else if (bid1 == 'cooperate') {
       if (bid1 == bid2) { # CC -> R -> 3
            return(3)
       } else { # CD -> S -> 2
            return(2)
       }
    }  else if (bid1 == 'defect') {
        if (bid1 == bid2) { # DD -> P -> 0
            return(0)
        } else { # DC -> T -> 1
            return(1)
        }
    } else {
       print("Error in cooperate-defect matching!")
       break
    }
}

axelrod_encode = function(a_code) {
    num_agents = nrow(a_code) # Determine number of agents, n, in 'n x n' matrix input
    code_id = decision = win = c()

    for (i in seq(num_agents)) { # The "attacking" agent
        for (j in seq(num_agents)) { # The "defending" agent
            # Agents cannot play themselves, so skip in this case.
            if (i == j) { # This could be improved by stripping i_id1 from i_id2's for loop declaration
                next
            }

            for (k in seq(length(a_code[i,j,])-3)) {

                if (is.na(a_code[i,j,k+3])) { ## Due to dumb coding above, NA exists in list
                    next
                }

                encoded_seq = (a_code[i,j,k] * 4^2 + a_code[i,j,k+1] * 4^1 + a_code[i,j,k+2] * 4^0)
                code_id = c(code_id,encoded_seq)

                if (a_code[i,j,k+3] > 1) { # 0 and 1 are defect; 2 and 3
                    decision = c(decision,'C')
                } else {
                    decision = c(decision,'D')
                }

                if (a_code[i,j,k+3] %% 2) { # Consider R and T (1 and 3) as win
                    win = c(win,TRUE)
                } else {
                    win = c(win,FALSE)
                }
            }
        }
    }
    return(data.frame(code_id,decision,win, stringsAsFactors = FALSE))
}

## For testing
# raw_parser("validate.csv")
