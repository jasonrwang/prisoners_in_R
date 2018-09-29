raw_parser = function(file_name) {

    tourn_raw = read.csv(file_name,sep=",",stringsAsFactors = FALSE)
    num_agents = max(unique(tourn_raw$id1)) # Only works if IDs are sequential!
    outcome_sequences = array(data=NA,dim=c(num_agents,num_agents,40)) # 40 is wasteful. See improvement suggestion below.
    # typeof(outcome_sequences) ## just for testing

    ## To get data about the number of games played between each pair
    # seq_length_list = c()

    for (i_id1 in seq(num_agents)) { # The "attacking" agent
        id1_index = which(tourn_raw$id1 %in% i_id1) # Find where agent #i_id1 plays

        for (i_id2 in seq(num_agents)) { # The "defending" agent
            # Agents cannot play themselves, so skip in this case.
            if (i_id1 == i_id2) { # This could be improved by stripping i_id1 from i_id2's for loop declaration
                next
            }

            id2_index = which(tourn_raw$id2 %in% i_id2) # Find where agent #i_id2 plays

            ## The below can be an improvement, but doesn't quite work yet.
            ## The "list" cannot replace the single element.
            # The number of times i_id1 attacks i_id2 to help pre-allocate list
            seq_list_len = length(which(id1_index %in% id2_index))
            #outcome_sequences[i_id1,i_id2] = vector("list",seq_list_len) # pre-allocate

            # To get data about the number of games played between each pair
            #seq_length_list = c(seq_length_list,seq_list_len)

            for (i in seq(seq_list_len)){
                outcome_sequences[i_id1,i_id2,i] = axelrod_code(tourn_raw$bid1[id1_index[i]],tourn_raw$bid2[id1_index[i]])
            }
        }
    }
    return(outcome_sequences)
}

axelrod_code = function(bid1, bid2) {
    # Determines the letter code associated with an interaction
    if (bid1 == 'cooperate') {
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

## For testing
# raw_parser("validate.csv")