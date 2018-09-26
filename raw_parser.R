tourn_raw = read.csv("tournament.csv",sep=",",stringsAsFactors = FALSE)

num_agents = max(unique(tourn_raw$id1))

outcome_sequences = array(data=NA,dim=c(num_agents,num_agents,50)) # 50 is wasteful. See improvement suggestion below.
typeof(outcome_sequences) # just for testing

for (i_id1 in seq(num_agents)) { # The "attacking" agent
    id1_index = which(tourn_raw$id1 %in% i_id1) # Find where agent #i_id1 plays

    for (i_id2 in seq(num_agents)) { # The "defending" agent
        # Agents cannot play themselves, so skip in this case.
        if (i_id1 == i_id2) {
            next
        }
        # cat("Agent 1: %i | Agent 2: %i",i_id1,i_id2)
        id2_index = which(tourn_raw$id2 %in% i_id2) # Find where agent #i_id2 plays

        ## The below can be an improvement, but doesn't quite work yet.
        ## The "list" cannot replace the single element.
        # The number of times i_id1 attacks i_id2 to help pre-allocate list
        seq_list_len = length(which(id1_index %in% id2_index))
        #outcome_sequences[i_id1,i_id2] = vector("list",seq_list_len) # pre-allocate
        
        for (i in seq(seq_list_len)){
            # Insert function to determine the proper output sequence
            outcome_sequences[i_id1,i_id2,i] = tourn_raw$bid1[id1_index[i]]
            print(outcome_sequences[i_id1,i_id2,]) # This thing prints, but it doesn't keep to validate!
            #outcome_sequences[i_id1][i_id2].append(tourn_raw$id1[i])
            #list.append(outcome_sequences[[i_id1,i_id2]],tourn_raw$bid1[i])
        }
    }
}