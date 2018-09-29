library(R6)
Agent <- R6Class("Agent",
                         
                         public = list(
                           bid = NULL,
                           book = NULL,
                           greeting = NULL,
                           id = NULL,
                           opponent_id = NULL,
                           opponent_greeting = NULL,
                           round = NULL, 
                           response = NULL,
                           
                           set_book = function(book=NA) {
                             self$book <- book
                           },
                           
                           set_id = function(id=NA) {
                             self$id = id
                           },
                           
                           set_opponent_id = function(opponent_id=NA) {
                             self$opponent_id = opponent_id
                           },
                           
                           set_response = function(response=NA) {
                             self$response <- response
                           },
                           
                           set_round = function(round=NA) {
                             self$round <- round
                           },
                           
                           set_greeting = function() {
                             greeting <- "Quick, what does love start with? L-O"
                           },
                           
                           receive_greeting = function(greeting = NA) {
                             self$opponent_greeting = greeting
                           },
                           
                           get_bid = function() {
                             bid_vector <- c("cooperate","defect")
                             last_move_self <- NA
                             last_move_opponent <- NA
                             last_round <- tail(subset(self$book, (id1 == self$id | id2 == self$id) & 
                                                         (id1 == self$opponent_id | id2 == self$opponent_id)), 1)
                             set.seed(1)
                             
                             if (nrow(last_round)==0)  {
                             } else if (last_round[1,1] == self$id) {
                               last_move_self <- last_round[1,4]
                               last_move_opponent <- last_round[1,5]
                             } else if (last_round[1,1] == self$opponent_id) {
                               last_move_self <- last_round[1,5]
                               last_move_opponent <- last_round[1,4]
                             }
                             
                             if (nrow(last_round)==0) {
                               self$bid <- bid_vector[1]
                             } else if ((last_move_self == bid_vector[1]) & (last_move_opponent == bid_vector[1])) {
                               self$bid <- sample(bid_vector, 1, prob=c(4/5,1/5))
                             } else if ((last_move_self == bid_vector[1]) & (last_move_opponent == bid_vector[2])) {
                               self$bid <- sample(bid_vector, 1, prob=c(1/2,1/2))
                             } else if ((last_move_self == bid_vector[2]) & (last_move_opponent == bid_vector[1])) {
                               self$bid <- sample(bid_vector, 1, prob=c(2/5,3/5))
                             } else if ((last_move_self == bid_vector[2]) & (last_move_opponent == bid_vector[2])) {
                               self$bid <- bid_vector[2]
                             }
                           },
                           
                           formulate_bid = function() {
                             self$get_bid()
                           }
                         )
)