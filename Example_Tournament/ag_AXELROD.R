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
                             greeting <- "Tit for Tat always wins..."
                           },

                           receive_greeting = function(greeting = NA) {
                             self$opponent_greeting = greeting
                           },


                           check_greeting  <- function() {
                              if (self$opponent_greeting == "Lemon!") {
                                self$bid <- bid_vector[2]
                                break
                              }
                           },

                           assign_value <- function(x) {
                             number = 1

                             for (i in rev(seq(3))) {
                               if (last_round[i,1] == self$opponent_id) {
                                 if (last_round[i,5] == "cooperate") { #self
                                   if(last_round[i,4] == "defect") {
                                    number = number + 2*pow(4,i-1)
                                   } else {
                                     number = number + 4*pow(4,i-1)
                                   }
                                 }
                                } else {
                                  if (last_round[i,4] == "defect") { #self
                                   if(last_round[i,5] == "cooperate") {
                                    number = number + 1*pow(4,i-1)
                                   } else {
                                     number = number + 0*pow(4,i-1)
                                   }
                                }

                               }
                             }

                             return(number)

                             """
                             CC = 4
                             CD = 2
                             DC = 1
                             DD = 0


                             """
                           },

                           string_three_plus <- function() {
                             axelrod_seq <- ("CDCCCDDDCCCDDDCDCDDDCDDDDCDDDDDDCCCDCDDDCCCDCCDDCDDDCDDDDCDDDDDD")
                             n<- self$assign_value()
                             if (axelrod_seq[n] == "D") {
                               self$bid <- bid_vector[2]
                             } else {
                               self$bid <- bid_vector[1]
                             }

                           },

                           check_moves <- function() {
                            if (nrow(last_round) == 0)  {
                             } else if (last_round[2,1] == self$opponent_id) {
                               last_move_self <- last_round[2,5]
                               last_move_opponent <- last_round[2,4]
                             } else if (last_round[2,2] == self$opponent_id) {
                               last_move_self <- last_round[2,4]
                               last_move_opponent <- last_round[2,5]
                              } else if (last_round[1,1] == self$opponent_id) {
                               last_move_self <- last_round[1,5]
                               last_move_opponent <- last_round[1,4]
                             } else if (last_round[1,2] == self$opponent_id) {
                               last_move_self <- last_round[1,4]
                               last_move_opponent <- last_round[1,5]
                             }
                           },

                           tit_for_tat <- function() {
                            if (nrow(last_round) == 0) {
                               self$bid <- bid_vector[1]
                             } else if ((nrow(last_round) == (1 | 2)) & (last_move_self == bid_vector[1]) & (last_move_opponent == bid_vector[1])) {
                               self$bid <- sample(bid_vector, 1, prob=c(4/5,1/5))
                             } else if ((nrow(last_round) == (1 | 2)) & (last_move_self == bid_vector[1]) & (last_move_opponent == bid_vector[2])) {
                               self$bid <- sample(bid_vector, 1, prob=c(1/2,1/2))
                             } else if ((nrow(last_round) == (1 | 2)) & (last_move_self == bid_vector[2]) & (last_move_opponent == bid_vector[1])) {
                               self$bid <- sample(bid_vector, 1, prob=c(2/5,3/5))
                             } else if ((nrow(last_round) == (1 | 2)) & (last_move_self == bid_vector[2]) & (last_move_opponent == bid_vector[2])) {
                               self$bid <- bid_vector[2]
                             }
                           },

                           get_bid = function() {
                             bid_vector <- c("cooperate","defect")
                             last_move_self <- NA
                             last_move_opponent <- NA
                             last_round <- tail(subset(self$book, (id1 == self$id | id2 == self$id) & (id1 == self$opponent_id | id2 == self$opponent_id)), 3)

                             self$check_greeting()
                             self$check_moves()
                             self$tit_for_tat()
                             self$string_three_plus()
                           },

                           formulate_bid = function() {
                             self$get_bid()
                           }
                         )
)
