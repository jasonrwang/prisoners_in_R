library(R6)

Agent <- R6Class("Agent",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   greeting = "Hi!",
                   id = NULL,
                   opponent_id = NULL,
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
                     self$ response <-response 
                   },
                   
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   get_bid = function() {
                     bid_vector <- c("cooperate","defect")
                     self$bid <- sample(bid_vector,1)
                   }
                 )
)
