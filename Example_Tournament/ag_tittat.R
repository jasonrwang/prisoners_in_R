library(R6)

Agent <- R6Class("Agent",
                 
                 public = list(
                   #bid states if we defect or cooperate
                   bid = NULL,
                   #book is the dataset of the played tournament until this moment
                   book = NULL,
                   #before giving the bid the agents will exchange a greeting
                   greeting = "Hello, this is group 1.",
                   #our id number
                   id = NULL,
                   #the id number of our opponent
                   opponent_id = NULL,
                   #the number of the current round which is used to store the bids in the book
                   round = NULL,
                   #the recieved greeting from the opponent
                   response = NULL,
                   
                   #get the dataset until this moment
                   set_book = function(book=NA) {
                     self$book <- book
                   },
                   
                   #set our id for usage in the dataset
                   set_id = function(id=NA) {
                     self$id = id
                   },
                   
                   #set the opponent id for usage in the dataset
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   #set the greeting of the opponent
                   set_response = function(response=NA) {
                     self$ response <-response
                   },
                   
                   #set the round number for usage in the dataset
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   #set your bid for comparison to the opponent
                   get_bid = function() {
                     #bid_vector gives the two options that are possible for the agent
                     bid_vector <- c("cooperate","defect"),
                     #last_interact checks what the opponent did the last time it was in the tournament
                     last <- last_interact(book, opponent_id),
                     #our agent will do the same as the other agent did last time, or, if this is the first meeting, it will defect
                     if (is.NULL(last)) {
                       self$bid <- "defect"
                     } else {
                       self$bid <- last
                     }
                   },
                   
                   #check what the opponent did the last time it was in the tournament
                   last_interact <- function(tournament, a) {
                     t<- tournament
                     rec1 <- t[(t$id1==a), c("round", "bid2")]
                     rec2 <- t[(t$id2==a), c("round", "bid1")]
                     names(rec1) <- c("round", "bid")
                     names(rec2) <- c("round", "bid")
                     all_rec <- rbind(rec1,rec2)
                     
                     if (nrow(all_rec) > 0) {
                       round <- all_rec$round
                       n=max(round)
                       last <- all_rec[all_rec$round==n, "bid"]
                     } else {
                       last <- NULL
                     }
                     return(last)
                   }
                 )
)