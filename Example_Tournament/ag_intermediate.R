library(R6)

Agent <- R6Class(
  "Agent",
  
  public = list(
    bid = NULL,
    book = NULL,
    greeting = NULL,
    id = NULL,
    opponent_id = NULL,
    opponent_greeting = NULL,
    round = NULL,
    response = NULL,
    
    set_book = function(book = NA) {
      self$book <- book
    },
    
    set_id = function(id = NA) {
      self$id = id
    },
    
    set_opponent_id = function(opponent_id = NA) {
      self$opponent_id = opponent_id
    },
    
    set_response = function(response = NA) {
      self$response <- response
    },
    
    set_round = function(round = NA) {
      self$round <- round
    },
    
    set_greeting = function() {
      greeting <- "Hi!"
    },
    
    receive_greeting = function(greeting = NA) {
      self$opponent_greeting = greeting
    },
    
    get_bid = function() {
      strategy_list1 <- self$book[['bid1']]
      strategy_list2 <- self$book[['bid2']]
      strategy_list <- c(as.character(strategy_list1),as.character(strategy_list2))
      if(length(strategy_list)!=0)
        self$bid <- sample(strategy_list,1)
      else
        self$bid <- "defect"
    },
    
    formulate_bid = function() {
      self$get_bid()
    }
  )
)