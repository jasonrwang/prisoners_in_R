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
      greeting <- "Lemon!"
    },
    
    receive_greeting = function(greeting = NA) {
      self$opponent_greeting = greeting
    },
    
    get_bid = function() {
      self$bid <- "defect"
    },
    
    formulate_bid = function() {
      self$get_bid()
    }
  )
)