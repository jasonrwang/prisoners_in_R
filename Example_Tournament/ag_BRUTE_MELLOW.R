library(R6)
Agent <- R6Class("Agent",
  #this part was already given
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
      greeting <- "Back to PRESTO..."
    },

    receive_greeting = function(greeting = NA) {
      self$opponent_greeting = greeting
    },

    #this code was made to get a certain bid for the agent
    get_bid = function() {
      bid_vector <- c("cooperate","defect")
      #creating a few variables which will be used later on
      last_move_self <- NA
      last_move_opponent <- NA
      #create a database with the last rounds
      last_round <- tail(subset(self$book, (id1 == self$id | id2 == self$id) & (id1 == self$opponent_id | id2 == self$opponent_id)), 3)
      last_round_single <- tail(subset(self$book, (id1 == self$id | id2 == self$id) & (id1 == self$opponent_id | id2 == self$opponent_id)), 1)
      #the string which will decide what the agent should do according to the paper by Mittal and Deb
      axelrod_seq <- c("D","D","D","D","D","D","C","C","C","D","D","C","D","D","D","D","D","D","C","D","D","D","D","C","D","C","C","D","D","C","D","D","D","D","C","D","C","C","C","D","D","C","D","C","D","C","D","C","D","D","C","C","D","D","D","D","C","D","D","D","D","D","C","C","C","D","C","C","D","D")
      #this is the index for the bid, it starts at 1 because R start at 1 with counting
      number <- 1

      #first check what the bid was of the last round we played this agent
      if (nrow(last_round_single) == 0)  {
      } else if (last_round_single[1,1] == self$id) {
        last_move_self <- last_round_single[1,4]
        last_move_opponent <- last_round_single[1,5]
      } else if (last_round_single[1,1] == self$opponent_id) {
        last_move_self <- last_round_single[1,5]
        last_move_opponent <- last_round_single[1,4]
      }

      #this is a simple tit for that function for the first couple of rounds
      if (nrow(last_round) == 3) {
      } else if (nrow(last_round_single)==0) {
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

      #this is the function that looks at the last three rounds and adds it up to a number which is used to find the bid in the string
      if (nrow(last_round) < 3) {
      } else if ((last_round[3,1] == self$id) & (last_round[3,2] == self$opponent_id) & (last_round[3,4] == bid_vector[1]) & (last_round[3,5] == bid_vector[1])) {
      number <- number + (3 * 4^2)
      } else if ((last_round[3,1] == self$id) & (last_round[3,2] == self$opponent_id) & (last_round[3,4] == bid_vector[2]) & (last_round[3,5] == bid_vector[1])) {
      number <- number + (1 * 4^2)
      } else if ((last_round[3,1] == self$id) & (last_round[3,2] == self$opponent_id) & (last_round[3,4] == bid_vector[1]) & (last_round[3,5] == bid_vector[2])) {
      number <- number + (2 * 4^2)
      } else if ((last_round[3,1] == self$id) & (last_round[3,2] == self$opponent_id) & (last_round[3,4] == bid_vector[2]) & (last_round[3,5] == bid_vector[2])) {
      number <- number + (0 * 4^2)
      } else if ((last_round[3,1] == self$opponent_id) & (last_round[3,2] == self$id) & (last_round[3,4] == bid_vector[1]) & (last_round[3,5] == bid_vector[1])) {
      number <- number + (3 * 4^2)
      } else if ((last_round[3,1] == self$opponent_id) & (last_round[3,2] == self$id) & (last_round[3,4] == bid_vector[2]) & (last_round[3,5] == bid_vector[1])) {
      number <- number + (2 * 4^2)
      } else if ((last_round[3,1] == self$opponent_id) & (last_round[3,2] == self$id) & (last_round[3,4] == bid_vector[1]) & (last_round[3,5] == bid_vector[2])) {
      number <- number + (1 * 4^2)
      } else if ((last_round[3,1] == self$opponent_id) & (last_round[3,2] == self$id) & (last_round[3,4] == bid_vector[2]) & (last_round[3,5] == bid_vector[2])) {
      number <- number + (0 * 4^2)
      }

      #this is the function that looks at the last three rounds and adds it up to a number which is used to find the bid in the string
      if (nrow(last_round) < 3) {
      } else if ((last_round[2,1] == self$id) & (last_round[2,2] == self$opponent_id) & (last_round[2,4] == bid_vector[1]) & (last_round[2,5] == bid_vector[1])) {
        number <- number + (3 * 4^1)
      } else if ((last_round[2,1] == self$id) & (last_round[2,2] == self$opponent_id) & (last_round[2,4] == bid_vector[2]) & (last_round[2,5] == bid_vector[1])) {
        number <- number + (1 * 4^1)
      } else if ((last_round[2,1] == self$id) & (last_round[2,2] == self$opponent_id) & (last_round[2,4] == bid_vector[1]) & (last_round[2,5] == bid_vector[2])) {
        number <- number + (2 * 4^1)
      } else if ((last_round[2,1] == self$id) & (last_round[2,2] == self$opponent_id) & (last_round[2,4] == bid_vector[2]) & (last_round[2,5] == bid_vector[2])) {
        number <- number + (0 * 4^1)
      } else if ((last_round[2,1] == self$opponent_id) & (last_round[2,2] == self$id) & (last_round[2,4] == bid_vector[1]) & (last_round[2,5] == bid_vector[1])) {
        number <- number + (3 * 4^1)
      } else if ((last_round[2,1] == self$opponent_id) & (last_round[2,2] == self$id) & (last_round[2,4] == bid_vector[2]) & (last_round[2,5] == bid_vector[1])) {
        number <- number + (2 * 4^1)
      } else if ((last_round[2,1] == self$opponent_id) & (last_round[2,2] == self$id) & (last_round[2,4] == bid_vector[1]) & (last_round[2,5] == bid_vector[2])) {
        number <- number + (1 * 4^1)
      } else if ((last_round[2,1] == self$opponent_id) & (last_round[2,2] == self$id) & (last_round[2,4] == bid_vector[2]) & (last_round[2,5] == bid_vector[2])) {
        number <- number + (0 * 4^1)
      }

      #this is the function that looks at the last three rounds and adds it up to a number which is used to find the bid in the string
      if (nrow(last_round) < 3) {
      } else if ((last_round[1,1] == self$id) & (last_round[1,2] == self$opponent_id) & (last_round[2,4] == bid_vector[1]) & (last_round[2,5] == bid_vector[1])) {
        number <- number + (3 * 4^0)
      } else if ((last_round[1,1] == self$id) & (last_round[1,2] == self$opponent_id) & (last_round[1,4] == bid_vector[2]) & (last_round[1,5] == bid_vector[1])) {
        number <- number + (1 * 4^0)
      } else if ((last_round[1,1] == self$id) & (last_round[1,2] == self$opponent_id) & (last_round[1,4] == bid_vector[1]) & (last_round[1,5] == bid_vector[2])) {
        number <- number + (2 * 4^0)
      } else if ((last_round[1,1] == self$id) & (last_round[1,2] == self$opponent_id) & (last_round[1,4] == bid_vector[2]) & (last_round[1,5] == bid_vector[2])) {
        number <- number + (0 * 4^0)
      } else if ((last_round[1,1] == self$opponent_id) & (last_round[1,2] == self$id) & (last_round[1,4] == bid_vector[1]) & (last_round[1,5] == bid_vector[1])) {
        number <- number + (3 * 4^0)
      } else if ((last_round[1,1] == self$opponent_id) & (last_round[1,2] == self$id) & (last_round[1,4] == bid_vector[2]) & (last_round[1,5] == bid_vector[1])) {
        number <- number + (2 * 4^0)
      } else if ((last_round[1,1] == self$opponent_id) & (last_round[1,2] == self$id) & (last_round[1,4] == bid_vector[1]) & (last_round[1,5] == bid_vector[2])) {
        number <- number + (1 * 4^0)
      } else if ((last_round[1,1] == self$opponent_id) & (last_round[1,2] == self$id) & (last_round[1,4] == bid_vector[2]) & (last_round[1,5] == bid_vector[2])) {
        number <- number + (0 * 4^0)
      }

      #this function uses the number to see what letter is at that index in the provided string
      if (nrow(last_round) < 3) {
      } else if (axelrod_seq[number] == "D") {
        self$bid <- bid_vector[2]
      } else if (axelrod_seq[number] == "C") {
        self$bid <- bid_vector[1]
      }

    },

    formulate_bid = function() {
      self$get_bid()
    }
  )
)
