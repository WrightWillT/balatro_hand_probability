############################################################################################################################################
# BALATRO HAND PROBABILITY CALCULATOR OVERVIEW
############################################################################################################################################

# Started: 2024-03-06
# Purpose: Run simulations to calculate the probability of getting a given poker hand based on deck composition and hand size.


############################################################################################################################################
# LOAD LIBRARIES
############################################################################################################################################

library(tidyverse)


############################################################################################################################################
# DEFINE DECK(S)
############################################################################################################################################

# A deck is comprised of cards which have the properties Rank and Suit

# Creating the deck
suits <- c("S", "H", "D", "C")
ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
deck <- sort(outer(ranks, suits, paste0))

# Shuffling the deck
shuffled_deck <- sample(deck, length(deck))

# Drawing 5 cards
drawn_cards <- shuffled_deck[1:5]

# Display drawn cards
print(drawn_cards)

############################################################################################################################################
# HAND TESTS
############################################################################################################################################

is_pair <- function(hand) {
  # Extract ranks
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Create a frequency table of the ranks
  rank_counts <- table(ranks)
  
  # Check if there's at least one rank with exactly two cards
  return(any(rank_counts == 2))
}


is_two_pair <- function(hand) {
  # Extract ranks
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Create a frequency table of the ranks
  rank_counts <- table(ranks)
  
  # Check if there are at least two ranks with exactly two cards each
  return(sum(rank_counts == 2) == 2)
}


is_three_of_a_kind <- function(hand) {
  # Extract ranks
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Create a frequency table of the ranks
  rank_counts <- table(ranks)
  
  # Check if there's at least one rank with exactly three cards
  return(any(rank_counts == 3))
}


is_straight <- function(hand) {
  # Helper function to convert ranks to numeric values, considering Ace as both 1 and 14
  rank_to_numeric <- function(rank) {
    rank_values <- c("2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9, 
                     "T" = 10, "J" = 11, "Q" = 12, "K" = 13, "A" = 1)
    return(rank_values[rank])
  }
  
  # Extract ranks from the hand
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Convert ranks to numeric, handling Ace as high and low
  numeric_ranks <- unlist(lapply(ranks, rank_to_numeric))
  if("A" %in% ranks) {
    numeric_ranks <- c(numeric_ranks, 14) # Add Ace as high
  }
  
  # Sort and get unique ranks
  unique_ranks <- sort(unique(numeric_ranks))
  
  # Check for consecutive sequences
  if (length(unique_ranks) >= 5) {
    for(i in 1:(length(unique_ranks) - 4)) {
      # Now we know i + 4 will not exceed the length of unique_ranks,
      # so we don't need an inner condition to check bounds.
      if(all(diff(unique_ranks[i:(i + 4)]) == 1)) {
        found_straight <- TRUE
        break
      }
    }
  }
  
  # Check for low-Ace straight
  if(all(c(2,3,4,5,14) %in% unique_ranks)) {
    return(TRUE)
  }
  
  return(FALSE)
}


is_flush <- function(hand) {
  # Extract suits
  suits <- substr(hand, nchar(hand), nchar(hand))
  
  # Check if at least 5 of the same suit
  return(max(table(suits)) >= 5)
}


is_full_house <- function(hand) {
  # Extract ranks
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Create a frequency table of the ranks
  rank_counts <- table(ranks)
  
  # Check for three-of-a-kind and a pair
  return(any(rank_counts == 3) && any(rank_counts == 2))
}


is_four_of_a_kind <- function(hand) {
  # Extract ranks
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Create a frequency table of the ranks
  rank_counts <- table(ranks)
  
  # Check if there's at least one rank with exactly four cards
  return(any(rank_counts == 4))
}


# Adjusted function to check for a straight, correctly handling Aces and avoiding invalid indexing
is_straight_helper <- function(ranks) {
  # Define rank values, including Aces as both 1 and 14 to handle them being high and low
  rank_values <- c("2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9,
                   "T" = 10, "J" = 11, "Q" = 12, "K" = 13, "A" = 1)
  numeric_ranks <- unlist(lapply(ranks, function(rank) rank_values[rank]))
  
  # Include Ace as both high and low
  if ("A" %in% ranks) {
    numeric_ranks <- c(numeric_ranks, 14)
  }
  
  # Sort and get unique ranks to remove duplicates
  sorted_ranks <- sort(unique(numeric_ranks))
  
  # Initialize a flag for finding a straight
  found_straight <- FALSE
  
  # Check for consecutive sequences
  if (length(sorted_ranks) >= 5) {
    for(i in 1:(length(sorted_ranks) - 4)) {
      # Now we know i + 4 will not exceed the length of sorted_ranks,
      # so we don't need an inner condition to check bounds.
      if(all(diff(sorted_ranks[i:(i + 4)]) == 1)) {
        found_straight <- TRUE
        break
      }
    }
  }
  
  return(found_straight)
}


is_straight_flush <- function(hand) {
  # Helper function is defined above
  
  # Extract suits and ranks
  suits <- substr(hand, nchar(hand), nchar(hand))
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Check each suit for a straight
  for(suit in unique(suits)) {
    if(is_straight_helper(ranks[suits == suit])) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}



############################################################################################################################################
# HAND SIMULATION SETUP
############################################################################################################################################

# Function to simulate drawing a hand and checking for hand types
simulate_hands <- function(n, hand_size) {
  # Define the deck
  suits <- c("S", "H", "D", "C")
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
  deck <- sort(outer(ranks, suits, paste0))
  
  # Initialize counters for each hand type
  counts <- setNames(
    as.numeric(rep(0, 8)),
    c("Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
  )
  
  for (i in 1:n) {
    # Shuffle the deck and draw hand_size cards
    hand <- sample(deck, hand_size)
    
    # Check for each hand type and update counters
    if (is_pair(hand)) counts["Pair"] <- counts["Pair"] + 1
    if (is_two_pair(hand)) counts["Two Pair"] <- counts["Two Pair"] + 1
    if (is_three_of_a_kind(hand)) counts["Three of a Kind"] <- counts["Three of a Kind"] + 1
    if (is_straight(hand)) counts["Straight"] <- counts["Straight"] + 1
    if (is_flush(hand)) counts["Flush"] <- counts["Flush"] + 1
    if (is_full_house(hand)) counts["Full House"] <- counts["Full House"] + 1
    if (is_four_of_a_kind(hand)) counts["Four of a Kind"] <- counts["Four of a Kind"] + 1
    if (is_straight_flush(hand)) counts["Straight Flush"] <- counts["Straight Flush"] + 1
  }
  
  return(counts)
}


# Example usage: Run 10000 simulations for hands of size 7
n <- 10000
hand_size <- 7

# Record the start time
start_time <- Sys.time()

# Run simulation
simulation_results <- simulate_hands(n, hand_size)

# Show time elapsed for predicting larger batches
print(Sys.time() - start_time)

# Print the results
print(simulation_results)

# As a pct
print(simulation_results / n)


############################################################################################################################################
# 1MM HAND SIMULATIONS FOR HANDS OF SIZE 5-10
############################################################################################################################################

# Run simulations for hand sizes 5 to 10 and collect results
results <- list()
for (hand_size in 5:10) {
  results[[as.character(hand_size)]] <- simulate_hands(1e6, hand_size)
}

# Convert results to percentages
results_perc <- lapply(results, function(x) (x / 1e6) * 100)

# Create a data frame for the summary table
poker_hand_types <- c("Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
summary_table <- data.frame(poker_hand = poker_hand_types)

# Fill the summary table with the percentages
for (hand_size in 5:10) {
  hand_size_col <- paste("hand_size", hand_size, sep = "_")
  # Extracting percentages for the current hand size
  summary_table[[hand_size_col]] <- unlist(lapply(results_perc[[as.character(hand_size)]], function(x) x))
}

View(summary_table)


############################################################################################################################################
# VISUALIZE RESULTS
############################################################################################################################################

summary_table_long <- summary_table %>%
  pivot_longer(cols = starts_with("hand_size"),
               names_to = "hand_size",
               names_prefix = "hand_size_",
               values_to = "probability")


summary_table_long$poker_hand <- factor(
  summary_table_long$poker_hand,
  levels = c("Pair", "Two Pair", "Three of a Kind", "Flush", 
             "Straight", "Full House", "Four of a Kind", "Straight Flush")
  )


ggplot(summary_table_long, aes(x = as.integer(hand_size), y = probability, color = poker_hand)) +
  geom_line() +
  geom_point() + # Optional: Add points to the lines
  labs(title = "Poker Hand Probabilities Across Different Hand Sizes",
       x = "Hand Size",
       y = "Probability (%)",
       color = "Poker Hand") +
  theme_minimal() +
  scale_x_continuous(breaks = 5:10) + # Ensure hand sizes are treated as discrete values
  theme_bw() 










