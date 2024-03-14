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
drawn_cards <- shuffled_deck[1:8]

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
  
  # Check if there's at least one rank with at least two cards
  return(any(rank_counts >= 2))
}


is_two_pair <- function(hand) {
  # Extract ranks
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Create a frequency table of the ranks
  rank_counts <- table(ranks)
  
  # Check if there are at least two ranks with at least two cards each
  return(sum(rank_counts >= 2) >= 2)
}


is_three_of_a_kind <- function(hand) {
  # Extract ranks
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Create a frequency table of the ranks
  rank_counts <- table(ranks)
  
  # Check if there's at least one rank with at least three cards
  return(any(rank_counts >= 3))
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
  
  # Check for at least three-of-a-kind and at least a pair
  return(any(rank_counts >= 3) && any(rank_counts >= 2))
}


is_four_of_a_kind <- function(hand) {
  # Extract ranks
  ranks <- substr(hand, 1, nchar(hand)-1)
  
  # Create a frequency table of the ranks
  rank_counts <- table(ranks)
  
  # Check if there's at least one rank with at least four cards
  return(any(rank_counts >= 4))
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

# need to update correctly, but i just want to alter the deck once for now
simulate_hands_alt <- function(n, hand_size) {
  # Define the deck
  suits <- c("S", "H", "D", "C")
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "A")
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
#simulation_results <- simulate_hands_alt(n, hand_size)

# Show time elapsed for predicting larger batches
print(Sys.time() - start_time)

# Print the results
print(simulation_results)

# As a pct
print(simulation_results / n)


############################################################################################################################################
# HAND SIMULATIONS
############################################################################################################################################

hand_size_low <- 2
hand_size_high <- 40 #52
n <- 10000

# Run simulations for hand sizes 5 to 10 and collect results
results <- list()
for (hand_size in hand_size_low:hand_size_high) {
  results[[as.character(hand_size)]] <- simulate_hands_alt(n, hand_size)
  print(hand_size)
}

# Convert results to percentages
results_perc <- lapply(results, function(x) (x / n) * 100)

# Create a data frame for the summary table
poker_hand_types <- c("Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
summary_table <- data.frame(poker_hand = poker_hand_types)

# Fill the summary table with the percentages
for (hand_size in hand_size_low:hand_size_high) {
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


g <- ggplot(summary_table_long, aes(x = as.integer(hand_size), y = probability, color = poker_hand)) +
  geom_line() +
  geom_point() + # Optional: Add points to the lines
  labs(title = "Poker Hand Probabilities Across Different Hand Sizes",
       x = "Hand Size",
       y = "Probability (%)",
       color = "Poker Hand") +
  theme_minimal() +
  theme_bw() 

p <- ggplot(summary_table_long, aes(x = as.integer(hand_size), y = probability, color = poker_hand)) +
  geom_line() +
  geom_point() + # Optional: Add points to the lines
  labs(title = "Poker Hand Probabilities Across Different Hand Sizes",
       x = "Hand Size",
       y = "Probability (%)",
       color = "Poker Hand") +
  theme_minimal() +
  theme_bw()


############################################################################################################################################
# FIRST BOSS SIMULATOR
############################################################################################################################################

# Context: tag-fishing is the act of restarting runs till you get two favorable tags for the small and big blinds. The idea is that you
  # eventually are satisfied with the tags and skip right to the first boss where you have no upgrades and a basic deck. The purpose of this
  # section is to optimize a strategy for this boss. While basic Texas Hold 'em has been studied to death, this variant with an 8 card hand,
  # multiple hands, and multiple discards makes the optimal strategy more ambiguous.

# The goal is to get a score of at least 600 in 4 hands with 3 discards and a hand size of 8. Each hand has a different base amount of Chips
  # and Mult. Ideally, the results should inform optimal play at each stage of the round (either playing a hand or discarding). 

# Ah, so ChatGPT is telling me that a reinforcement learning is going to be rough in R. I'm considering a switch to Python so I can use all
  # the latest libraries.

# I can also just use a heuristic approach such as only considering what the hand probabilities are with 1 discard of up to 5 cards
# Is a discard similar to just getting one more card from a stats POV? I think so. So just use the summary table for 13 cards?
# actually, no, because if we draw 4 of a suit and discard the 4 other cards, then it's a little worse than 1 in 4 done 3 times

#ahhh, we actually want to calculate something slightly different: the probability of getting each hand from specific starting points
# (like the one just above). So when I have one pair and discard 5 of the other cards (presumably keeping the highest 3rd card), what are 
# the odds for hitting two pair, three of a kind, etc. There are probably all that many 'starting positions' which are unique
# once we assume all suits are treated the same

# chat gpt says it's almost 60% for hitting the flush when you have 4 of a suit and draw. given 2 opportunities, it's almost 90%.
# at 3 redraws, it's 99.93%


############################################################################################################################################
# UNIQUE HANDS
############################################################################################################################################

# not actually unique, but 'similarly unique'. e.g. 2-3-4 is the same as 3-4-5 (depending on other cards)

# goal here is to calculate how many of these quasi-unique hands there are to see if developing an optimal 
# strategy for each via simulation or probability is plausible.



############################################################################################################################################
# AS-I-SEE-EM SIMULATION APPROACH
############################################################################################################################################

# Instead of finding all unique 'hand types', why not just create a function which returns the best strategy for a specific hand?
# I can then use the function to plug in the hand I'm unsure about and get all the relevant info.

# One method would be to enumerate through all possible combinations of discarding, simulate n draws for each set of discards, and then
# record the % of n in which the best poker hand of the final 8 is selected. 

# how many discard strategies are there? 

# Correct R syntax to calculate the total number of discard/draw possibilities
total_possibilities <- sum(sapply(0:5, function(k) choose(8, k)))

# Print the total possibilities
print(total_possibilities)

# 219 would be very easy to run through like 10k hands since that's only 2.19M simulations


simulate_redraws <- function(hand, n) {
  # Define the full deck
  suits <- c("H", "D", "C", "S")
  values <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
  deck <- sort(outer(values, suits, paste0))
  
  # Remove the cards in the hand from the deck
  deck <- setdiff(deck, hand)
  
  # Generate all combinations of indices for the cards to discard
  discard_combinations <- lapply(0:5, function(k) combn(1:8, k, simplify = FALSE))
  discard_combinations <- unlist(discard_combinations, recursive = FALSE)
  
  # Initialize the result list
  results <- vector("list", length(discard_combinations))
  
  # Simulate discards and redraws for each combination of discarded cards
  for (i in seq_along(discard_combinations)) {
    discard_indices <- discard_combinations[[i]]
    # Determine cards to keep based on discard indices
    kept_cards <- hand[-discard_indices]
    results[[i]] <- vector("list", n)
    
    for (j in 1:n) {
      # Shuffle the deck and draw cards to replace the discarded ones
      shuffled_deck <- sample(deck)
      num_to_draw <- 8 - length(kept_cards)
      new_cards <- shuffled_deck[seq_len(num_to_draw)]
      
      # Combine kept cards with new cards to form the new hand
      new_hand <- c(kept_cards, new_cards)
      
      # Record the new hand in the results
      results[[i]][[j]] <- new_hand
    }
  }
  
  return(results)
}



# Function to evaluate the best poker hand in a given hand
evaluate_best_hand <- function(hand) {
  # Check for each type of hand, starting from the best to the lowest
  if (is_straight_flush(hand)) {
    return("Straight Flush")
  } else if (is_four_of_a_kind(hand)) {
    return("Four of a Kind")
  } else if (is_full_house(hand)) {
    return("Full House")
  } else if (is_flush(hand)) {
    return("Flush")
  } else if (is_straight(hand)) {
    return("Straight")
  } else if (is_three_of_a_kind(hand)) {
    return("Three of a Kind")
  } else if (is_two_pair(hand)) {
    return("Two Pair")
  } else if (is_pair(hand)) {
    return("Pair")
  } else {
    return("High Card")
  }
}

# Assuming 'results' is your nested list of simulated redrawn hands
# Convert each hand into the best poker hand description
converted_results <- lapply(test, function(list_of_hands) {
  sapply(list_of_hands, evaluate_best_hand)
})


# Function to calculate the percentage of each poker hand type in a list of hands
calculate_hand_percentages <- function(hand_list) {
  # Count occurrences of each hand type
  hand_counts <- table(hand_list)
  
  # Calculate percentages
  hand_percentages <- hand_counts / sum(hand_counts)
  
  return(hand_percentages)
}

# Apply the function to each list of hands in converted_results
percentage_results <- lapply(converted_results, calculate_hand_percentages)


# Define poker hand strength ranking
hand_strength_order <- c("Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair", "Pair", "High Card")

# Initialize variables to store the results
max_percentages <- numeric(length(hand_strength_order))
max_percentage_indices <- numeric(length(hand_strength_order))
names(max_percentages) <- hand_strength_order
names(max_percentage_indices) <- hand_strength_order

# Loop through each hand type and each index in percentage_results to find max percentages and indices
for (hand_type in hand_strength_order) {
  for (i in seq_along(percentage_results)) {
    # Extract the current percentage for the hand type, if it exists
    current_percentage <- ifelse(is.null(dimnames(percentage_results[[i]])), 
                                 NA, 
                                 percentage_results[[i]][hand_type])
    
    if (!is.na(current_percentage) && current_percentage > max_percentages[hand_type]) {
      max_percentages[hand_type] <- current_percentage
      max_percentage_indices[hand_type] <- i
    }
  }
}

# Create the DataFrame
poker_hand_summary_df <- data.frame(
  Poker_Hand = names(max_percentages),
  Highest_Percentage = max_percentages,
  Max_Percentage_Index = max_percentage_indices,
  stringsAsFactors = FALSE
)


# Pull it all together into one function

hand_optimizer <- function(hand, n = 1000) {
  # Generate all combinations of indices for the cards to discard
  discard_combinations <- lapply(0:5, function(k) combn(1:8, k, simplify = FALSE))
  discard_combinations <- unlist(discard_combinations, recursive = FALSE)
  
  # Simulate n redraws for all possible combinations of discard/redraw
  simulation_results <- simulate_redraws(hand, n)
  
  # Convert cards to best hand
  converted_results <- lapply(simulation_results, function(list_of_hands) {
    sapply(list_of_hands, evaluate_best_hand)
  })
  
  # Summarize best hands by their percent occurrence for each combination
  percentage_results <- lapply(converted_results, calculate_hand_percentages)
  
  # Define poker hand strength ranking
  hand_strength_order <- c("Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair", "Pair", "High Card")
  
  # Initialize variables to store the results
  max_percentages <- numeric(length(hand_strength_order))
  max_percentage_indices <- numeric(length(hand_strength_order))
  names(max_percentages) <- hand_strength_order
  names(max_percentage_indices) <- hand_strength_order
  
  # Loop through each hand type and each index in percentage_results to find max percentages and indices
  for (hand_type in hand_strength_order) {
    for (i in seq_along(percentage_results)) {
      # Extract the current percentage for the hand type, if it exists
      current_percentage <- ifelse(is.null(dimnames(percentage_results[[i]])), 
                                   NA, 
                                   percentage_results[[i]][hand_type])
      
      if (!is.na(current_percentage) && current_percentage > max_percentages[hand_type]) {
        max_percentages[hand_type] <- current_percentage
        max_percentage_indices[hand_type] <- i
      }
    }
  }
  
  # Map Max_Percentage_Index to actual discard combinations
  max_discard_combinations <- sapply(max_percentage_indices, function(index) {
    if (!is.na(index) && index > 0) {
      # Get the discard combination for this index
      discard_indices <- discard_combinations[[index]]
      # Map indices to card values in the hand
      discard_cards <- hand[discard_indices]
      # Combine into a comma-separated string
      paste(discard_cards, collapse = ", ")
    } else {
      "" # Use an appropriate placeholder if no discard combination is applicable
    }
  })
  
  # Create the updated DataFrame
  poker_hand_summary_df <- data.frame(
    Poker_Hand = names(max_percentages),
    Highest_Percentage = max_percentages,
    Discard_Combination = max_discard_combinations,
    stringsAsFactors = FALSE
  )
  
  # Remove redundant row names
  rownames(poker_hand_summary_df) <- NULL
  
  # Determine optimal hand (straight or better) and discard
  optimal_row <- which(poker_hand_summary_df$Highest_Percentage == max(poker_hand_summary_df[1:5,]$Highest_Percentage))
  optimal_hand <- poker_hand_summary_df$Poker_Hand[optimal_row]
  optimal_discard <- poker_hand_summary_df$Discard_Combination[optimal_row]
  
  # Combine and display the text information
  cat("Input hand:", paste(hand, collapse=", "), "\n")
  cat("Optimal hand:", optimal_hand, "\n")
  cat("Discard:", optimal_discard, "\n")
  cat("\n") # Linebreak
  
  # Display the DataFrame
  print(poker_hand_summary_df)

  
  #return(poker_hand_summary_df)
}




