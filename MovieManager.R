library(ggplot2)

# Define a list to store movie ratings
movies <- list()

# Function to add a movie and its rating
add_movie <- function(movie_name, rating) {
  movies[[movie_name]] <<- rating
  cat("Added:", movie_name, "with rating", rating, "\n")
}

# Function to display all movie ratings
display_movies <- function() {
  if (length(movies) == 0) {
    cat("No movies available.\n")
  } else {
    cat("Movie Ratings:\n")
    for (movie in names(movies)) {
      cat(movie, ":", movies[[movie]], "\n")
    }
  }
}

# Function to update a movie
update_movie <- function(movie_name, new_rating) {
  if (movie_name %in% names(movies)) {
    movies[[movie_name]] <<- new_rating
    cat("Updated:", movie_name, "to rating", new_rating, "\n")
  } else {
    cat("Movie not found.\n")
  }
}

# Function to remove a movie
delete_movie <- function(movie_name) {
  if (movie_name %in% names(movies)) {
    movies[[movie_name]] <<- NULL
    cat("Deleted rating for:", movie_name, "\n")
  } else {
    cat("Movie not found.\n")
  }
}

# Function to plot movie ratings
plot_ratings <- function() {
  if (length(movies) == 0) {
    cat("No movie ratings available to plot.\n")
  } else {
    movie_names <- names(movies)
    ratings <- unlist(movies)
    data <- data.frame(Movie = movie_names, Rating = ratings)
    
    ggplot(data, aes(x = Movie, y = Rating)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      labs(title = "Movie Ratings", x = "Movie", y = "Rating") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}

# Test cases
add_movie("Inception", 9)
add_movie("The Matrix", 8)
add_movie("Master of Disguise", 1)
add_movie("Jack and Jill", 4)
add_movie("Megamind", 10)
display_movies()
update_movie("Inception", 10)
delete_movie("The Matrix")
display_movies()
plot_ratings()

