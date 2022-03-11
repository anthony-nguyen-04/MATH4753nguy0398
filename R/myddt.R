#' Takes in the DDT data set and filters the data to only contain a given SPECIES.
#' Next, it saves this filtered data to a CSV file. Then, it graphs LENGTH vs
#' WEIGHT, colored according to RIVER. Afterwards, it creates a relative
#' frequency table, in relation to the RIVER variable. Finally, it outputs
#' the DDT data set before filtering, the data set after filtering, and
#' the relative frequency table from before.
#'
#' @param df the data frame for the data
#' @param SPECIES the SPECIES type to use to filter
#'
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 aes ggplot geom_point geom_smooth ggtitle
#' @importFrom utils write.csv
#'
#' @return the list of the data before filtering, the data after filtering,
#' and the relative frequency table for RIVER.
#'
#' @export

myddt <- function(df, SPECIES){
  RIVER <-  WEIGHT <- LENGTH <- NULL

  # saves the SPECIES name to a variable
  species = SPECIES

  # filters out the df Data Set to only include fish that are a specific SPECIES
  filteredDF <- df %>% filter({with(df, SPECIES) == species})

  # creates the file name to save the filtered data to
  location <- paste("LvsWfor", species,".csv",sep='')

  # writes the filtered data to a CSV file
  write.csv(filteredDF, location, row.names=FALSE)

  # creates a relative frequency table, in relation to RIVER
  relFreqRiver = round(table(with(df, RIVER))/ length(with(df, RIVER)), 6)

  # creates a list
  # Contains the data before filtering, the data after filtering, and the
  #   relative frequency table for RIVER.
  dataset <- list(beforeSubset = df, afterSubset = filteredDF, riverFreq = relFreqRiver)

  # prints the list (from earlier) to command line
  print(dataset)

  # creates ggplot from filtered Data Set, with LENGTH vs WEIGHT
  g <- ggplot(filteredDF, aes(x=LENGTH, y=WEIGHT))

  # makes it into a scatter plot
  g <- g + geom_point(aes(color=RIVER))

  # places quadratic curve onto plot
  g <- g + geom_smooth(formula = y~x +I(x^2), method = "lm")

  # insert name to plot
  g <- g + ggtitle("ANTHONY NGUYEN")

  # display plot
  g
}
