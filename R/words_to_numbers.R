#' words_to_numbers
#'
#'
#' words_to_numbers takes a character string and replaces numbers
#' written in words with numerics.
#'
#'
#' @param string a character string
#'
#' @return Returns a string with any detected numbers written as words replaced by numbers
#'
#' @examples
#' library(wordstonumbers)
#' words_to_numbers("ninety-nine red balloons")
#'
#' words_to_numbers("twelve")
#'
#' words_to_numbers("Forty-three thousand")
#'
#' words_to_numbers("There were one thousand, two hundred and thirty. Twelve.")
#'
#' example <-
#' "The PRQ is a twelve-item, four-point Likert scale
#' (from one = Never to four = Very Often) with three sub-scores
#' bullying (PRQ-Bully), being victimized (PRQgroup-Victim),
#' and pro-social behavior (PRQ-Prosocial). A translated,
#' backtranslated final Arabic version of the scale was found
#' to be accurate showing good internal consistency in this
#' sample [PRQ-Victim (alpha = .seventy four)
#' and PRQ-Bullies (alpha = seventy-four)]."
#'
#' words_to_numbers(example)
#'
#' @importFrom rlang .data
#'
#' @export
words_to_numbers <- function(string) {

  if(length(unlist(string)) > 1 | class(string) != "character") {
    warning("The argument which was passed to words_to_numbers is not a length 1 character element, only the first element has been used here. Consider using the apply or purrr::map functions to assess multiple elements at once.")
  string <- unlist(string)[[1]]
  }

  ### Setting up constants
  UNITS <- list(
    zero = 0,
    one = 1,
    two = 2,
    thirteen = 13,
    three = 3,
    fourteen = 14,
    four = 4,
    fifteen = 15,
    five = 5,
    sixteen = 16,
    six = 6,
    seventeen = 17,
    seven = 7,
    eighteen = 18,
    eight = 8,
    nineteen = 19,
    nine = 9,
    ten = 10,
    eleven = 11,
    twelve = 12
  )

  TENS <- list(
    ten = 10,
    twenty = 20,
    thirty = 30,
    forty = 40,
    fifty = 50,
    sixty = 60,
    seventy = 70,
    eighty = 80,
    ninety = 90
  )

  MAGNITUDES <- list(
    hundred = 100,
    thousand = 1000,
    million = 1000000,
    billion = 1000000000,
    trillion = 1000000000000,
    quadrillion = 1000000000000000,
    quintillion = 1000000000000000000,
    sextillion = 1000000000000000000000,
    septillion = 1000000000000000000000000,
    octillion = 1000000000000000000000000000,
    nonillion = 1000000000000000000000000000000,
    decillion = 1000000000000000000000000000000000
  )

  NUMBER <- c(UNITS, TENS, MAGNITUDES)

  UNIT_NAMES <- names(UNITS)
  TEN_NAMES <- names(TENS)
  MAGNITUDE_NAMES <- names(MAGNITUDES)

  NUMBER_NAMES <- c(UNIT_NAMES, TEN_NAMES, MAGNITUDE_NAMES)

  # Splitting in to tokens at punctuation
  stringSplitVec <-
    stringr::str_split(string,
                       "(?<=[[[:punct:]]|\\s])|(?=[[[:punct:]]|\\s])",
                       simplify = T)

  # create binaries for whitespace or punctuation
  punctuationBinary <-
    stringr::str_detect(stringSplitVec, "[[:punct:]]|\\s|(^and$)")

  # Ensuring that "." breaks numbers apart by counting it as non-punctuation
  punctuationBinary[stringr::str_detect(stringSplitVec, "\\.")] <-
    FALSE

  ### Detecting numbers
  # Detecting numerics
  numericBinary <-
    !is.na(suppressWarnings(as.numeric(stringSplitVec)))

  # Detecting numbers - i.e., any words that match any of the NUMBER_WORDS
  numberBinary <-
    stringr::str_detect(stringSplitVec, stringr::regex(
      paste("^", NUMBER_NAMES, "$", collapse = "|", sep = ""),
      ignore_case = T
    ))

  # Quick exit if there are no numbers words detected
  if (sum(numberBinary[!numericBinary]) < 1) {
    return(string)
  }
  # And quick exit if there is only one number word pressent
  if (length(stringSplitVec) == 1) {
    return(as.character(format(NUMBER[[tolower(string)]], scientific = F)))
  }

  # Making tibble
  stringSplit <-
    tibble::tibble(
      id = 1:length(numberBinary),
      stringSplit = as.character(stringSplitVec),
      punctuationBinary,
      # Note that this counts numbers as words or numbers as digits as numbers
      numberBinary = numericBinary | numberBinary,
      numericBinary
    )

  stringSplit$group <- NA

  # Using cumulative sum to count the number of non-number items, not counting punctuation
  stringSplit$group[!stringSplit$punctuationBinary] <-
    cumsum(!stringSplit$numberBinary[!stringSplit$punctuationBinary])

  # Removing dots and other words from contention to ensure that groups get broken at points
  stringSplit$group[stringr::str_detect(stringSplit$stringSplit, "\\.")] <-
    -1
  stringSplit$group  <-
    ifelse((stringSplit$numberBinary |
              stringSplit$punctuationBinary),
           stringSplit$group,
           -1
    )

  # Filling in all NAs between two other identical values -
  # this means that each set of number words is grouped together
  stringSplit$group <-
    ifelse(
      tidyr::fill(stringSplit, "group",  .direction = "down")$group == tidyr::fill(stringSplit, "group",  .direction =  "up")$group,
      tidyr::fill(stringSplit, "group",  .direction = "down")$group,
      NA
    )

  # Checking for [number] [point] [number] and converting it into "number.number"
  last_position <- nrow(stringSplit)

  stringSplit$point <- stringr::str_detect(stringSplit$stringSplit,
                                           stringr::regex("point|dot",
                                                          ignore_case = T))
  stringSplit$tokenAheadPoint <- c(stringSplit$point[-1], F)
  stringSplit$tokenBehindPoint <-
    c(F, stringSplit$point[-last_position])

  stringSplit$space <- stringSplit$stringSplit == " "
  stringSplit$tokenAheadSpace <- c(stringSplit$space[-1], F)
  stringSplit$tokenBehindSpace <-
    c(F, stringSplit$space[-last_position])

  stringSplit$tokenAheadNumber <- c(stringSplit$numberBinary[-1], F)
  stringSplit$tokenBehindNumber <-
    c(F, stringSplit$numberBinary[-last_position])

  stringSplit$stringSplit <-
    ifelse(c(
      # If the token is a space AND
      stringSplit$space &
        # ... the token following the space is a point and before is a number, and the point is followed by anything, and then a number
        (
          stringSplit$tokenAheadPoint &
            stringSplit$tokenBehindNumber  &
            c(stringSplit$tokenAheadNumber[-(1:2)], F, F)
        ) |
        # ... the token before the value is a space AND the token ahead is a point, which is in turn followed by a number (three ahead))))
        (
          stringSplit$tokenAheadNumber & stringSplit$tokenBehindPoint
        )
    )
    ,
    "", stringSplit$stringSplit)

  # Replace  "dot" or "point" with points (as eg "ten point five")
  stringSplit$stringSplit <-
    ifelse(
      c(T, T, stringSplit$numberBinary[-((last_position - 1):last_position)]) &
        c(stringSplit$numberBinary[-c(1, 2)], F, F) &
        stringSplit$point,
      ".",
      stringSplit$stringSplit
    )

  # initiallising number vector
  stringSplit$number <-
    ifelse(stringSplit$numericBinary,
           yes = suppressWarnings(as.numeric(stringSplit$stringSplit)),
           no = NA)

  # Filtering down to just those groups that have numerics
  numericStrings <- dplyr::filter(stringSplit, .data$group > -1)

  # Identifying the types of each number
  numericStrings$magnitudeType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", MAGNITUDE_NAMES, "$", collapse = "|"),
                                       ignore_case = T)) |
    ifelse(
      !is.na(numericStrings$number),
      as.numeric(numericStrings$number) %in% MAGNITUDES,
      F
    )

  numericStrings$tenType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", TEN_NAMES, "$", collapse = "|"),
                                       ignore_case = T)) |
    ifelse(!is.na(numericStrings$number),
           (
             as.numeric(numericStrings$number) == 10 |
               (as.numeric(numericStrings$number) %in% TENS)
           )
           , F)

  numericStrings$unitType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", UNIT_NAMES, "$", collapse = "|"),
                                       ignore_case = T)) |
    ifelse(
      !is.na(numericStrings$number),
      # Counting as a unit if the token is a digit between 0 - 99 but not a 10
      as.numeric(numericStrings$number) %in% seq(0, 99)[-c(seq(11, 91, by = 10))],
      F
    )

  # Helper function to check whether the token is a number or matches a number and returns either the numeric or the string
  token_to_number <- function(tokens) {
    tokens <- tolower(tokens)
    unlist(purrr::map(tokens,
                      function(token) {
                        if (is.null(NUMBER[token][[1]])) {
                          return(as.numeric(token))
                        } else {
                          NUMBER[token]
                        }
                      }))
  }

  # Breaking apart groups according to the following rules
  for (groups in unique(numericStrings$group)) {
    # Extracting numbers only
    numericsOnly <-
      dplyr::filter(numericStrings,
                    numericStrings$numberBinary,
                    numericStrings$group ==
                      groups)
    if (nrow(numericsOnly) < 2) {
      # If there is only one element it doesn't need to change
      numericsOnly$tochange <- FALSE
      # Else if there are multiple, we need to check that there are not any illegal characters
    } else if (nrow(numericsOnly) > 1) {
      #L <- 1:nrow(numericsOnly)
      pairs_to_test <-
        tibble::tibble(e1 = 1:(nrow(numericsOnly) - 1),
                       e2 = 2:nrow(numericsOnly))
      # Figuring out break points, i.e., splitting where there are (note that
      # this takes e2 as the position to split at, i.e., e1 is the preceeding
      # value and e2 is the one which is broken at)
      numericsOnly$tochange  <- c(
        FALSE,
        # Breaking to new number if  a unit type (1-19) was preceeded by a unit type (1-19)
        (numericsOnly$unitType[pairs_to_test$e1] &
           numericsOnly$unitType[pairs_to_test$e2]) |
          # Also breaking if a ten type (10,20,30, ..., 90) of was preceeded by a unit type
          (numericsOnly$unitType[pairs_to_test$e1] &
             numericsOnly$tenType[pairs_to_test$e2]) |
          # And breatking if a a ten type is preceeded by a ten type
          (numericsOnly$tenType[pairs_to_test$e1] &
             numericsOnly$tenType[pairs_to_test$e2]) |
          # And breatking if a a ten type is preceeded by a unit type above 10
          (numericsOnly$tenType[pairs_to_test$e1] &
             numericsOnly$unitType[pairs_to_test$e2]) &
          token_to_number(numericsOnly$stringSplit[pairs_to_test$e2]) > 10 |
          # Adding break if a larger number in digits is followed by a word in
          # numbers of a smaller magnitude (e.g., "1000 hundred" or "100 ten")
          (
            numericsOnly$numericBinary[pairs_to_test$e1] &
              token_to_number(numericsOnly$stringSplit[pairs_to_test$e2]) <=
              token_to_number(numericsOnly$stringSplit[pairs_to_test$e1])
          ) |
          # Breaking if two digits are consequtive (e.g., "20, 1" vs "21")
          (
            numericsOnly$numericBinary[pairs_to_test$e1] &
              numericsOnly$numericBinary[pairs_to_test$e2]
          ) |
          # And breaking if a number is preceeded by itself
          (tolower(numericsOnly$stringSplit[pairs_to_test$e1]) ==
             (
               tolower(numericsOnly$stringSplit[pairs_to_test$e2])
             )) |
          # Breaking if a magnitude is preceeded by a magnitude of a larger magnitude
          token_to_number(numericsOnly$stringSplit[pairs_to_test$e1]) > 100 &
          token_to_number(numericsOnly$stringSplit[pairs_to_test$e1]) >
          token_to_number(numericsOnly$stringSplit[pairs_to_test$e2]) &
          numericsOnly$magnitudeType[pairs_to_test$e1] &
          numericsOnly$magnitudeType[pairs_to_test$e2]
      )
    }

    # This part checks triplets
    if (nrow(numericsOnly) > 3) {
      triplets_to_test <-
        tibble::tibble(
          e1 = 1:(nrow(numericsOnly) - 2),
          e2 = 2:(nrow(numericsOnly) - 1),
          e3 = 3:nrow(numericsOnly)
        )
      numericsOnly$tochange <- c(
        # Note that unlike the doubles, this "centers" on the middle value
        # (i.e., e2 is still the value at which the break happens, not the last value)
        F,
        #  This breaks if:
        # a mangnitude is followed by a magnitude, and the latter magnitude is larger than the first
        # (e.g., "twenty thousand, one million" as compared to "one million, twenty thousand")
        # unless the lower number is a hundred in which case we let it slide (because, for example one hundred twenty thousand makes sense)

        # This carves out some exceptions for when we have e.g., Hundreds of thousands
        # When the number before the hundred is below ten
        c(
          T,
          !(
            numericsOnly$unitType[triplets_to_test$e1 - 1] &
              token_to_number(numericsOnly$stringSplit[triplets_to_test$e1 -
                                                         1]) < 10 &
              token_to_number(numericsOnly$stringSplit[triplets_to_test$e1[-1]]) == 100 &
              token_to_number(numericsOnly$stringSplit[triplets_to_test$e3[-1]]) > 100
          )
        ) &
          (
            token_to_number(numericsOnly$stringSplit[triplets_to_test$e1]) <
              token_to_number(numericsOnly$stringSplit[triplets_to_test$e3]) &
              numericsOnly$magnitudeType[triplets_to_test$e1] &
              numericsOnly$magnitudeType[triplets_to_test$e3]
          ),
        F
      ) | numericsOnly$tochange
    }
    # Updating numeric strings withupdated groups
    numericsOnly$group <-
      stringr::str_c("a", numericsOnly$group, cumsum(numericsOnly$tochange))
    numericStrings[match(numericsOnly$id, numericStrings$id), ] <-
      dplyr::select(numericsOnly,-"tochange")
  }

  # Dropping unchanging tokens (i.e., those that are not required, like punctuation that should not be altered)
  numericStrings <- dplyr::filter(numericStrings,!is.na(groups))

  # Reassigning the now ungrouped non-numerics
  numericStrings$group[!stringr::str_detect(numericStrings$group, "^a\\d")] <- NA
  numericStrings$group <-
    ifelse(
      tidyr::fill(numericStrings, .data$group,  .direction = "down")$group == tidyr::fill(numericStrings, .data$group,  .direction =  "up")$group,
      tidyr::fill(numericStrings, .data$group,  .direction = "down")$group,
      NA
    )
  # Helper function for assessing each group of numbers
  identifyNumbers <- function(processedNumerics) {
    # Extracting numbers only
    numericsOnly <- dplyr::filter(processedNumerics, numberBinary)
    # Creating numbers columns
    numericsOnly$number[is.na(numericsOnly$number)] <-
      as.numeric(NUMBER[match(tolower(numericsOnly$stringSplit[is.na(numericsOnly$number)]), NUMBER_NAMES)])
    # Copying for tracking of original numbers, which impact the way that things are summed up
    numericsOnly$oldNumber <- numericsOnly$number
    # For all magnitiude types, count all smaller magnitude types as multipliers of the magnitude value
    if (sum(numericsOnly$magnitudeType) > 0) {
      for (position in which(numericsOnly$magnitudeType)) {
        startCountingFrom <-
          ifelse(any(numericsOnly$oldNumber[1:(position - 1)][numericsOnly$magnitudeType[1:(position -
                                                                                              1)]] > numericsOnly$oldNumber[position]),
                 max(
                   which(numericsOnly$oldNumber[1:(position - 1)] > numericsOnly$oldNumber[position]) + 1
                 ),
                 1)

        if (position > 1) {
          # if the position is not the first value, then take the sum of all previous values
          previousSum <-
            sum(numericsOnly$number[startCountingFrom:(position - 1)]) # begining or previous highest magnitude
          # And then take the product of the previous values and the current value
          value <- previousSum * numericsOnly$number[position]
          # Setting the current value equal to that
          numericsOnly$number[position] <- value
          # And setting the now summed up values to be equal to 0
          numericsOnly$number[startCountingFrom:(position - 1)] <- 0
        }
      }
    }
    # Finally, the number is equal to the sum of all of the constitute numbers
    return(format(sum(numericsOnly$number), scientific = F))
  }

  numericedOutput <- stringSplit

  # for each group of numbers
  for (groups in unique(stats::na.omit(numericStrings$group))) {
    ids <- dplyr::filter(numericStrings, .data$group == groups)$id
    # Blanking out the non-used numbers and repacing strings with numbers
    numericedOutput$number[numericedOutput$id %in% ids][1] <-
      identifyNumbers(numericStrings[numericStrings$group == groups, ])
    # Replacing strings with the appropraite values, after removing all old text
    numericedOutput$stringSplit[numericedOutput$id %in% ids] <- ""
    numericedOutput$stringSplit[numericedOutput$id %in% ids][1] <-
      numericedOutput$number[numericedOutput$id %in% ids][1]
  }
  return(paste0(numericedOutput$stringSplit, collapse = ""))
}
