Enigma <- setClass("Enigma",
                   slots = c(
                     rotor_types = "character", # I, II, III etc. (order matters!),
                     rotors = "list",
                     start_positions = "character",
                     ring_settings = "character",
                     reflector = "character", # simple character string
                     plugboard = "character" # named character string of variable length
                   ),
                   prototype=list(
                     rotor_types = c("III", "II", "I"),
                     start_positions = c("A", "A", "A"),
                     ring_settings = c("A", "A", "A"),
                     plugboard = c()
                   )
)

#' Initialize an M3 Enigma machine
#'
#' Available rotors: I-VIII and reflectors B and C. 
#' @param rotors Rotors used, from left to right. (I - VIII)
#' @param start_positions Start positions (e.g. c("A", "A", "A")).
#' @param ring_settings Ring settings (e.g. c("D", "E", "F")).
#' @param reflector Which reflector is used ("B" or "C").
#' @param plugboard Plugboard settings. Should be a named list.
#' @export Enigma
#' @examples
#' # Initialize an enigma machine
#' enigma <- Enigma(rotors = c("II", "I", "III"), 
#'                  start_positions = c("D", "E", "F"),
#'                  ring_settings = c("F", "A", "R"),
#'                  reflector = "B",
#'                  plugboard = c(
#'                      A = "D",
#'                      C = "Y",
#'                      F = "Q",
#'                      G = "R",
#'                      I = "L",
#'                      W = "M",
#'                      Z = "U"
#'                    )
#'                 )
#' # Encrypt a few words:
#' encrypt(enigma, "helloworld")               
#' 
Enigma <- function(rotors = c("III", "II", "I"),
                   start_positions = c("A", "A", "A"),
                   ring_settings = c("A", "A", "A"),
                   reflector = "B",
                   plugboard = c()) {
  
  new("Enigma", rotor_types = rotors, start_positions = start_positions,
      ring_settings = ring_settings, reflector = reflector, plugboard = plugboard)
}



# Initialize Enigma
setMethod (
  f = "initialize",
  signature = "Enigma",
  definition = function(.Object, rotor_types, rotors, start_positions, ring_settings, reflector, plugboard){
    # Convert start_positions and ring_settings to uppercase 
    start_positions <- toupper(start_positions)
    ring_settings <- toupper(ring_settings)
    
    # Some initial checks
    if(any(!rotor_types %in% names(available_rotors))) stop("Unknown rotor(s) specified")
    if(any(!start_positions %in% LETTERS)) stop("Invalid start positions")
    if(any(!ring_settings %in% LETTERS)) stop("Invalid ring settings")
    if(!reflector %in% c("B", "C")) stop("Invalid reflector")
    
    # Set rotors in position
    rotors <- list(available_rotors[[rotor_types[3]]], available_rotors[[rotor_types[2]]],available_rotors[[rotor_types[1]]])
    for(i in 1:length(rotors)) rotors[[i]] <- set_position(rotors[[i]], rev(start_positions)[i]) 
    for(i in 1:length(rotors)) rotors[[i]]@ring_settings <- rev(ring_settings)[i]

    # Initialize reflector
    reflector <- available_reflectors[[reflector]]
    reflector <- stringr::str_split(reflector, pattern = "")[[1]]
    names(reflector) <- LETTERS
    
    # Initialize plugboard
    if(length(plugboard) > 0) {
      plugboard <- tibble::tibble(key = names(plugboard), value = plugboard)
      plugboard <- plugboard[!duplicated(t(apply(plugboard, 1, sort))),] # Remove doubles
      if(dplyr::n_distinct(c(plugboard$key, plugboard$value)) < length(c(plugboard$key, plugboard$value))) {
        stop("Each letter can be set only once in the plugboard")
      }
      plugboard_names <- c(plugboard$value, plugboard$key)
      plugboard <- c(plugboard$key, plugboard$value)
      names(plugboard) <- plugboard_names
      
    } else {
      plugboard <- ""
    }
    
    
    # Set slots
    .Object@rotor_types <- rotor_types
    .Object@rotors <- rotors
    .Object@start_positions <- start_positions
    .Object@ring_settings <- ring_settings
    .Object@reflector <- reflector
    .Object@plugboard <- plugboard
    validObject(.Object)
    return(.Object)
  })

#' Encrypt a string using an Enigma instance
#' 
#' @param Enigma An object of class 'Enigma'
#' @param string Character string to encrypt.
#' @rdname encrypt
#' @export 
#' 
setGeneric("encrypt", function(Enigma, string) standardGeneric("encrypt"))

#' @rdname encrypt
setMethod (
  f = "encrypt",
  signature = "Enigma",
  definition = function(Enigma, string){
    string <- stringr::str_split(string, pattern = "")[[1]]
    # Convert to uppercase 
    string <- toupper(string)
    
    # Check if string contains non-letters
    if(any(!string %in% LETTERS)) stop("String contains non-letters")
    string_encrypted <- vector(mode = "character", length = length(string))
    j <- 1
    for(char in string) {
      
      # Plugboard -> up to three pairs
      char <- if(char %in% names(Enigma@plugboard)) Enigma@plugboard[char] else char
      
      # Rotations 
      if(at_turnover_position(Enigma@rotors[[2]])) {
        for(i in 1:length(Enigma@rotors)) {
          Enigma@rotors[[i]] <- rotate(Enigma@rotors[[i]])
        } 
      } else if(at_turnover_position(Enigma@rotors[[1]])) {
        for(i in c(1,2)) {
          Enigma@rotors[[i]] <- rotate(Enigma@rotors[[i]])
        }
      } else {
        Enigma@rotors[[1]] <- rotate(Enigma@rotors[[1]])
      }
      
      # Encrypt
      for(i in 1:3) {
        char <- encrypt_letter_forward(Enigma@rotors[[i]], letter = char)
      }
      
      # Reflect
      char <- Enigma@reflector[char]
      
      # Encrypt backwards
      for(i in 3:1) {
        char <- encrypt_letter_backward(Enigma@rotors[[i]], letter = char)
      }
      
      # Plugboard
      char <- if(char %in% names(Enigma@plugboard)) Enigma@plugboard[char] else char
      string_encrypted[j] <- char
      j <- j + 1
    }
    stringr::str_c(string_encrypted, collapse = "")
  })

# Show method for Enigma
#' @describeIn Enigma show settings of the Enigma machine
#' @export 
setMethod("show", "Enigma", function(object) {
  cat(is(object)[[1]], "\n",
      "Type: M3\n",
      "Rotors:", object@rotor_types[1], object@rotor_types[2], object@rotor_types[3], "\n",
      "Reflector: ", object@reflector, "\n",
      "Plugboard:",
      sep = " "
  )
  if(length(object@plugboard) > 0) {
    cat("\n")
    for(i in 1:length(object@plugboard)) cat("\t", names(object@plugboard)[i], " : ", object@plugboard[i], "\n", sep = "")
  } else {
    "Not Used"
  }
})

#' An S4 class to represent a rotor in the Enigma machine.
#'
#' @slot position Position of the rotor (0-25).
#' @slot wiring Wiring of the rotor (relative to the alphabet).
#' @slot turnover_position Turnover notch position (A-Z).
#' @slot ring_settings Ring setting.

Rotor <- setClass("Rotor", 
                  # Slots
                  slots = list(position = "numeric", 
                               wiring = "character",
                               turnover_position = "character",
                               ring_settings = "character"),
                  # Defaults: position = 1
                  prototype = list(
                    position = 0,
                    wiring = stringr::str_split("EKMFLGDQVZNTOWYHXUSPAIBRCJ", pattern = "")[[1]],
                    turnover_position = "Q",
                    ring_settings = "A"
                  ),
                  validity=function(object)
                  {
                    if((object@position < 0)) {
                      return("A negative number for the position was given")
                    }
                    if((!object@ring_settings %in% LETTERS)) {
                      return("Ringstellung should be a (uppercase) letter")
                    }
                    if(!all(object@turnover_position %in% LETTERS)) {
                      return("Turnover position should be a (uppercase) letter")
                    }
                    return(TRUE)
                  }
)

## Methods 

# create a method to assign the value of a coordinate
setGeneric("rotate", function(Rotor) standardGeneric("rotate"))

setMethod(f = "rotate",
          signature = "Rotor",
          definition = function(Rotor)
          {
            Rotor@position <- Rotor@position + 1
            return(Rotor)
          }
)

setGeneric("encrypt_letter_forward", function(Rotor, letter) standardGeneric("encrypt_letter_forward"))

setMethod(f = "encrypt_letter_forward",
          signature="Rotor",
          definition=function(Rotor, letter)
          {
            position <- (Rotor@position + 1 - which(LETTERS == Rotor@ring_settings)) %% 26
            encrypted_letter <- Rotor@wiring[(which(LETTERS == letter) - 1 + position) %% 26 + 1]
            encrypted_letter <- LETTERS[(which(LETTERS == encrypted_letter) - position - 1) %% 26 + 1]
            return(encrypted_letter)
          }
)

setGeneric("encrypt_letter_backward", function(Rotor, letter) standardGeneric("encrypt_letter_backward"))

setMethod(f = "encrypt_letter_backward",
          signature="Rotor",
          definition=function(Rotor, letter)
          {
  
            position <- (Rotor@position + 1 - which(LETTERS == Rotor@ring_settings)) %% 26
            encrypted_letter <- LETTERS[(which(LETTERS == letter) + position - 1) %% 26 + 1]
              encrypted_letter <- LETTERS[(which(Rotor@wiring == encrypted_letter) - 1 - position) %% 26 + 1]
            return(encrypted_letter)
            
          }
)

setGeneric("at_turnover_position", function(Rotor) standardGeneric("at_turnover_position"))


setMethod(f = "at_turnover_position",
          signature="Rotor",
          definition=function(Rotor)
          {
            LETTERS[(Rotor@position %% 26) + 1] %in% Rotor@turnover_position
          }
)

setGeneric("set_position", function(Rotor, start_positions) standardGeneric("set_position"))

setMethod(f = "set_position",
          signature="Rotor",
          definition=function(Rotor, start_positions)
          {
            Rotor@position <- which(LETTERS == start_positions) - 1
            return(Rotor)
          }
)

## Rotors
available_rotors <- list(
  "I" = Rotor(position = 0, wiring = stringr::str_split("EKMFLGDQVZNTOWYHXUSPAIBRCJ", pattern = "")[[1]], 
              turnover_position = "Q"),
  "II" = Rotor(position = 0, wiring = stringr::str_split("AJDKSIRUXBLHWTMCQGZNPYFVOE", pattern = "")[[1]], 
               turnover_position = "E"),
  "III" = Rotor(position = 0, wiring = stringr::str_split("BDFHJLCPRTXVZNYEIWGAKMUSQO", pattern = "")[[1]], 
                turnover_position = "V"),
  "IV" = Rotor(position = 0, wiring = stringr::str_split("ESOVPZJAYQUIRHXLNFTGKDCMWB", pattern = "")[[1]], 
               turnover_position = "J"),
  "V" = Rotor(position = 0, wiring = stringr::str_split("VZBRGITYUPSDNHLXAWMJQOFECK", pattern = "")[[1]], 
              turnover_position = "Z"),
  "VI" = Rotor(position = 0, wiring = stringr::str_split("JPGVOUMFYQBENHZRDKASXLICTW", pattern = "")[[1]], 
              turnover_position = c("Z", "M")),
  "VII" = Rotor(position = 0, wiring = stringr::str_split("NZJHGRCXMYSWBOUFAIVLPEKQDT", pattern = "")[[1]], 
              turnover_position = c("Z", "M")),
  "VIII" = Rotor(position = 0, wiring = stringr::str_split("FKQHTLXOCBJSPDZRAMEWNIUYGV", pattern = "")[[1]], 
                turnover_position = c("Z", "M"))
)

## Reflectors
available_reflectors <- list(
  "B" = "YRUHQSLDPXNGOKMIEBFZCWVJAT",
  "C" = "FVPJIAOYEDRZXWGCTKUQSBNMHL"
)