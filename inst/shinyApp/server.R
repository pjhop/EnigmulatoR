function(input, output, session) {
  values <- reactiveValues(A = "", B = "", C = "", D = "",
                           E = "", F = "", G = "", H = "", 
                           I= "", J = "", K = "", L = "", M = "",
                           N = "", O = "", P = "", Q = "", R = "",
                           S = "", T = "", U = "", V = "", W = "",
                           X = "", Y = "", Z = ""
                           )
   # Works! Another (maybe cleaner) option is to reduce the available options in drag-down menus
   lapply(LETTERS, function(letter) {
    observeEvent(input[[paste0("p_", letter)]], {
         # First set values to "" (to prevent error caused by Enigma updating with a plugboard containg double values)
         if(values[[letter]] != "") {
           values[[letter]] <- values[[(names(reactiveValuesToList(values))[reactiveValuesToList(values) == letter])]] <- ""
         }
         
         # Remove old value:
         if(any(reactiveValuesToList(values) != "" & reactiveValuesToList(values) == input[[paste0("p_", letter)]])) {
           values[[names(reactiveValuesToList(values))[reactiveValuesToList(values) == input[[paste0("p_", letter)]] & reactiveValuesToList(values) != "" ]]] <- ""
         }

         # Update values 
         values[[letter]] <- input[[paste0("p_", letter)]]
         if(input[[paste0("p_", letter)]] != "") {
           values[[input[[paste0("p_", letter)]]]] <- letter
         }
       })
  })
  
  lapply(LETTERS, function(letter) {
    observeEvent(values[[letter]],ignoreInit=TRUE, {
      # First remove old value:
      if(input[[paste0("p_",letter)]] != values[[letter]]) {
        updateSelectInput(session, paste0("p_", letter),
                          selected = values[[letter]])
      }
      

    })
  })
  
  # Enigma
  enigma <- reactive({
    Enigma(rotors = c(input$rotor1, input$rotor2, input$rotor3), 
           start_positions = c(input$position1, input$position2,input$position3),
           ring_settings = c(input$setting1, input$setting2, input$setting3),
           reflector = input$reflector,
           plugboard = if(any(reactiveValuesToList(values) != "")) {unlist(reactiveValuesToList(values)[reactiveValuesToList(values) != ""])} else{ c()}
    )
  })

  # Encrypt
  output$textOutput <- renderText({encrypt(enigma(), string = input$textInput)})
}