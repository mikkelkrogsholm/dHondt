library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("dHondt"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textAreaInput("parties", label = "Partier og valgforbund", value = "Valgforbund1 = (A = 22.7, B = 9.5, F = 8.3, Ø = 22, Å = 10.2)\nValgforbund2 = (C = 5.3, D = .9, I = 3.5, K = 1, O = 5.2, V = 11.1)\nrest = .3", height = '200px'),
        numericInput("seats", label = "Antal pladser", value = 55),
        actionButton("go", "Beregn fordeling")
      ),
      mainPanel(
         plotOutput("mandatplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Define functions
  dHondt <- function(votes, seats){ 
    tmp <- tibble::tibble( 
      candidates = rep(names(votes), each = seats), 
      scores = as.vector(sapply(votes, function(x) x / 1:seats)) 
    ) 
    tmp <- tmp$candidates[order( - tmp$scores )] [1:seats] 
    tmp <- tibble::as_tibble(table(tmp))
    
    res <- tmp$n
    names(res) <- tmp$tmp
    
    res
  } 
  mandatfordeler <- function(forbund, seats){
    liste_sum <- unlist(lapply(forbund, sum))
    liste_mandater <- dHondt(liste_sum, seats)
    
    mandatfordeling <- lapply(1:length(liste_mandater), function(i){
      liste_navn <- names(liste_mandater)[i]
      
      seats <- liste_mandater[liste_navn]
      partier <- forbund[[liste_navn]]
      
      dHondt(partier, seats)
      
    })
    
    my_res <- unlist(mandatfordeling)
    
    rev(sort(my_res))
  }
  parse_string <- function(string){
    my_lines <- readr::read_lines(string)
    my_lines <- stringr::str_replace_all(my_lines, "\\(", "c\\(")
    my_lines[-length(my_lines)] <- stringr::str_replace_all(my_lines[-length(my_lines)], "\\)", "),")
    my_lines <- paste(my_lines, collapse = " ")
    my_lines <- paste0("list(", my_lines, ")")
    eval(parse(text = my_lines))
  }
  
  plotdata <- eventReactive(input$go, {
    res <- parse_string(input$parties)
    mandater <- mandatfordeler(res, input$seats)
    
    tibble::tibble(
      party = names(mandater),
      seats = mandater
    )
  })
  
   
   output$mandatplot <- renderPlot({
     
     ggplot2::ggplot(plotdata(), ggplot2::aes(x = reorder(party, rev(seats)), 
                                              y = seats,
                                              label = seats)) +
       ggplot2::geom_bar(stat = "identity") + 
       ggplot2::geom_label() + 
       ggplot2::labs(x = "", y = "Antal mandater", title = "Mandatfordeling") + 
       ggplot2::theme_minimal()
     

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

