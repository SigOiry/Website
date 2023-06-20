ui <- fluidPage(
    actionButton("rmv", "Remove UI"),
    h1("This is no longer useful", id="txt"),
    
    h1("This is no longer usefulzdfq", id="txt1"),
    
    actionButton("rmv", "Remove UI"),
)

server <- function(input, output, session) {
    observeEvent(input$rmv, {
        removeUI(
            selector = "div:has(> #txt)"
        )
    })
}

shinyApp(ui, server)