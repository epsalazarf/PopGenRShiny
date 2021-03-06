# 1000 Genomes Project Sampler (Shiny)
# Shiny: app.R
# Author: Pavel Salazar-Fernandez
# Developed at: LANGEBIO - Mexico

# Pipeline:
# 1. Reads the popinfo file for the 1000 Genomes Project samples.
# 2. Identifies categories for easier subsetting.
# 3. Generates a table with random samples.
# 
# Features:
# - Choose number of samples
# - Subset regions/populations
# - Preview the table
# - Save table as a popinfo file and a list of IDs for plink command --keep

#setwd(dirname(parent.frame(1)$ofile))
pi <- read.csv("1KGP3.popinfo.txt", sep="\t")
pop <- as.character(unique(pi$POP))
regs <- as.character(unique(pi$SPOP))


ui <- fluidPage(
  img(src="MorLabLogo.jpg", style = "float:right"),
  titlePanel("1000 Genomes Project Sampler"),
  helpText("Author: Pavel Salazar-Fernandez | Developed at: LANGEBIO (MX)"),
  hr(), 
  sidebarLayout(
    sidebarPanel(width=2,
                 numericInput("ns", label = "Samples per region/population:",
                              value= 10, min= 1, max=9999),
                 h5("Total samples selected:"),
                 textOutput("totsam"),
                 br(),
                 selectizeInput("spops", label = "Regions selected:", 
                                choices= regs, selected = NULL,
                                options= list(maxItems = length(regs)-1,
                                              placeholder = 'Select population(s)',
                                              onInitialize = I('function() { this.setValue(""); }'))),
                 selectizeInput("pops", label = "Populations selected:", 
                                choices= pop, selected = NULL,
                                options= list(maxItems = length(pop)-1,
                                              placeholder = 'Select population(s)',
                                              onInitialize = I('function() { this.setValue(""); }'))),
                 actionButton("goButton", "Save Tables"),
                 textOutput("value")
    ),
    mainPanel(
      tableOutput("SampleTable")
    )
  )
)

server <- function(input, output) {
  dataset <-  reactive({
    popsel <- c()
    if(length(input$spops) > 0){
      for (i in input$spops){popsel <- c(popsel,sort(sample(which(pi$SPOP==i),input$ns)))}
    } else if(length(input$pops) > 0){
      for (i in input$pops){popsel <- c(popsel,sort(sample(which(pi$POP==i),input$ns)))}
    } else {
      for (i in pop){popsel <- c(popsel,sort(sample(which(pi$POP==i),input$ns)))}
    }
    pi[popsel,]
  })
  
  output$totsam <- reactive(dim(dataset())[1])
  
  output$SampleTable <- renderTable({dataset()})
  
  output$value <- renderText(paste(Export()))

  Export <- eventReactive(input$goButton,{
      setwd("output")
      write.table(dataset(),file="1KG.sample.popinfo.txt", row.names=F, quote=F,sep="\t",eol="\n")
      write.table(dataset()[,c("FAMID","ID")],file="1KG.sample.ids.txt",quote=F,sep="\t",row.names = F,col.names = F,eol="\n")
      setwd("..")
      return("Success: Tables saved to /output")
  })
  
}

shinyApp(ui = ui, server = server)