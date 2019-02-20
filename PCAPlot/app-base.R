# AUTO PCA PLOTTER (Shiny)
# Shiny: app.R
# Author: Pavel Salazar-Fernandez
# Developed at: LANGEBIO - Mexico
# Last Edit: August 11 2016

# Requirements:
# - EVAL and EVEC files from the PCA program (e.g. *smartpca*). Only one per directory.
# - popinfo file


# Pipeline:
# 1. Reads first .evec and .eval files from a chosen directory.
# 2. Identifies names, regions and populations from a given popinfo.txt
# 3. Generates a PCA plot with color-coded regions.

# Features:
# - Plot types: Can select between points or tags for the plot.
# - Select Population: Shows only a particular population.
# - Emphasize Population: Highlights points or tags for a chosen population.
# - Color by Category: User can choose the criteria for coloring.
# - Show/Hide Legend: Displays all values from the selected category.
# - (NEW) Interactive Zoom: select an area and double click to zoom in, double click again to zoom out.

#<START>
# Load required libraries 
library(shiny)

#<INPUT>
# Choose data directory
setwd(choose.dir(default = "", caption = "Select folder with PCA files"))
#</INPUT>

#<PREPARATIONS>
# Read popinfo file (.txt):
pifile <- dir(pattern = "\\popinfo.txt$")[1]
popinfo <- read.csv(pifile,sep="",header=TRUE)
poptags <- as.character(unique(popinfo$POP))
uniquecols <- sapply(popinfo,function(x) length(unique(x)))
fields <- names(uniquecols[uniquecols>1])
if ("POP_SIMPLE" %in% colnames(popinfo)) {
  pops <- unique(subset(popinfo, select = c(POP,POP_SIMPLE)))
  pops <- gsub("_"," ",(paste(pops$POP," (",pops$POP_SIMPLE,")",sep="")))
} else {
  pops <- as.character(unique(popinfo$POP))
}
deftitle <- unlist(strsplit(pifile,"[.]"))[1]

# Load PCA files
evec.in <- dir(pattern = "\\.evec$")[1]
eval.in <- dir(pattern = "\\.eval$")[1]
data <- read.table(evec.in)
eval <- read.table(eval.in)

ncomps <- table(sapply(data, class))["numeric"]
PC1Col <- (match("numeric",sapply(data, class)))
IDcol <- PC1Col - 1
row.names(data)[IDcol] <- "ID"
popIDs <- gsub(".*:","",data[,IDcol])
nsamples <- length(popIDs)
idx <- match(popIDs, popinfo$ID)


#</PREPARATIONS>

#<UI>
ui <- fluidPage(    
  # Page Title
  img(src="MorLabLogo.jpg", style = "float:right"),
  titlePanel("Auto PCA Plotter"),
  helpText("Author: Pavel Salazar-Fernandez | Developed at: LANGEBIO (MX)"),
  hr(),    
  # Sidebar
  sidebarLayout(      
    
    # Input Panels
    sidebarPanel(width=3,
                 h4("Settings"),
                 textInput("plottitle", label="Title", value = ""),
                 numericInput("PCa", label = "First Component (X)", value = 1,
                              min = 1, max = ncomps, step = 1),
                 numericInput("PCb", label = "Second Component (Y)", value = 2,
                              min = 1, max = ncomps, step = 1),
                 selectizeInput("pops", label = "Populations displayed:", 
                                choices= pops, selected = NULL,
                                options= list(maxItems = length(pops)-1,
                                              placeholder = 'Select population(s)',
                                              onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 selectizeInput("pope", label = "Populations emphasis:", 
                                choices= pops, selected = NULL,
                                options= list(maxItems = 1,
                                              placeholder = 'Choose population',
                                              onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 hr(),
                 selectizeInput("flds", label = "Group Coloring:", 
                                choices= fields, selected = NULL,
                                options= list(maxItems = 1,
                                              placeholder = 'Choose color grouping',
                                              onInitialize = I('function() { this.setValue(""); }'))),
                 radioButtons("type", label = "Type:",
                              choices = list("Points" = 1, "Text" = 2), 
                              selected = 1),
                 checkboxInput("leg", label = "Legend", value = FALSE),
                 hr(),
                 h5("Points Info"),
                 verbatimTextOutput("brshinfo")
    ),
    
    # Plotting Area
    mainPanel(width=9,
              plotOutput("PCAPlot", width = "960px", height= "960px", dblclick = "dclk", brush = brushOpts(id= "brsh", resetOnNew = TRUE))  
    )
  )
)
#</UI>

#<SERVER>
server <- function(input, output) {
  
  #<REACTIVES>
  # Inputs
  sub.pops <- reactive(as.vector(popinfo[popinfo$POP %in% substr(input$pops,1,3),IDcol]))
  pope.idn <- reactive(as.vector(popinfo[popinfo$POP %in% substr(input$pope,1,3),IDcol]))
  grouping <- reactive(if(input$flds != "")as.character(unique(popinfo[,input$flds])) else(""))
  PCaCol <- reactive(PC1Col + input$PCa - 1)
  PCbCol <- reactive(PC1Col + input$PCb - 1)
  pct.varPCa <- reactive(round(1000*eval[PCaCol(),]/sum(eval),2))
  pct.varPCb <- reactive(round(1000*eval[PCbCol(),]/sum(eval),2))
  ranges <- reactiveValues(x = NULL, y = NULL)
  #</REACTIVES>
  
  #<OBSERVERS>
  observeEvent(input$dclk, {
    brush <- input$brsh
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  #</OBSERVERS>
  
  #<PROCESSING>
  hiplot <- reactive({
    sbP.idx <- which(popIDs %in% sub.pops())
    pope.idx <- which(popIDs %in% pope.idn())
    if (input$flds != "") {
      coloring <- c(grouping(),"(Emphasis)")
      groups.color <- c(rainbow(length(coloring)-1),"#000000")
      names(groups.color) <- coloring
      data$Colors <- groups.color[as.character(popinfo[idx,input$flds])]
    } else {
      if("COLOR" %in% colnames(popinfo)){
        data$Colors <- as.character(popinfo[idx,"COLOR"])
        coloring <- "(Emphasis)"
        groups.color <- "#000000"
      } else {
        coloring <- c("Samples","(Emphasis)")
        groups.color <- c("#666666","#000000")
        data$Colors <- "#666666"
      }
    }
    if(length(pope.idx) > 0){coloring[length(coloring)] <- input$pope}
    plot(data[,c(PCaCol(),PCbCol())], col= data$Colors, pch= 20, cex.main = 1.5,
         xlab= paste("PC",input$PCa," (",pct.varPCa(),"%)",sep=""),
         ylab= paste("PC",input$PCb," (",pct.varPCb(),"%)",sep=""), 
         main= ifelse(input$plottitle=="",deftitle,input$plottitle),
         type= ifelse(is.null(input$pops) && input$type == 1,"p","n"), cex=3,
         xlim= ranges$x,
         ylim= ranges$y
    )
    abline(v= 0, h= 0, lty= 2, col= "grey")
    if(input$leg) legend("topright", gsub("_"," ",coloring), ncol= 1, col= groups.color, pch= 20,
                         pt.cex= 3, bty= "n")
    if (input$type==1) {
      points(data[sbP.idx,c(PCaCol(),PCbCol())], cex= 2, pch= 19, col= data$Colors[sbP.idx])
      points(data[pope.idx,c(PCaCol(),PCbCol())], cex= 2.5, pch= 23, bg="#000000", col= "#FFFFFF")
    }
    else {
      if (is.null(input$pops)) text(data[,c(PCaCol(),PCbCol())], labels= popinfo[idx,IDcol], col= data$Colors, cex= 0.9)
      else text(data[sbP.idx,c(PCaCol(),PCbCol())], labels=data[sbP.idx,IDcol], cex= 1, col= data$Colors[sbP.idx])
      if (length(pope.idx) > 0) text(data[pope.idx,c(PCaCol(),PCbCol())], labels= data[pope.idx,IDcol], font= 2, cex=1, col= "#000000")
    }
  })
  #</PROCESSING>
  
  #<OUTPUT>
  output$PCAPlot <- renderPlot(hiplot())
  output$brshinfo <- renderPrint({popinfo[rownames(brushedPoints(data[,],input$brsh, xvar= PCaCol(), yvar= PCbCol())),c("FID","ID","POP","POP_SIMPLE")]})
  #</OUTPUT>
}
#</SERVER>

#<APP>
shinyApp(ui = ui, server = server)
#</APP>

#<END>