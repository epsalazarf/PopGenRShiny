# AUTO ADMIXTURE PLOTTER (Shiny)
# Shiny: app.R
# Author: Pavel Salazar-Fernandez
# Developed at: LANGEBIO - Mexico
# Last Edit: March 10 2016

# Requirements:
# - R library: shinyjs
# - Q files from the admixture program (all Q files in the directory are read).
# - popinfo file

# Pipeline:
# 1. Reads popinfo file.
# 2. Reads .Q file of selected K.
# 3. Generates plot.

# Features:
# - Changing the 'Ks' parameter changes the plot showed.
# - Select two or more populations for display.
# - Add borders to individual bars.
# - Collapse sample tags in population tags.
# - Arrange samples by increasing biggest component.
# - Subset by a value from a category.
# - Custom re-coloring of components with visual color picker.

#<START>

# Load required libraries 
library(shiny)
#library(shinyjs)
require(colourpicker)

#<INPUT>
# Choose data folder
setwd(choose.dir(default = "", caption = "Select folder with ADMIXTURE files"))
#</INPUT>

#<PREPARATIONS>
# Read popinfo file (.txt):
pifile <- dir(pattern = "\\popinfo.txt$")[1]
popinfo <- read.csv(pifile,header=TRUE, sep= "\t")
if (ncol(popinfo) == 1) {popinfo <- read.csv(pifile,header=TRUE, sep= " ")}

uniquecols <- sapply(popinfo,function(x) length(unique(x)))
fields <- names(uniquecols[uniquecols>1 & uniquecols < dim(popinfo)[1]/2])
if ("POP_SIMPLE" %in% colnames(popinfo)) {
  pops <- unique(subset(popinfo, select = c(POP,POP_SIMPLE)))
  pops <- gsub("_"," ",(paste(pops$POP," (",pops$POP_SIMPLE,")",sep="")))
} else {
  pops <- as.character(unique(popinfo$POP))
}

barnames <- as.vector(popinfo$ID)
grpnames <- as.vector(popinfo$POP)
popnames <- unique(grpnames)
spaces <- c(0,diff(popinfo$POP))
spaces <- replace(spaces, spaces != 0, 2)

popinfo$NewOrd <- match(popinfo$POP,popnames)
index <- c(1, diff(as.numeric(popinfo$NewOrd)))
index[index !=0] <- 1
borders <- seq(1, length(index))[index==1]
offset <- round(diff(c(borders,length(index) ) )/2)
newnames <- rep("", length(grpnames))
newnames[borders+offset] <- as.character(grpnames[borders+offset])


# List all Q files
kmin <- 99
kmax <- 1
qfiles <- list()
for(f in dir(pattern = "\\.Q$")){
  key <- count.fields(f)[1]
  kmin <- ifelse(key < kmin, key, kmin)
  kmax <- ifelse(key > kmax, key, kmax)
  qfiles[[key]] <- paste(f)
}

#</PREPARATIONS>

ui <- fluidPage(    
  
  # Page Title
  img(src="MorLabLogo.jpg", style = "float:right"),
  titlePanel("Auto ADMIXTURE Plotter"),
  helpText("Author: Pavel Salazar-Fernandez | Developed at: LANGEBIO (MX)"),
  hr(),    
  # Sidebar
  sidebarLayout(      
    
    # Input Panels
    sidebarPanel(width=2,
                 h4("Parameters"),
                 textInput("plottitle", label="Title", value = ""),
                 numericInput("Ks", label = "Number of K", 
                              value = kmin,
                              min=kmin, max=kmax),
                 hr(),
                 selectizeInput("pops", label = "Population", 
                                choices= pops, selected = NULL,
                                options= list(maxItems = length(pops)-1,
                                              placeholder = 'Select population(s)',
                                              onInitialize = I('function() { this.setValue(""); }'))
                                ),
                 selectizeInput("pope", label = "Zoom 5x (All)", 
                                choices= pops, selected = NULL,
                                options= list(maxItems = length(pops)-1,
                                              placeholder = 'Select population(s)',
                                              onInitialize = I('function() { this.setValue(""); }'))
                                ),
                 checkboxInput("brdr", label = "Border", value = FALSE),
                 checkboxInput("ptag", label = "POP Tags", value = TRUE),
                 checkboxInput("sort", label = "Sorted", value = TRUE),
                 br(),
                 h4("Auto-Subset"),
                 selectizeInput("fctr", label = "Factor", 
                                choices= fields, selected = NULL,
                                options= list(maxItems = 1,
                                              placeholder = 'Choose factor',
                                              onInitialize = I('function() { this.setValue(""); }'))),
                 uiOutput("choosegrps"),
                 br(),
                 h4("Coloring"),
                 div(style = "display:inline-block",
                     colourInput("col1", label=  "Color 1:", value = "#FF0000", palette = "limited"),
                     colourInput("col2", label=  "Color 2:", value = "#0000FF", palette = "limited"),
                     colourInput("col3", label=  "Color 3:", value = "#00FF00", palette = "limited"),
                     colourInput("col4", label=  "Color 4:", value = "#FFFF00", palette = "limited"),
                     colourInput("col5", label=  "Color 5:", value = "#00FFFF", palette = "limited"),
                     colourInput("col6", label=  "Color 6:", value = "#FF00FF", palette = "limited")),
                 div(style = "display:inline-block",
                     colourInput("col7", label=  "Color 7:", value = "#FF7F00", palette = "limited"),
                     colourInput("col8", label=  "Color 8:", value = "#1E90FF", palette = "limited"),
                     colourInput("col9", label=  "Color 9:", value = "#008B00", palette = "limited"),
                     colourInput("col10", label=  "Color 10:", value = "#9400D3", palette = "limited"),
                     colourInput("col11", label=  "Color 11:", value = "#8B4500", palette = "limited"),
                     colourInput("col12", label=  "Color 12:", value = "#999999", palette = "limited"))
    ),
    
    # Plotting Area
    mainPanel(width=10,
              plotOutput("AdmixPlot", height = "480px", width= "1280px")  
    )
    
  )
)
#</UI>

#<SERVER>
server <- function(input, output) {
  
  #<REACTIVES>
  # Switches
  all.pops <- reactive(ifelse(is.null(input$pops) && input$grps == "",TRUE,FALSE))
  sub.pops <- reactive(substr(input$pops,1,3))
  pop.emph <- reactive(substr(input$pope,1,3))
  barcolors <- reactive(c(input$col1,input$col2,input$col3,input$col4,
                          input$col5,input$col6,input$col7,input$col8,
                          input$col9,input$col10,input$col11,input$col12))
  #</REACTIVES>
  
  #<OUTPUT>  
  
  #<UI>
  output$choosegrps <- renderUI({
    if(length(input$fctr) == 0) return()
    groups <- unique(as.vector(popinfo[[input$fctr]]))
    selectizeInput("grps", label = "Group", 
                   choices= groups, selected = NULL,
                   options= list(maxItems = 1,
                                 placeholder = 'Choose factor value',
                                 onInitialize = I('function() { this.setValue(""); }')))
  })
  #</UI>  
  
  # Plot Rendering
  output$AdmixPlot <- renderPlot({
    #<PROCESSING>
    # Load Q File
    kData <- read.table(qfiles[[input$Ks]],header=FALSE)
    
    # Data Sorting
    if(input$sort){
      for(p in popnames[1:length(popnames)]){
        pop <- as.numeric(rownames(popinfo[popinfo$POP==p,]))
        vsort <- names(sort(apply(kData[c(pop),],2,mean), decreasing = TRUE))[c(1,2,3)]
        popsort <- order(kData[c(pop),][vsort[1]],kData[c(pop),][vsort[2]],kData[c(pop),][vsort[3]])
        barnames[pop] <- barnames[pop[popsort]]
        kData[min(pop):max(pop),] <- kData[pop[popsort],]
      }
    }
    KData <- t(kData)
    #</PROCESSING>
    par(las=2, font.lab=2, mar=c(4,4,4,0)+0.1)
    wide1 <- rep(1,dim(KData)[2])
    # Check: All populations selected?
    if(all.pops()) {
      widee <- wide1
      if (!is.null(input$pope)) {
        pope.idx <- c()
        for(i in pop.emph()){pope.idx <- c(pope.idx,which(popinfo$POP %in% i))}
        widee <- wide1
        widee <- replace(widee,pope.idx,3)
      }
      barplot(KData, space=spaces, cex.names = 0.8,
              col=barcolors(), border=ifelse(input$brdr,"#AAAAAA",NA),
              names.arg = if(input$ptag) newnames else (barnames), 
              width= if(is.null(input$pope)) wide1 else (widee),
              main= input$plottitle, cex.axis = 0.8,
              xlab= "Population",ylab= paste("K= ",input$Ks))
    } else { # Subpopulation Selected
      sbPs.idx <- c()
      if (input$grps != ""){
        sbPs.idx <- which(popinfo[[input$fctr]] %in% input$grps)
        sbPs.title <- paste(input$fctr, ": ", input$grps, sep="")
      } else {
        for(i in sub.pops()){sbPs.idx <- c(sbPs.idx,which(popinfo$POP %in% i))}
        sbPs.title <- as.character(unique(popinfo$POP[sbPs.idx]))
      }
      subspaces <- c(0,diff(popinfo$POP[sbPs.idx]))
      subspaces <- replace(subspaces, subspaces != 0, 1)
      barplot(KData[,sbPs.idx], space= subspaces, cex.names = 1,
              col=barcolors(), border=ifelse(input$brdr,"#AAAAAA",NA),
              names.arg= if(input$ptag) newnames[sbPs.idx] else (barnames[sbPs.idx]),
              width= if(is.null(input$pope)) wide1 else (widee),
              main = input$plottitle,
              xlab= "Population",ylab= paste("K= ",input$Ks))
    }
  })
  #</OUTPUT>
}
#</SERVER>

#<APP>
shinyApp(ui = ui, server = server)
#</APP>

#<END>