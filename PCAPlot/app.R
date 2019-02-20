# AUTO PCA PLOTTER (Shiny) v.2
# Shiny: app.R
# Author: Pavel Salazar-Fernandez
# Developed at: LANGEBIO - Mexico
# Last Edit: May 08 2017

# Requirements:
# - EVAL and EVEC files from the smartpca*. Only one per directory.
# - popinfo file


# Pipeline:
# 1. Reads first .evec and .eval files from a chosen directory.
# 2. Identifies names, regions and populations from a given popinfo.txt
# 3. Generates a color-coded PCA plot.

# Features:
# - Plot types: Can select between points or tags for the plot.
# - Select Population: Displays only selected population(s).
# - Emphasize Population: Highlights points or tags for a chosen population.
# - Color by Category: User can choose the criteria for coloring using the popinfo.
# - Auto-Legend: Shows color codings for the selected category.
# - Interactive Zoom: select an area and double click to zoom in, double click again to zoom out.
# - [TBD] ASH transformation for contextual zoom.

#<START> ####
# Load required libraries 
require(shiny)
library(scales)
library(ggplot2)

#<INPUT> ####
# Choose data directory
setwd(choose.dir(default = "", caption = "Select folder with PCA files"))

#</INPUT>

#<PREPARATIONS> ####

# FUNCTIONS
refact <- function (x){
  if (!is.factor(x)) 
    x <- factor(x)
  ll <- as.character(na.omit(unique(x)))
  if (anyNA(x))
    ll <- c(ll, NA)
  factor(x, levels = ll, exclude = NULL)
}


# Load PCA files
pca.data <- read.table(dir(pattern = "\\.evec$")[1], stringsAsFactors = F)
eval <- scan(dir(pattern = "\\.eval$")[1])

ncomps <- table(sapply(pca.data, class))["numeric"]
PC1Col <- match("numeric",sapply(pca.data, class))
IDcol <- PC1Col - 1
colnames(pca.data)[IDcol:(IDcol+ncomps)] <- c("ID",paste0("PC",1:ncomps))
popIDs <- gsub(".*:","",pca.data$ID)

# Read popinfo file (.txt):
pifile <- dir(pattern = "\\popinfo.txt$")[1]
popinfo <- read.table(pifile,sep="\t",header=TRUE,comment.char = "")

# Data merging
pca.data <- merge(pca.data, popinfo, by.x = "ID", sort = F)
pca.data <- as.data.frame(lapply(pca.data, function(x) if(class(x) == "factor"){refact(x)} else{x}))

# Data field scan
pops <- as.character(unique(pca.data$POP))
uniquecols <- sapply(pca.data,function(x) length(unique(x)))
fields <- names(uniquecols[uniquecols>1])[-(IDcol:(IDcol+ncomps))]
if ("POP_SIMPLE" %in% colnames(pca.data)) {
  names(pops) <- unique(paste0(pca.data$POP," (",pca.data$POP_SIMPLE,")"))
}
deftitle <- unlist(strsplit(pifile,"[.]"))[1]

#</PREPARATIONS>

#<UI> ####
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
                                choices= pops, selected= NULL,
                                options= list(maxItems = length(pops)-1,
                                              placeholder = 'Select population(s)',
                                              onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 selectizeInput("pope", label = "Populations emphasis:", 
                                choices= pops, selected= NULL, multiple= F,
                                options= list(placeholder = 'None',
                                  onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 hr(),
                 selectizeInput("flds", label = "Group Coloring:", 
                                choices= fields, selected = "POP",
                                options= list(maxItems = 1,
                                              placeholder = 'Choose color grouping'#,
                                              #onInitialize = I('function() { this.setValue(""); }')
                                              )),
                 radioButtons("type", label = "Type:",
                              choices = list("Points" = 1, "Text" = 2), 
                              selected = 1),
                 hr(),
                 downloadButton('dlPlot', 'Save as PDF'),
                 hr(),
                 h5("Points Info"),
                 verbatimTextOutput("brshinfo")
    ),
    
    # Plotting Area
    mainPanel(width=9,
              plotOutput("PCAPlot", width = "1080px", height= "960px", dblclick = "dclk", brush = brushOpts(id= "brsh", resetOnNew = TRUE))  
    )
  )
)
#</UI>

#<SERVER> ####
server <- function(input, output) {
  
  #<REACTIVES>
  # Inputs
  sub.pops <- reactive(pca.data[pca.data$POP %in% input$pops,IDcol])
  pope.idn <- reactive(pca.data[pca.data$POP %in% input$pope,IDcol])
  groups <- reactive(as.character(levels(pca.data[pca.data$POP != input$pope,input$flds])))
  PCaCol <- reactive(paste0("PC",input$PCa))
  PCbCol <- reactive(paste0("PC",input$PCb))
  pct.PCa <- reactive(paste(PCaCol()," (",percent(eval[input$PCa]/sum(eval)),")",sep=""))
  pct.PCb <- reactive(paste(PCbCol()," (",percent(eval[input$PCb]/sum(eval)),")",sep=""))
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
  
  #<OUTPUT> ####
  output$PCAPlot <- renderPlot({
    # Data subset
    pca.keep <- pca.data
    if (!is.null(input$pops)){
      pca.keep <- pca.data[ popIDs %in% sub.pops(), , drop= F]
    }
    if (input$pope %in% pops){
      pca.emph <- pca.data[ popIDs %in% pope.idn(), , drop= F]
      pca.emph$plotColors <- "#000000FF"
      pca.keep <- subset(pca.keep, !(ID %in% pca.emph$ID))
    }
    
    #Coloring
    if (input$flds == "POP") {
      if("COLOR" %in% colnames(pca.keep)){
        pca.keep$plotColors <- paste0(pca.keep$COLOR,"FF")
      } else {
        grp.colors <- setNames(rainbow(length(pops), s = 0.9, v = 0.9), pops)
        pca.keep$plotColors <- grp.colors[pca.keep$POP]
      }
    } else {
      grp.colors <- setNames(rainbow(length(na.omit(groups())), s = 0.9, v = 0.9), na.omit(groups()))
      if(any(is.na(groups()))){grp.colors <- c(grp.colors,setNames("#777777FF", NA))}
      pca.keep$plotColors <- grp.colors[as.character(pca.keep[,input$flds])]
    }
    
    pca.keep$plotColors <- factor(pca.keep$plotColors, unique(pca.keep$plotColors))
    if(any(is.na(groups()))){
      levels(pca.keep$plotColors) <- c(levels(pca.keep$plotColors),"#777777FF")
      pca.keep$plotColors[is.na(pca.keep$plotColors)] <- "#777777FF"
    }
    
    #Plotting
    pca.plot <- ggplot(data= pca.keep, aes_string(x= PCaCol(), y= PCbCol(), color="plotColors")) + theme_light() + 
      geom_hline(yintercept= 0, colour= "#333333") + geom_vline(xintercept= 0, colour= "#333333") +
    {if (input$type == 1) { #POINTS
        geom_point(size= 3, alpha= 0.8)
    }} +
    {if (input$type == 2) { #TEXT
        geom_text(aes(label= ID), size = 3, alpha= 0.8)
    }} +
    #Emphasis
    {if (input$pope %in% pops & input$type == 1) { 
      geom_point(data= pca.emph, aes_string(x= PCaCol(), y= PCbCol(), color="plotColors"), size= 4, alpha= 0.8, shape= 23, fill= "black", color= "white")}} +
    {if (input$pope %in% pops & input$type == 2) {
         geom_label(data= pca.emph, aes_string(x= PCaCol(), y= PCbCol(), label= "ID"), size = 3, alpha= 0.8, color= "white", fill= pca.emph$plotColors, fontface="bold")}} +
      scale_color_identity(name= input$flds, labels= groups(), guide= "legend") +
    ggtitle(ifelse(input$plottitle == "", deftitle, input$plottitle)) + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
    labs(x= pct.PCa(), y= pct.PCb()) + coord_cartesian(xlim= ranges$x, ylim= ranges$y, expand = T)
    
    #Output
    ggsave("PCAplot.pdf",pca.plot, width= 24, height= 21, units="cm")
    pca.plot
  })
  
  output$dlPlot <- downloadHandler(
    filename = "PCAplot.pdf",
    content = function(file) {
      file.copy("PCAplot.pdf", file, overwrite=TRUE)
    }
  )
  
  output$brshinfo <- renderPrint({pca.data[rownames(brushedPoints(pca.data[,],input$brsh, xvar= PCaCol(), yvar= PCbCol())),c("ID","POP","POP_SIMPLE")]})
  #</OUTPUT>
}

#</SERVER>

#<APP> ####
shinyApp(ui = ui, server = server)
#</APP>

#<SANDBOX> ####

#<END> ####