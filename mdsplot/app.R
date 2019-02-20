# AUTO MDS PLOTTER (Shiny)
# Shiny: app.R
# Author: Pavel Salazar-Fernandez
# Developed at: LANGEBIO - Mexico
# Last Edit: March 10 2016

# Requirements:
# - MDS file from the plink. Only one per directory.
# - popinfo file

# Pipeline:
# 1. Reads first .mds file from a chosen directory.
# 2. Identifies names, regions and populations from a given popinfo.txt
# 3. Generates a MDS plot with color-coded regions.

# Features:
# - Plot types: Can select between points or tags for the plot.
# - Select Population: Shows only a particular population.
# - Emphasize Population: Highlights points or tags for a chosen population.
# - Color by Category: User can choose the criteria for coloring.
# - Show/Hide Legend: Displays all values from the selected category.
# - Zoom: Expands view to better visualize a population.

#<START>
# Load required libraries 
library(shiny)

#<INPUT>
# Choose data directory
setwd(choose.dir(default = "", caption = "Select folder with MDS files"))
#</INPUT>

#<PREPARATIONS>
# Read popinfo file (.txt):
pifile <- dir(pattern = "\\.txt$")[1]
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

# Load MDS files
mds <- dir(pattern = "\\.mds$")[1]
data <- read.table(mds, header=TRUE)
IDcol <-2
xcol <- 4
ycol <- 5
popIDs <- as.character(data[,2])
nsamples <- length(popIDs)
idx <- match(popIDs, popinfo$ID)

#</PREPARATIONS>

#<FUNCTIONS>
ZoomLim <- function(subpop = integer(0), pct = 0.1) {
  limits <- c()
  if(length(subpop) == 0){subpop <- c(1:nsamples)}
  limits <- c(limits,min(data[subpop,xcol])-abs(min(data[subpop,xcol])* pct))
  limits <- c(limits,max(data[subpop,xcol])+abs(max(data[subpop,xcol])* pct))
  limits <- c(limits,min(data[subpop,ycol])-abs(min(data[subpop,ycol])* pct))
  limits <- c(limits,max(data[subpop,ycol])+abs(max(data[subpop,ycol])* pct))
  return(limits)
}
#</FUNCTIONS>

#<UI>
ui <- fluidPage(    
  # Page Title
  img(src="MorLabLogo.jpg", style = "float:right"),
  titlePanel("Auto MDS Plotter"),
  helpText("Author: Pavel Salazar-Fernandez | Developed at: LANGEBIO (MX)"),
  hr(),    
  # Sidebar
  sidebarLayout(      
    
    # Input Panels
    sidebarPanel(width=2,
                 h3("Settings"),
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
                 selectizeInput("flds", label = "Group Coloring.", 
                                choices= fields, selected = NULL,
                                options= list(maxItems = 1,
                                              placeholder = 'Choose color grouping',
                                              onInitialize = I('function() { this.setValue(""); }'))),
                 radioButtons("type", label = "Type:",
                              choices = list("Points" = 1, "Text" = 2), 
                              selected = 1),
                 checkboxInput("leg", label = "Legend", value = FALSE),
                 checkboxInput("zoom", label = "Zoom In", value = FALSE),
                 sliderInput("zoomaxis", label = "Zoom Out %:", min = 100, max = 800,
                             value = 120, step = 10)
    ),
    
    # Plotting Area
    mainPanel(width=10,
              plotOutput("MDSPlot",width="100%",height="900px")  
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
  zoomaxis <- reactive(input$zoomaxis/100 - 1)
  grouping <- reactive(if(input$flds != "")as.character(unique(popinfo[,input$flds])) else(""))
  #</REACTIVES>
  
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
      coloring <- c("Samples","(Emphasis)")
      groups.color <- c("#666666","#000000")
      data$Colors <- "#666666"
    }
    if(length(pope.idx) > 0){coloring[length(coloring)] <- input$pope}
    zLimits <- ZoomLim(if (length(pope.idx) > 0) pope.idx else (sbP.idx),zoomaxis())
    plot(data[,c(xcol,ycol)], col= data$Colors, pch= 20, xlab= "C1",
         ylab= "C2", cex.main = 1.5,
         main= ifelse(is.null(input$pops),"All Populations",paste(input$pops, collapse =" / ")),
         type= ifelse(is.null(input$pops) && input$type == 1,"p","n"), cex=3,
         xlim= if (input$zoom == TRUE) zLimits[0:2] else (range(data[,xcol])),
         ylim= if (input$zoom == TRUE) zLimits[3:4] else (range(data[,ycol]))
    )
    abline(v= 0, h= 0, lty= 2, col= "grey")
    if(input$leg) legend("topright", gsub("_"," ",coloring), ncol= 1, col= groups.color, pch= 20,
                         pt.cex= 3, bty= "n")
    if (input$type==1) {
      points(data[sbP.idx,c(xcol,ycol)], cex= 2, pch= 19, col= data$Colors[sbP.idx])
      points(data[pope.idx,c(xcol,ycol)], cex= 2.5, pch= 23, bg="#000000", col= "#FFFFFF")
    }
    else {
      if (is.null(input$pops)) text(data[,c(xcol,ycol)], labels= popinfo[idx,IDcol], col= data$Colors, cex= 0.9)
      else text(data[sbP.idx,c(xcol,ycol)], labels=data[sbP.idx,IDcol], cex= 1, col= data$Colors[sbP.idx])
      if (length(pope.idx) > 0) text(data[pope.idx,c(xcol,ycol)], labels= data[pope.idx,IDcol], font= 2, cex=1, col= "#000000")
    }
  })
  #</PROCESSING>
  
  #<OUTPUT>
  output$MDSPlot <- renderPlot(hiplot())
  #</OUTPUT>
}
#</SERVER>

#<APP>
shinyApp(ui = ui, server = server)
#</APP>

#<END>