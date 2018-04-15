#' Array plot function to work on ST data
#'
#' Opens up a gadet, put your gene name to display the counts along the tissue.
#' Your count matrix needs to be in the form samples -> columns, genes -> rows.
#' The columns names needs to be in the form of [x]x[y], where [x] and [y] are coordinates on the array.
#' @keywords arrayplot
#' @export
#' @name arrayPlot
#' @examples
#' arrayPlot(counts)
library(shiny)
library(shinyWidgets)
library(miniUI)
library(ggplot2)

arrayPlot <- function(countM) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package \"shiny\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
    stop("Package \"shinyWidgets\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("miniUI", quietly = TRUE)) {
    stop("Package \"miniUI\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  ui <- miniPage(
    gadgetTitleBar("Make arrayplot of chosen gene"),
    miniContentPanel(
      plotOutput("plot", height = "100%", brush = "brush"),
    miniButtonBlock(
      textInput("geneButton", "Gene: ", placeholder = "gene"), border = "top"
  )
  ))

  server <- function(input, output, session) {


    arrayImgInput <- reactive({
      input$geneButton
    })


    # Render the plot
    output$plot <- renderPlot({

      xcoord = as.numeric(sapply(strsplit(colnames(countM), "x"), "[[", 1))
      ycoord = as.numeric(sapply(strsplit(colnames(countM), "x"), "[[", 2))
      df = as.data.frame(cbind(x=xcoord, y=ycoord))


      if(input$geneButton %in% rownames(countM)){
        CM = countM[which(rownames(countM) == input$geneButton), ]

        df = cbind(df, CM)
        ggplot(df, aes(x=x, y=y)) + geom_point(size=CM/mean(CM)) +
          ggtitle(input$geneButton) +
          coord_fixed() +
          theme_bw()
      }else{
        ggplot(df, aes(x=x, y=y))+
          ggtitle("Gene not found")+
          coord_fixed()+
          theme_bw()
      }



    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(data, input$brush))
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ggbrush"))

}

