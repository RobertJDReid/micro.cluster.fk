library(shiny)
library(dendextend)
library(tidyverse)

microDF <- readRDS("cluster-meta.RDS")
microDend <- readRDS("dend.RDS")

ui <- fluidPage(
  fluidRow(
    column(4,
           selectizeInput("sample", "Sample", unique(microDF$sample[
             order(as.numeric(str_extract(microDF$sample,"\\d+")))
           ] ))
    ),
    column(4,
           selectizeInput("field", "Field", unique(microDF$field[
             order(as.numeric(microDF$field))
           ] ))
    )
  ),
  #textOutput("selected_sample"),
  #textOutput("selected_field"),
  
  plotOutput("cluster_pos",
             click = "plot_click",
             brush = "plot_brush"),
  
  tableOutput("field_table"),
  
  fluidRow(
    column(4,
           plotOutput("dendrogram")
    ),
    column(4,
           plotOutput("cell"))
  )
)

server <- function(input, output, session) {
  output$selected_sample <- renderText(as.character(input$sample ))
  output$selected_field <- renderText(as.character(input$field ))
  output$cluster_pos <- renderPlot(
    microDF %>%
      filter(sample == input$sample) %>%
      filter(field  == input$field) %>%
      ggplot(aes(x=loc.x,y=loc.y)) +
      geom_point() +
      #scale_y_reverse() +
      lims(x=c(0,86.6),y=c(65.7,0)) +
      theme_bw() +
      theme(aspect.ratio=510/672)
  )
  # output$field_table <- renderTable(
  #   microDF %>%
  #     filter(sample == input$sample) %>%
  #     filter(field  == input$field)
  # )
  output$field_table <- renderTable(
    {req(input$plot_click)
      nearPoints(filter(microDF,
                        sample == input$sample,
                        field  == input$field),
                 input$plot_click)
    }
  )
  # output$field_table <- renderTable(
  #   {req(input$plot_brush)
  #     brushedPoints(filter(microDF,
  #                          sample == input$sample,
  #                          field  == input$field),
  #                   input$plot_brush)
  # 
  #   }
  # )
  output$dendrogram <- renderPlot({
    plot_num <- paste(input$sample,input$field,sep = ".")
    microDend[[plot_num]] %>%
      plot(xlim = c(3,0),
           horiz=TRUE,
           main = paste0("no. of nodes ",
                         nleaves(microDend[[plot_num]]),plot_num)
           #asp = 300/(nleaves(microDend[[paste(input$sample,input$field,sep = ".")]])*10 + 20)
      )
  },
  #height = 600,
  height = reactive( 150 +
    nleaves(microDend[[paste(input$sample,input$field,sep = ".")]])*12
  ),
  width = 300
  )
  output$cell <- renderPlot(
    {
      req(input$plot_click)
      x <- nearPoints(filter(microDF,
                             sample == input$sample,
                             field  == input$field),
                      input$plot_click)
      plot_num <- paste(input$sample,input$field,sep = ".")
      local_dend <- microDend[[plot_num]]
      selected_signals <- unlist(stringr::str_extract_all(x$signals,"\\w+ \\d+"))
      prune_signals <- labels(local_dend)[which(!labels(local_dend) %in% selected_signals)]
      local_dend %>%
        prune(prune_signals) %>%
        plot(xlim = c(3,0),
             horiz=TRUE)
    }
  )
  # output$cell <- renderPlot(
  #   {
  #     req(input$plot_brush)
  #     x <- brushedPoints(filter(microDF,
  #                            sample == input$sample,
  #                            field  == input$field),
  #                     input$plot_brush)
  #     plot_num <- paste(input$sample,input$field,sep = ".")
  #     local_dend <- microDend[[plot_num]]
  #     selected_signals <- unlist(stringr::str_extract_all(x$signals,"\\w+ \\d+"))
  #     prune_signals <- labels(local_dend)[which(!labels(local_dend) %in% selected_signals)]
  #     local_dend %>%
  #       prune(prune_signals) %>%
  #       plot(xlim = c(3,0),
  #            horiz=TRUE)
  #   }
  # )
}

shinyApp(ui, server)