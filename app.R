library(shiny)
library(ggplot2)



# Read in the data and create the plots
data <- read.csv("~/Downloads/data_not_mm10.csv")  # Update with correct file path
plots <- list()
for (i in 1:45) {
  mat_t <- t(data[i + 2,])
  df <- reshape2::melt(mat_t)
  df <- df[-1,]
  df$Var2 <- NULL
  df$group <- paste0("Metric ", i)
  df$value <- as.numeric(df$value)
  df <- df[-1,]
  
  set.seed(45)
  
  plot <- ggbetweenstats(data = df, 
                         x = group,
                         y = value,
                         outlier.tagging = FALSE,
                         outlier.label = Var1,
                         segment.curvature = 0,
                         segment.size      = 0.2,
                         min.segment.length = 10,
                         arrow = arrow(length = unit(0.015, "npc"))
  ) +
    scale_color_manual(values = c("#56B4E9")) +
    theme(axis.text = element_text(face = "bold", size = 10)) +
    xlab("Metrics") +
    ylab("Value") +
    coord_flip()
  
  plots[[i]] <- plot
}



ui <- fluidPage(
  selectInput("plot_select", label = "Select Plot", choices = c("Estimated Number of Cells",
                                                                "Feature linkages detected",
                                                                "Linked genes",
                                                                "Linked peaks",
                                                                "ATAC Confidently mapped read pairs",
                                                                "ATAC Fraction of genome in peaks",
                                                                "ATAC Fraction of high quality fragments in cells",
                                                                "ATAC Fraction of high quality fragments overlapping TSS",
                                                                "ATAC Fraction of high quality fragments overlapping peaks",
                                                                "ATAC Fraction of transposition events in peaks in cells",
                                                                "ATAC Mean raw read pairs per cell",
                                                                "ATAC Median high quality fragments per cell",
                                                                "ATAC Non nuclear read pairs",
                                                                "ATAC Number of peaks",
                                                                "ATAC Percent duplicates",
                                                                "ATAC Q30 bases in barcode",
                                                                "ATAC Q30 bases in read 1",
                                                                "ATAC Q30 bases in read 2",
                                                                "ATAC Q30 bases in sample index i1",
                                                                "ATAC Sequenced read pairs",
                                                                "ATAC TSS enrichment score",
                                                                "ATAC Unmapped read pairs",
                                                                "ATAC Valid barcodes",
                                                                "GEX Fraction of transcriptomic reads in cells",
                                                                "GEX Mean raw reads per cell",
                                                                "GEX Median UMI counts per cell",
                                                                "GEX Median genes per cell",
                                                                "GEX Percent duplicates",
                                                                "GEX Q30 bases in UMI",
                                                                "GEX Q30 bases in barcode",
                                                                "GEX Q30 bases in read 2",
                                                                "GEX Q30 bases in sample index i1",
                                                                "GEX Q30 bases in sample index i2",
                                                                "GEX Reads mapped antisense to gene",
                                                                "GEX Reads mapped confidently to exonic regions",
                                                                "GEX Reads mapped confidently to genome",
                                                                "GEX Reads mapped confidently to intergenic regions",
                                                                "GEX Reads mapped confidently to intronic regions",
                                                                "GEX Reads mapped confidently to transcriptome",
                                                                "GEX Reads mapped to genome",
                                                                "GEX Reads with TSO",
                                                                "GEX Sequenced read pairs",
                                                                "GEX Total genes detected",
                                                                "GEX Valid UMIs",
                                                                "GEX Valid barcodes")),
  plotOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    plot_index <- match(input$plot_select, c("Estimated Number of Cells",
                                             "Feature linkages detected",
                                             "Linked genes",
                                             "Linked peaks",
                                             "ATAC Confidently mapped read pairs",
                                             "ATAC Fraction of genome in peaks",
                                             "ATAC Fraction of high quality fragments in cells",
                                             "ATAC Fraction of high quality fragments overlapping TSS",
                                             "ATAC Fraction of high quality fragments overlapping peaks",
                                             "ATAC Fraction of transposition events in peaks in cells",
                                             "ATAC Mean raw read pairs per cell",
                                             "ATAC Median high quality fragments per cell",
                                             "ATAC Non nuclear read pairs",
                                             "ATAC Number of peaks",
                                             "ATAC Percent duplicates",
                                             "ATAC Q30 bases in barcode",
                                             "ATAC Q30 bases in read 1",
                                             "ATAC Q30 bases in read 2",
                                             "ATAC Q30 bases in sample index i1",
                                             "ATAC Sequenced read pairs",
                                             "ATAC TSS enrichment score",
                                             "ATAC Unmapped read pairs",
                                             "ATAC Valid barcodes",
                                             "GEX Fraction of transcriptomic reads in cells",
                                             "GEX Mean raw reads per cell",
                                             "GEX Median UMI counts per cell",
                                             "GEX Median genes per cell",
                                             "GEX Percent duplicates",
                                             "GEX Q30 bases in UMI",
                                             "GEX Q30 bases in barcode",
                                             "GEX Q30 bases in read 2",
                                             "GEX Q30 bases in sample index i1",
                                             "GEX Q30 bases in sample index i2",
                                             "GEX Reads mapped antisense to gene",
                                             "GEX Reads mapped confidently to exonic regions",
                                             "GEX Reads mapped confidently to genome",
                                             "GEX Reads mapped confidently to intergenic regions",
                                             "GEX Reads mapped confidently to intronic regions",
                                             "GEX Reads mapped confidently to transcriptome",
                                             "GEX Reads mapped to genome",
                                             "GEX Reads with TSO",
                                             "GEX Sequenced read pairs",
                                             "GEX Total genes detected",
                                             "GEX Valid UMIs",
                                             "GEX Valid barcodes"))
                        plots[[plot_index]]
  })
}    
                                             
shinyApp(ui, server)
