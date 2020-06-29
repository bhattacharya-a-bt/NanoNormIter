#### INPUT: data - matrix of expressions with genes on
####                rows and samples on columns
####        metadata - matrix of metadata with a column
####                that corresponds to the colnames of data
####        id - colname of sample ids
#### OUTPUT: ggplot2 RLE plot

makeRLEplot <- function(data,metadata,id){

    data = data - apply(data,1,median)
    stack = stack(data)
    colnames(stack)[1] = id
    stackPlot = merge(stack,metadata,by=id)
    colnames(stackPlot)[1:2] = c('Sample','values')
    rle_plots = ggplot(data = stackPlot,aes(x = Sample,y = values, color = ER_status)) +
        geom_boxplot(coef = 6) + theme_minimal() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=24),
              plot.title = element_text(size = 30),
              legend.title=element_text(size=20),
              legend.text=element_text(size=20),
              strip.text = element_text(size=24),
              panel.spacing=unit(1, "lines"),
              panel.border = element_rect(color = "grey",
                                          fill = NA, size = .1),
              legend.position = 'bottom',
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) + xlab('Sample') +
        ylab('Median deviation of log expression') + ylim(c(-4,4))
    return(rle_plots)

}
