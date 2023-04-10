norm_help <- function(percentile = NULL, 
                      z = NULL,
                      mean = 0, 
                      sd = 1, 
                      save = FALSE) {

  # Load/Install Packages  
  if(!require(ggplot2)){install.packages("ggplot2")}
  if(!require(svglite)){install.packages("svglite")}
  if(!require(ggrepel)){install.packages("ggrepel")}
  if(!require(dplyr)){install.packages("dplyr")}

  #Function agruments
  cent <- mean
  spread <- sd
  
  if (is.numeric(percentile)) {
    perc <- percentile
    z <- qnorm(perc/100, mean = 0, sd = 1)
  } else if (is.numeric(z)) {
    perc <- pnorm(z, mean = 0, sd = 1) * 100
  } else {
    perc <- .0001
    z <- qnorm(perc/100, mean = 0, sd = 1)
  }
  
  #Main Distribution
  quant <- qnorm(perc/100, mean = cent, sd = spread)
  
  x <- seq((cent - spread*3.5), (cent + spread*3.5), by = 0.01)
  
  df <- data.frame(x = x,
                   y = dnorm(x, mean = cent, sd = spread),
                   lab = paste("Percentile = ", round(perc, 2),
                               "\nz = ", round(z, 2),
                               "\nQuantile = ", round(quant, 2),
                               sep = "")
                   )
  
  
  
  # Percent Segments
  df_seg <- data.frame(
    segs = seq(-3, 3, 0.5)
  )
  
  df_seg$seg_loc = cent + spread * df_seg$segs
  df_seg$y_seg = dnorm(df_seg$seg_loc, mean = cent, sd = spread)
  
  #Text
    #Percentages
  text = c(rev(round(pnorm(seq(0.5, 3.5, 0.5)) * 100 - pnorm(seq(0, 3, 0.5)) * 100, 1)),
           round(pnorm(seq(0.5, 3.5, 0.5)) * 100 - pnorm(seq(0, 3, 0.5)) * 100, 1))
  
    #DataFrame of coords
  text_df <- data.frame(text,
                        x = seq(cent - 3.25 * spread,
                                cent + 3.25 * spread, 
                                length.out = 14),
                        y = dnorm(seq(cent - 3.25 * spread,
                                      cent + 3.25 * spread, 
                                      length.out = 14),
                                  mean = cent, sd = spread)
  )
  
    #Height Adjustment
  text_df$y <- text_df$y - text_df$y/2
  text_df$y[c(1:3, 12:14)] <- text_df$y[c(1:3, 12:14)] + 
                              text_df$y[c(1:3, 12:14)] * 
                              c(16, 4, 2.5, 2.5, 4, 16)
  
  
  #Plot
  x_nudge <- ifelse(z > 0, 1.25 * spread, (-1.25) * spread)
  caption <- paste("\n\n\nPercentile: A point in a distribution below which a ",
                    "specified percentage of the values fall.",
                   
                   "\n        Can be conceptualized as 'area under the curve' ",
                   "calculated from left to right. ",
                   "The 100th percentile the entire area.",
                   
                   "\n\nz-score: Distance from the mean (\U00B5) in standard ",
                   "deviations (\U03C3). ", 
                   "Note: each segment of the distribution represents 0.5 ",
                   "standard deviations.",
                   
                   "\n\nQuantile: A location on the distributions x-axis. ",
                   "The z-score is a type of quantile. ",
                   "When you have a \U00B5 = 0 and a \U03C3 = 1, the ",
                   "x-axis is z-scores.",
                   sep = "")
  title <- paste("Normal Distribution:\n\U00B5 = ", round(cent, 2),
                 ", \U03C3 = ", round(spread, 2),
                 sep = "")
  
  
  plot <- ggplot(df, aes(x = x, y = y)) +
    #geom_line(linewidth = 1) +
    
    #Percentile shading
    geom_ribbon(data = subset(df, x < quant), 
                aes(ymax = y, ymin = 0),
                fill = "#00BFFF", 
                colour = NA, 
                alpha = .5) +
    
    #Distribution
    stat_function(fun = dnorm, args = c(mean = cent, sd = spread),
                  linewidth = 1) +
    
    #Percentage Guide Segments
    geom_segment(data = df_seg, aes(x = seg_loc, 
                                    xend = seg_loc,
                                    y = 0, 
                                    yend = y_seg)) +
  
    #Requested Point
    geom_label_repel(data = subset(df, near(x, round(quant, 2))),
                     aes(label = lab),
                     box.padding   = 0.35, 
                     point.padding = 1,
                     segment.color = 'black',
                     nudge_x = x_nudge,
                     segment.angle = 20,
                     segment.size  = 0.75, 
                     size = 6) +
    
    geom_segment(x = quant, 
                 y = 0, 
                 xend = quant, 
                 yend = dnorm(quant, mean = cent, sd = spread),
                 colour = "red", 
                 linewidth = 1) +
    
    geom_point(data = subset(df, near(x, round(quant, 2))),
               colour = "red", 
               size = 5) +
    
    #Percentage Labels
    geom_text(data = text_df, aes(x = x, y = y), 
              label = paste(text, "%", sep = ""),
              colour = "#696969",
              size = 6) +
    
    #Axis
    scale_x_continuous(breaks = df_seg$seg_loc) +
    labs(x = "Quantile\n(i.e., x-value)",
         y = "Density",
         title = title,
         caption = caption) +
    
    theme_classic() +
    
    theme(plot.title = element_text(hjust=0.5, size = 24, face = "bold"),
          plot.caption = element_text(hjust=0, size = 14),
          axis.text = element_text(colour = "black", size = 18),
          axis.title = element_text(colour = "black", size = 20)
          )
  
  print(plot)
  
  if (save == TRUE) {
    filename <- paste(round(perc), "th Percentile Plot.svg", sep = "")
    
    ggsave(filename, dpi = 400, units = "in", width = 17, height = 10)
    
    message(paste("File saved as '", filename, "'", sep = ""))
  }
}

