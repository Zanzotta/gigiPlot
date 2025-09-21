#------------------------------------------------------------------------------#
#
#  EASY PLOT WITH GIGI FUNCTION
#
#------------------------------------------------------------------------------#

gigiPlot <- function(x, y = NULL, type = NULL){
  # Dplyr and ggplot has to be uploaded
  if(is.null(type) == T){ 
            type <- menu(c("Histogram (Frequency)","Histogram (Density)","Scatterplot","Path","Boxplot","Violinplot",
                 "Step","Line","Bar Chart (identity)","Bar Chart (coord flip)"), # 10 choices
                  title="What is your plot style ?")
                        }
  
  # This choice has been made to standardize variables name between plot definition
  dat <- tibble(x,y)
  x_name <- names(dat)[1]
  y_name <- names(dat)[2]
  
  switch (type,
          
          freqhist = ggplot(dat, aes(x=x, y=after_stat(x))) + 
            geom_histogram(bins = round(log2(length(x)) + 1), fill="blue1", color="white") + 
            ggtitle(paste("Histogram of ", x_name, sep = "")) +
            theme_minimal(base_size = 12),
          
          freqdens = ggplot(dat, aes(x=x, y=after_stat(density))) + 
            geom_histogram(bins = round(log2(length(x)) + 1), fill="blue1", color="white") + 
            geom_density(linewidth=0.75) + 
            ggtitle(paste("Histogram of ", x_name, sep = "")) +
            theme_minimal(base_size = 12),
          
          scatterplot = ggplot(dat, aes(x=x, y=y)) + 
            geom_point(alpha=0.6, size = 2) + 
            ggtitle(paste("Scatterplot, ", x_name,"vs",y_name , sep = " ")) +
            theme_minimal(base_size = 12),
          
          path = ggplot(dat, aes(x=x, y=y)) + 
            geom_path(linewidth=0.75) + 
            ggtitle(paste("Geom_Path ",x_name,"with",y_name , sep = " ")) +
            theme_minimal(base_size = 12),
          
          boxplot = ifelse(is.factor(y) == T,
                                print(ggplot(dat, aes(x=x, y=y, fill=y)) + 
                                  geom_boxplot() + 
                                  ggtitle(paste("Boxplot of",x_name, sep = " ")) +
                                  theme_minimal(base_size = 12)),
                                print("The selected y is not categorical. Change variable for the plot")),
          
          violinplot = ifelse(is.factor(y) == T,
                                print(ggplot(dat, aes(x=x, y=y, fill=y)) + 
                                  geom_violin(fill="blue1") + 
                                  ggtitle(paste("Boxplot of",x_name, sep = " ")) +
                                  theme_minimal(base_size = 12)), 
                                print("The selected y is not categorical. Change variable for the plot")),
          
          linestep = ggplot(dat, aes(x=x, y=y)) + 
            geom_step(linewidth=0.75) + 
            ggtitle(paste("Geom_Step ",x_name,"with",y_name , sep = " ")) +
            theme_minimal(base_size = 12),
          
          line = ggplot(dat, aes(x=x, y=y)) + 
            geom_line(linewidth=0.75) + 
            ggtitle(paste("Geom_Line ",x_name,"with",y_name , sep = " ")) +
            theme_minimal(base_size = 12),
          
          bar = ggplot(dat, aes(x=x, y=y)) + 
            geom_bar(stat="identity",fill="blue1", color="white") + 
            ggtitle(paste("Bar Chart ",x_name,"with",y_name , sep = " ")) +
            theme_minimal(base_size = 12),
          
          bar_flip = ifelse(is.factor(x) == T,
                            print(ggplot(dat, aes(x=x, y=after_stat(count))) + 
                                    geom_bar(fill="blue1", color="white") + 
                                    ggtitle(paste("Bar Chart of ",x_name, sep = " ")) +
                                    coord_flip() +
                                    theme_minimal(base_size = 12)),
                            print("The selected x is not categorical. Change variable for the plot"))
          
         )
  }


