read_spraychart = function(filename){
    library(png)
    library(ggplot2)
    library(reshape)
    library(dplyr)
    current_filename = gsub('.{4}$', '', filename)
    arrays = readPNG(filename)

    # get rid of the floating numbers
    array.r = arrays[,,1]
    array.g = arrays[,,2]
    array.b = arrays[,,3]

    rmelt = melt(array.r, varnames = c("Y", "X"))
    gmelt = melt(array.g, varnames = c("Y", "X"))
    bmelt = melt(array.b, varnames = c("Y", "X"))

    combine.melt = data.frame("X" = rmelt$X, "Y" = rmelt$Y, 
                              "R" = rmelt$value, "G" = gmelt$value, 
                              "B" = bmelt$value)

    # get unique complete color combos
    unique.col = unique(combine.melt[, 3:5])

    red.combo = combine.melt[19634, 3:5]
    green.combo = combine.melt[19647, 3:5]
    blue.combo = combine.melt[19660, 3:5]
    purple.combo = combine.melt[19673, 3:5]

    # create binary color matrices
    red.bin.mat = matrix(0, 600, 600)

    for(i in 1:600){
      for(j in 1:600){
        if(array.r[i, j] == red.combo$R && array.g[i,j] == red.combo$G && array.b[i, j] == red.combo$B){
            red.bin.mat[i, j] = 1
        }
      }
    }

    green.bin.mat = matrix(0, 600, 600)

    for(i in 1:600){
      for(j in 1:600){
        if(array.r[i, j] == green.combo$R && array.g[i,j] == green.combo$G && array.b[i, j] == green.combo$B){
          green.bin.mat[i, j] = 1
        }
      }
    }

    blue.bin.mat = matrix(0, 600, 600)

    for(i in 1:600){
      for(j in 1:600){
        if(array.r[i, j] == blue.combo$R && array.g[i,j] == blue.combo$G && array.b[i, j] == blue.combo$B){
          blue.bin.mat[i, j] = 1
        }
      }
    }

    purple.bin.mat = matrix(0, 600, 600)

    for(i in 1:600){
      for(j in 1:600){
        if(array.r[i, j] == purple.combo$R && array.g[i,j] == purple.combo$G && array.b[i, j] == purple.combo$B){
          purple.bin.mat[i, j] = 1
        }
      }
    }

    # get rid of legend values

    red.bin.mat[400:600, 0:50] = 0
    green.bin.mat[400:600, 0:50] = 0
    blue.bin.mat[400:600, 0:50] = 0
    purple.bin.mat[400:600, 0:50] = 0

    # taking middle values
    x.red = c()
    y.red = c()
    for(i in 1:596){
      for(j in 3:598){
        if(red.bin.mat[i, j] == 1 && red.bin.mat[i+1, j] == 1 && red.bin.mat[i+2, j] == 1 && 
           red.bin.mat[i+3, j] == 1 && red.bin.mat[i+4, j] == 1 && red.bin.mat[i+2, j-2] == 1 && 
           red.bin.mat[i+2, j-1] == 1 && red.bin.mat[i+2, j] == 1 && red.bin.mat[i+2, j+1] == 1 &&
           red.bin.mat[i+2, j+2] == 1){
          x.red = c(x.red, j)
          y.red = c(y.red, i+2)
        }
      }
    }

    x.green = c()
    y.green = c()
    for(i in 1:596){
      for(j in 3:598){
        if(green.bin.mat[i, j] == 1 && green.bin.mat[i+1, j] == 1 && green.bin.mat[i+2, j] == 1 && 
           green.bin.mat[i+3, j] == 1 && green.bin.mat[i+4, j] == 1 && green.bin.mat[i+2, j-2] == 1 && 
           green.bin.mat[i+2, j-1] == 1 && green.bin.mat[i+2, j] == 1 && green.bin.mat[i+2, j+1] == 1 &&
           green.bin.mat[i+2, j+2] == 1){
          x.green = c(x.green, j)
          y.green = c(y.green, i+2)
        }
      }
    }


    x.blue = c()
    y.blue = c()
    for(i in 1:596){
      for(j in 3:598){
        if(blue.bin.mat[i, j] == 1 && blue.bin.mat[i+1, j] == 1 && blue.bin.mat[i+2, j] == 1 && 
           blue.bin.mat[i+3, j] == 1 && blue.bin.mat[i+4, j] == 1 && blue.bin.mat[i+2, j-2] == 1 && 
           blue.bin.mat[i+2, j-1] == 1 && blue.bin.mat[i+2, j] == 1 && blue.bin.mat[i+2, j+1] == 1 &&
           blue.bin.mat[i+2, j+2] == 1){
          x.blue = c(x.blue, j)
          y.blue = c(y.blue, i+2)
        }
      }
    }

    x.purple = c()
    y.purple = c()
    for(i in 1:596){
      for(j in 3:598){
        if(purple.bin.mat[i, j] == 1 && purple.bin.mat[i+1, j] == 1 && purple.bin.mat[i+2, j] == 1 && 
           purple.bin.mat[i+3, j] == 1 && purple.bin.mat[i+4, j] == 1 && purple.bin.mat[i+2, j-2] == 1 && 
           purple.bin.mat[i+2, j-1] == 1 && purple.bin.mat[i+2, j] == 1 && purple.bin.mat[i+2, j+1] == 1 &&
           purple.bin.mat[i+2, j+2] == 1){
          x.purple = c(x.purple, j)
          y.purple = c(y.purple, i+2)
        }
      }
    }

    # combine the datapoints into one dataset
    location_data = data_frame("x" = c(x.red, x.green, x.blue, x.purple), 
                               "y" = 600 - c(y.red, y.green, y.blue, y.purple),
                               "hit_type" = factor(c(rep(1, length(y.red)), rep(2, length(y.green)), 
                                                     rep(3, length(y.blue)), rep(4, length(y.purple)))))

    ggplot(location_data) + geom_point(aes(x = x, y = y, colour = hit_type)) + 
      scale_colour_discrete(name = "Hit Type", labels = c("Line Drive", "Ground Ball", "Fly Ball", "Pop Up")) + 
      geom_point(data = fielders, aes(x = x, y = y), size = 3, color = "blue") + 
      geom_segment(aes(x = leftpole.x, y = leftpole.y, xend = homeplate.x, yend = homeplate.y)) + 
      geom_segment(aes(x = homeplate.x, y = homeplate.y, xend = rightpole.x, yend = rightpole.y)) + 
      geom_segment(aes(x = third.x, y = third.y, xend = second.x, yend = second.y)) + 
      geom_segment(aes(x = second.x, y = second.y, xend = first.x, yend = first.y)) + 
      ggtitle(current_filename) + xlab("") + ylab("") + coord_fixed() + 
      coord_fixed() + xlim(c(0, 600)) + ylim(c(0, 600))

  new_filename = paste(current_filename, ".csv", sep = "")
  write.csv(location_data, new_filename)
}
