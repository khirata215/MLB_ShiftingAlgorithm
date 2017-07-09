
fielder_dist = function(hit_locations_x, hit_locations_y, fielders){
  sum.dist = 0    
  for(i in 1:length(hit_locations_x)){
      # getting the squared distance for each to each fielder
      dist1 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 1])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 1])^2)
      
      dist2 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 2])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 2])^2)
      
      dist3 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 3])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 3])^2)
      
      dist4 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 4])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 4])^2)
      
      dist5 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 5])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 5])^2)
      
      dist6 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 6])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 6])^2)
      
      dist7 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 7])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 7])^2)
      
      dist8 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 8])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 8])^2)
      
      dist9 = sqrt((hit_locations_x[i] - fielders$x[fielders$label == 9])^2 + 
                     (hit_locations_y[i] - fielders$y[fielders$label == 9])^2)
      
      # finding the minimum to add it to our running total
      sum.dist = sum(sum.dist, min(dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8, dist9))
  }
  return(sum.dist)
}