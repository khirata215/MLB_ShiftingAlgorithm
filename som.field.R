som.field = function(hit_locations_x, hit_locations_y, fielders, nsim){
  # create fielders data for the function
  fielder_data = fielder_orig = fielders
  
  for(i in 1:nsim){
    # pick the random point 
    current_index = sample(1:length(hit_locations_x), 1)
    current_x = hit_locations_x[current_index]
    current_y = hit_locations_y[current_index]
    
    # find the closest fielder
    dist1 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 1])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 1])^2)
    dist2 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 2])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 2])^2)
    dist3 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 3])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 3])^2)
    dist4 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 4])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 4])^2)
    dist5 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 5])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 5])^2)
    dist6 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 6])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 6])^2)
    dist7 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 7])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 7])^2)
    dist8 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 8])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 8])^2)
    dist9 = sqrt((current_x - fielder_orig$x[fielder_orig$label == 9])^2 + 
                   (current_y - fielder_orig$y[fielder_orig$label == 9])^2)
    
    min_dist = min(dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8, dist9)
    
    # identify the closest fielder and they're locations
    fielder_data$distance = c(dist1, dist2, dist3, dist4, dist6, dist5, dist7, dist8, dist9)
    closest_label = fielder_data$label[fielder_data$distance == min_dist]
    closest_x = fielder_data$x[fielder_data$distance == min_dist]
    closest_y = fielder_data$y[fielder_data$distance == min_dist]
    closest_int_x = fielder_data$int_x[fielder_data$distance == min_dist]
    closest_int_y = fielder_data$int_y[fielder_data$distance == min_dist]
    
    
    if(closest_label == 4 | closest_label == 5 | closest_label == 6 |
       closest_label == 7 | closest_label == 8 | closest_label == 9){
      # calculate how much to move the node
      tau1 = (3/2)*exp(-i/10000 - min_dist/100)
      if(tau1 > 1){
        tau = 1
      }
      else{
        tau = tau1
      }
      # move the node
      new_x = closest_x + tau*(current_x - closest_x)
      new_y = closest_y + tau*(current_y - closest_y)
      
      # update the fielder dataset
      fielder_data$x[fielder_data$label == closest_label] = new_x
      fielder_data$y[fielder_data$label == closest_label] = new_y
      
    }
    # find and move the neighbors
    for(j in 1:length(fielders$x)){
      if(sqrt((fielder_data$int_x[j] - closest_int_x)^2 + (fielder_data$int_y[j] - closest_int_y)^2) <= sqrt(17) & 
         (fielder_data$label[j] == 4 | fielder_data$label[j] == 5 | fielder_data$label[j] == 6 | 
          fielder_data$label[j] == 7 | fielder_data$label[j] == 8 | fielder_data$label[j] == 9) & 
         fielder_data$label[j] != closest_label){
        # set values for this neighbor
        neighbor_label = fielder_data$label[j]
        neighbor_x = fielder_data$x[j]
        neighbor_y = fielder_data$y[j]
        
        # calculate the distance from this neighboring node to the current data point.
        neighbor_dist = (current_x - neighbor_x)^2 + 
          (current_y - neighbor_y)^2
        
        # calculate how much to move the node
        tau1 = (3/2)*exp(-i/10000 - neighbor_dist/100)
        if(tau1 > 1){
          tau = 1
        }
        else{
          tau = tau1
        }
        
        # move the node
        new_x = neighbor_x + tau*(min_dist/neighbor_dist)*(current_x - neighbor_x)
        new_y = neighbor_y + tau*(min_dist/neighbor_dist)*(current_y - neighbor_y)
        
        # update the fielder dataset
        fielder_data$x[j] = new_x
        fielder_data$y[j] = new_y
      }
    }
  }
  fielder_data_return = data.frame("x" = fielder_data$x, 
                                   "y" = fielder_data$y, 
                                   "label" = fielder_data$label)
  print(fielder_data_return)
}