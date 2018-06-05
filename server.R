server = function(input, output){
  ## Get location info and check if valid address
  shinyjs::hide("Go")
  shinyjs::hide("amenities")
  shinyjs::hide("knn")
  shinyjs::hide("close")
  shinyjs::hide("close_knn")
  
  proceed <- 0
  inputvector =  numeric(13) #numeric(length(colnames(data)))
  
  ### Create the link for the clickable webpages
  
  get_places = function(location){
    # place_stat = rep(0, length(l_places))
    key <- 'AIzaSyBS7BpcBacrqZNY5oq5TVE9aTgVYjMvRuw'
    places = google_places(search_string = location,
                           key = key)
    latitude <- places$results$geometry$location$lat
    longitude <- places$results$geometry$location$lng
    return(c(latitude, longitude))
  }
  
  output$location <- renderUI({
    textInput(inputId = 'location2',
              label = 'Please Enter An Address',
              value = 'Be Grand® Alto Polanco, Preventa Departamentos Torre IV')
  })
  
  
  observeEvent(input$Go1, {
    shinyjs::hide("Go")
    
    input$Go1
    places_vec = get_places(paste(input$location2, " Mexico"))
    # places_vec = get_places('Be Grand® Alto Polanco, Preventa Departamentos Torre IV')
    
    if(length(places_vec) != 2){
      input$Go1
      places_vec <<- get_places(input$location2)
    }
    
    output$warning <- renderText({
      if(length(places_vec) != 2){
        proceed <<- 0
        print("Please Enter A Valid Address")
      }else{
        proceed <<- 1
        shinyjs::hide("Go1")
        shinyjs::show("Go")
        inputvector[1:2] <- places_vec
        print("Please Enter More Information")
      }
    })
    
    output$total_area <- renderUI({
      if(proceed == 1){
        textInput(inputId = 'total_area2',
                  label = 'Total Area',
                  value = 100)
      }
    })
    output$builded_surface <- renderUI({
      if(proceed == 1){
        textInput(inputId = 'builded_surface2',
                  label = 'Builded Surface',
                  value = 80)
      }
    })
    output$bedrooms <- renderUI({
      if(proceed == 1){
        textInput(inputId = 'bedrooms2',
                  label = 'Total Bedrooms',
                  value = 3)
      }
    })
    output$bathrooms <- renderUI({
      if(proceed == 1){
        textInput(inputId = 'bathrooms2',
                  label = 'Total Bathrooms',
                  value = 2)
      }
    })
    output$k <- renderUI({
      if(proceed == 1){
        textInput(inputId = 'k2',
                  label = 'Choose K Nearest Neighbors',
                  value = 5)
      }
    })
    
    observeEvent(input$Go, {
      
      #############
      # Create a Progress object
      progress <- shiny::Progress$new()
      
      progress$set(message = "Loading", value = 0)
      ############
      
      # inputvector =  numeric(13) #numeric(length(colnames(data)))
      input$Go
      
      #now add the googleMaps features
      places_vec <- get_places(input$location2)
      # places_vec <- get_places("Be Grand® Alto Polanco, Preventa Departamentos Torre IV")
      
      
      inputvector[1:2] <- places_vec
      inputvector[3] <- as.numeric(as.character(input$bathrooms2))
      inputvector[4] <- as.numeric(as.character(input$bedrooms2))
      inputvector[5] <- as.numeric(as.character(input$builded_surface2))
      inputvector[6] <- as.numeric(as.character(input$total_area2))
      
      inputdf <<- as.data.frame(t(as.matrix(inputvector)))
      colnames(inputdf) <<- cols
      
      #l_places = colnames(d_radius_r) #[7:13]
      l_places = c("food", "hospital",  "lodging", "school", 
                   "university", "shopping_mall", "park")
      
      #now we want 4 dataframes for places nearby
      places_nearby <- data.frame(t(as.matrix(numeric(7))))
      #number of places nearby for each type of amenity
      places_nearby_num <- data.frame(t(as.matrix(numeric(7))))
      #latitude of each amenity
      places_nearby_lat <- data.frame(t(as.matrix(numeric(7))))
      #longitude of each amenity
      places_nearby_lon <- data.frame(t(as.matrix(numeric(7))))
      colnames(places_nearby) <- l_places #colnames(data)[7:13]
      colnames(places_nearby_num) <- l_places #colnames(data)[7:13]
      colnames(places_nearby_lat) <- l_places #colnames(data)[7:13]
      colnames(places_nearby_lon) <- l_places #colnames(data)[7:13]
      
      for(place in l_places){
        key <- 'AIzaSyBS7BpcBacrqZNY5oq5TVE9aTgVYjMvRuw'
        places = google_places(location = c(inputvector[1], inputvector[2]),
                               place_type = place,
                               radius = as.numeric(d_radius_r[place]),
                               key = key)
        if(length(places$results) != 0){
          names <- places$results$name[1]
          places_nearby[place] <- names #name of all places of an amenity type
          num <- ifelse(length(places["results"]$results$name) == 20, ">20", as.character(length(places["results"]$results$name)))
          places_nearby_num[place] <- num
          latitude <- places$results$geometry$location$lat[1]
          places_nearby_lat[place] <- latitude
          longitude <- places$results$geometry$location$lng[1]
          places_nearby_lon[place] <- longitude
        }else{
          places_nearby[place] <- "NA"
          places_nearby_num[place] <- 0
          places_nearby_lat[place] <- "None"
          places_nearby_lon[place] <- "NA"
        }
        n = length(places$results$name)
        if(place %in% l_places){
          inputdf[place] <<- as.numeric(as.character(n))
        }
        progress$inc(1/length(l_places), detail = paste("adding", place))
      }
      
      places_nearby_new <<- data.frame(row.names = 1:ncol(places_nearby))
      places_nearby_new['Amenity'] <<- as.character(colnames(places_nearby))
      places_nearby_new['Popularity'] <<- as.character(places_nearby_num[1, ])
      places_nearby_new['Closest Amenity'] <<- as.character(places_nearby[1, ])
      places_nearby_new['Lat'] <<- as.character(places_nearby_lat[1, ])
      places_nearby_new['Lon'] <<- as.character(places_nearby_lon[1, ])
      places_nearby_new['Distance'] <<- 111*sqrt((as.numeric(places_nearby_new$Lat) - as.numeric(places_vec[1]))^2 + (as.numeric(places_nearby_new$Lon) - as.numeric(places_vec[2]))^2)
      places_nearby_new['Distance1'] <<- 111*sqrt((as.numeric(places_nearby_new$Lat) - as.numeric(places_vec[1]))^2 + (as.numeric(places_nearby_new$Lon) - as.numeric(places_vec[2]))^2)
      places_nearby_new$Distance1 <<- ifelse(places_nearby_new$Popularity == 0, as.numeric(d_radius_r[places_nearby_new$Amenity])/1000, places_nearby_new$Distance)
      
      places_nearby_new <<- places_nearby_new %>% 
        arrange(Distance1) %>% 
        select(Amenity, Popularity, `Closest Amenity`, Distance)
      places_nearby_new$Distance <<- ifelse(places_nearby_new$Popularity == 0, paste(">", round(d_radius_r[places_nearby_new$Amenity]/1000, 2), sep = ''), round(places_nearby_new$Distance, 2))
      places_nearby_new$Distance <<- paste(places_nearby_new$Distance, "km", sep = "")
      
      on.exit(progress$close())
      
      pred = get_price(inputdf)
      pred1 = pred
      #pred = predict(model, newdata = inputdf)
      #print(paste("Your property price is ", pred))
      #pred = round(pred, digits = -3)
      pred = sapply(pred, FUN= function(x) prettyNum(x, big.mark=","))
      pred = paste("MN", pred)
      #return(pred)
      
      dist_ind = get_knn(inputdf[,1:2])
      dist = unlist(dist_ind[1][[1]])
      index_knn <<- unlist(dist_ind[2][[1]])
      
      #the number of neighbours needed
      k <<- as.integer(input$k2)
      output$boxplot <- renderPlot({
        # pred = get_price(inputdf)
        discount_knn <- data.frame(mape = data[index_knn[1:(1+k)],]$mape_test, group = paste(k, "NN", sep = ""))
        discount_knn$Price_MN = 1/ (1 - discount_knn$mape) * pred1 #(discount_knn$mape + 1) * pred1
        ggplot(discount_knn) +
          aes(x = group, y = Price_MN, fill = "green") +
          geom_boxplot(fill = "Forest Green") +
          coord_flip() +
          labs(y = "Price in MN", 
               title = paste("Confidence Interval of Price for The ", k, "-Nearest Neighbors", sep = "")) +
          geom_text(data = discount_knn, 
                    aes(y = quantile(discount_knn$Price_MN)[2], 
                        label = paste(as.character(round(quantile(discount_knn$Price_MN)[2]/1e6 , digits = 2)), "M", sep = '')), vjust = 10, color = "Black") +
          geom_text(data = discount_knn, 
                    aes(y = quantile(discount_knn$Price_MN)[3], 
                        label = paste(as.character(round(quantile(discount_knn$Price_MN)[3]/1e6, digits = 2)), "M", sep = '')), vjust = 8.5, color = "Black") + 
          geom_text(data = discount_knn, 
                    aes(y = quantile(discount_knn$Price_MN)[4], 
                        label = paste(as.character(round(quantile(discount_knn$Price_MN)[4]/1e6, digits = 2)), "M", sep = '')), vjust = 6.5, color = "Black") + 
          geom_text(data = discount_knn, aes(y = pred1, label = paste("Your House: ", as.character(round(pred1/1e6, digits = 2)), "M", sep = '')), vjust = 4, color = "Red") + 
          theme(axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                title = element_text(size = 9, face = 'bold')) + 
          geom_hline(yintercept = pred1, color = "red") + 
          ylim(min(min(discount_knn$Price_MN), pred1) - 0.1*(max(max(discount_knn$Price_MN), pred1) - min(min(discount_knn$Price_MN), pred1)), max(max(discount_knn$Price_MN), pred1) + 0.1*(max(max(discount_knn$Price_MN), pred1) - min(min(discount_knn$Price_MN), pred1)))
      })
      
      shinyjs::show("amenities")
      shinyjs::show("knn")
      
      ##### Output the box containing the estimated prices
      output$vbox = renderValueBox({
        valueBox(
          value = tags$p("The value of your house is", style = "font-size: 50%;"),
          subtitle = tags$b(pred, style = "font-size: 300%;"),
          icon = icon("home"),
          width = 6,
          color = "green"
        )
      })
    })
    
    
    ## Output the box containing the amenities
    observeEvent(input$amenities, {
      input$amenities
      output$an <- DT::renderDataTable({
        DT::datatable(places_nearby_new)
      })
      shinyjs::hide("amenities")
      shinyjs::show("close")
    })
    
    observeEvent(input$close, {
      input$close
      output$an <- DT::renderDataTable({
        
      })
      shinyjs::show("amenities")
      shinyjs::hide("close")
    })
    
    observeEvent(input$knn, {
      input$knn
      output$tb <- DT::renderDataTable({
        # k <- as.integer(input$k2)
        ###### This is all we need
        cols_ = colnames(data)[!grepl('norm', colnames(data))]
        df_knn <- data[index_knn[1:k], cols_]
        ## Need to revise with the correct data
        # df_knnpred1$url <- sprintf(paste('<a href="', df_knnpred1$url, '"></a>', sep = ""))
        # df_knn_display$url <- a(href = df_knn_display$url[1])
        df_knn$url <- paste0("<a href='", df_knn$url,"' target='_blank'>", df_knn$url,"</a>")
        #df_knn['Distance'] <- 111*sqrt((as.numeric(df_knn$latitude) - as.numeric(places_vec[1]))^2 + (as.numeric(df_knn$longitude) - as.numeric(places_vec[2]))^2)
        lon_lat = rev(places_vec)
        dist_h = distm(lon_lat, df_knn[,c("longitude", "latitude")], fun = distHaversine)
        df_knn['Distance'] = dist_h[1,]
        df_knn$Distance1 <- paste(round(df_knn$Distance/1e3, 2), "km", sep = "")
        # df_knn$url <- paste0("<a href='", df_knn$url,"'>", df_knn$url,"</a>")
        # df_knn$url <- createLink(df_knn$url)
        df_knn_display <- df_knn %>% 
          arrange(Distance) %>% 
          select(address, city, Price_MN, Total.area, Builded.surface, Bedrooms, 
                 Bathrooms, mape_test, Distance1, url)
        colnames(df_knn_display) <- c("Address", "city", "Price", "Total Area", 
                                      "Builded Surface", "Bedrooms", "Bathrooms", 
                                      "Percentage_Error", "Distance", "URL")
        #round the features 
        df_knn_display$Percentage_Error = round(df_knn_display$Percentage_Error, 3)
        df_knn_display$Percentage_Error = paste(as.character(df_knn_display$Percentage_Error * 100), "%", sep = "")
        DT::datatable(df_knn_display, escape = FALSE,  options = list(lengthMenu = c(5, 10, 20)))
        #options = list(pageLength = 5)
      })
      shinyjs::hide("knn")
      shinyjs::show("close_knn")
    })
    
    observeEvent(input$close_knn, {
      input$close_knn
      output$tb <- DT::renderDataTable({
        
      })
      shinyjs::show("knn")
      shinyjs::hide("close_knn")
    })
    
  })
}

shinyApp(ui = ui, server = server)
