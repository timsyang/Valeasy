ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Valeasy",
                  titleWidth = 250),
  
  dashboardSidebar(
    width = 250,
    tags$head(
      tags$style(HTML(".sidebar {height: 90vh; overflow-y: scroll;}"
      ) # close HTML
      )            # close tags$style
    ),             # close tags#Head
    sidebarMenu(
      convertMenuItem(menuItem("Dashboard", 
                               tabName = "dashboard", icon = icon("dashboard")), "Dashboard"),
      
      convertMenuItem(menuItem("Seller", icon = icon("home"), tabName = "Seller",
                               htmlOutput('location'),
                               actionButton(inputId = "Go1", label = 'Check Address', width = '210px', icon = icon('map-marker')),
                               textOutput('warning'),
                               htmlOutput('total_area'),
                               htmlOutput('builded_surface'),
                               htmlOutput('bedrooms'),
                               htmlOutput('bathrooms'),
                               htmlOutput('k'),
                               actionButton(inputId = "Go", label = 'Go', width = '210px', icon = icon('forward'))
      ), 'Seller')
      
    )),
  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Lobster", cursive;
                              font-weight: bold;
                              font-size: 31px;
                              },
                              tags$style(HTML(".sidebar {height: 90vh; overflow-y: scroll;}"
                              )
                              )
                              '))),
    tabItems(
      tabItem(tabName = "dashboard",
              h1("Welcome to valeasy"),
              h5(p("Are you planning to sell a house in Mexico? Use Valeasy!"),
                 p("Valeasy will help you find the right price to sell your house, all in a few clicks."),
                 p("Using the app, you set the size, room details, location and many other features you care about. 
                   Inside, but also outside, as you can select all your nearby amenities, such as food and university."), 
                 p("Not sure about everything yet? No problem. With Valeasy, you set your own requirements, and receive a fair price. 
                   Valeasy will find the right price for your house in the current market, reducing your dependence on brokers, 
                   making sure you never pay too much."),
                 p("Welcome to Valeasy!"))
                 ),
      
      tabItem(tabName = "Seller",
              fluidPage(box(width = 12, 
                            useShinyjs(),
                            column(12,
                                   valueBoxOutput("vbox", width = NULL))),
                        box(width = 12, 
                            useShinyjs(),
                            column(12,
                                   plotOutput("boxplot", width = "auto", height = "250px"))),
                        
                        box(width = 12,
                            useShinyjs(),
                            column(12, 
                                   #column(width = 12,
                                   #       DT::dataTableOutput("trace_table"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                   div(style = c('overflow-y: auto', 'overflow-x: scroll'), 
                                       DT::dataTableOutput("tb")),
                                   actionButton(inputId = "knn", label = 'Features for K Nearest Neighbors', width = '300px', icon = icon('house')), 
                                   actionButton(inputId = "close_knn", label = 'Close', width = '210px', icon = icon('times'))
                            )),
                        box(width = 12, 
                            useShinyjs(),
                            column(12,
                                   div(style = c('overflow-y: auto', 'overflow-x: scroll'), 
                                       DT::dataTableOutput("an")),
                                   actionButton(inputId = "amenities", label = 'Amenities Nearby', width = '300px', icon = icon('food')), 
                                   actionButton(inputId = "close", label = 'Close', width = '210px', icon = icon('times'))
                            )))
              
      )
      )
      )
    )