library(tidyverse)
library(shinythemes)
library(DT)

# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
d0 = readxl::read_xlsx("data/risk_index_matrix.xlsx", sheet=1, 
                       col_types = rep("text", 11)) %>% 
  mutate(Component = paste0(Component, " (", tolower(Attribute), ")")) %>%
  pivot_longer(cols = c(OPTION_1,	OPTION_2,	OPTION_3, VALUE_1,	VALUE_2,	VALUE_3),
               names_to ="vars", values_to="vals" ) %>% 
  separate(vars, into = c("var", "no"), sep = "_") %>%
  select(-no) %>%
  pivot_wider(names_from = "var", values_from = "vals", values_fn = list) %>%
  unnest(c(OPTION, VALUE))

d1 =  readxl::read_xlsx("data/risk_index_strategies.xlsx", sheet=1, 
                        col_types = rep("text", 4))

ggplot() + 
  geom_point(aes(0,0), size=150, color="red") +
  geom_text(aes(0,0, label="low")) +
  theme_void()

getscore = function(d0, Crop, Num, Var){
  out = d0 %>% 
    filter(`Last year rotation` == Crop) %>%
    filter(Number == Num) %>%
    filter(OPTION == Var) %>% 
    pull(VALUE) %>% 
    as.numeric()
  ifelse(length(out)==0, 0, out)
}

myselectize = function(labs, v, ind){
  selectizeInput(inputId = paste0('x',ind),
                 label = labs()[ind],
                 choices = setNames(v()$vals[[ind]], v()$vars[[ind]]))
}

ui <- fluidPage(theme = shinytheme("cerulean"),
  pageWithSidebar(
    headerPanel('RLEM seasonal risk estimate (draft version)'),
    sidebarPanel(
      HTML("<i>Enter your paddock details below</i><br><br>"),
      selectInput("y0", "0. Last year's paddock", c("Crop", "Pasture")),
      uiOutput("y1"),
      uiOutput("y2"),
      uiOutput("y3"),
      uiOutput("y4"),
      uiOutput("y5"),
      uiOutput("y6"),
      uiOutput("y7"),
      uiOutput("y8"),
    ),
    mainPanel(
      shiny::plotOutput("plot"),
      tableOutput("riskstrategies")
    )
  )
)

server <- 
function(input, output, session) {
  labs = reactive({
    d0 %>% 
      filter(`Last year rotation` == input$y0) %>%
      distinct(Number, Component) %>%
      mutate(lab = paste0(Number, ". ", Component)) %>%
      arrange(lab) %>%
      pull(lab) 
  })
  
  v = reactive({
    d0 %>% 
      filter(`Last year rotation` == input$y0) %>%
      group_by(Number, Component) %>%
      summarise(vars = list(OPTION),
                vals = list(VALUE),
                .groups="drop") %>%
      arrange(Number) 
    
  })
  
  output$y1 <- renderUI({ myselectize(labs, v, 1) })
  output$y2 <- renderUI({ myselectize(labs, v, 2) })
  output$y3 <- renderUI({ myselectize(labs, v, 3) })
  output$y4 <- renderUI({ myselectize(labs, v, 4) })
  output$y5 <- renderUI({ myselectize(labs, v, 5) })
  output$y6 <- renderUI({ myselectize(labs, v, 6) })
  output$y7 <- renderUI({ myselectize(labs, v, 7) })
  output$y8 <- renderUI({if(input$y0 == "Pasture") myselectize(labs, v, 8) })
  
  risk <- reactive({
    if(!is.null(input$x1)){
 
        as.numeric(input$x1) + 
        as.numeric(input$x2) +
        as.numeric(input$x3) +
        as.numeric(input$x4) +
        as.numeric(input$x5) +
        as.numeric(input$x6) +
        as.numeric(input$x7) +
        ifelse(input$y0 == "Pasture", as.numeric(input$x8), 0)
    }

  })
  
  
  riskrating <- reactive({
    riskrating = "Low"
    riskrating = ifelse(risk()>0, "Moderate", riskrating)
    riskrating = ifelse(risk()>5, "High", riskrating)
  })
  
  output$plot = renderPlot({
    risktext = paste("Risk score =", risk(), "\n" , riskrating(), "risk")
    riskcol = c(Low = "green", Moderate = "orange", High = "red")
    ggplot() + 
      geom_point(aes(0,0), size=150, color=riskcol[riskrating()], alpha=0.5) +
      geom_text(aes(0,0, label=risktext), size=12) +
      theme_void()
    
  })
  
  output$riskstrategies = renderTable({
  
    d1 %>% 
      filter(Risk == riskrating()) %>% 
      select(Period, Action) %>% 
      mutate(Action = gsub('■', '<br>',Action))
    
    

  }, sanitize.text.function=identity)
  
}

shinyApp(ui, server)
