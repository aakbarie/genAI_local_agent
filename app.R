library(shiny)
library(shinydashboard)
library(curl)
library(jsonlite)

# Define UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Generative AI and Coding Agent"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Model Selector", tabName = "model_selector", icon = icon("sliders-h")),
      selectInput("model_choice", "Choose a Model:",
                  choices = c("Generative AI Agent" = "phi3.5:latest", 
                              "Coding Agent" = "deepseek-coder-v2:latest")),
      helpText("Select the model you want to use for generating responses."),
      actionButton("clear_button", "Start New Conversation")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-black .main-header .logo {
          background-color: #244855;
        }
        .skin-black .main-header .navbar {
          background-color: #244855;
        }
        .skin-black .main-sidebar {
          background-color: #9daead;
          color: #fff;
        }
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #e64833;
        }
        .content-wrapper, .right-side {
          background-color: #fbe9d0;
        }
        .box {
          border-top-color: #874f41;
        }
        #send_button {
          background-color: #e64833;
          color: white;
          border: none;
          border-radius: 5px;
          padding: 10px 20px;
          font-size: 16px;
        }
        #send_button:hover {
          background-color: #874f41;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "model_selector",
              fluidRow(
                box(
                  title = "Your Input",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  textAreaInput("user_input", "Enter your message:", "", rows = 3, placeholder = "Type your message here..."),
                  actionButton("send_button", "Send"),
                  helpText("Click 'Send' to generate a response from the selected model.")
                )
              ),
              
              fluidRow(
                box(
                  title = "Model Response",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("model_response")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the conversation history
  conversation_history <- reactiveVal("")
  
  # Function to call the Ollama API with selected model
  call_ollama_api <- function(model, prompt) {
    # Combine conversation history with the new prompt
    full_prompt <- paste(conversation_history(), prompt, sep = "\nUser: ")
    
    data_list <- list(
      model = model, 
      prompt = full_prompt,
      system = "You are a helpful assistant.",
      stream = FALSE,
      options = list(
        temperature = 0.7,
        num_predict = 512
      )
    )
    
    json_payload <- toJSON(data_list, auto_unbox = TRUE)
    h <- new_handle()
    handle_setopt(h, copypostfields = json_payload)
    handle_setheaders(h, "Content-Type" = "application/json", "Accept" = "application/json")
    
    response <- curl_fetch_memory("http://localhost:11434/api/generate", handle = h)
    
    if (response$status_code != 200) {
      return("Error: Failed to connect to the API.")
    }
    
    parsed_response <- fromJSON(rawToChar(response$content))
    return(trimws(parsed_response$response))
  }
  
  # Reactive expression to get the response from the selected model
  response <- eventReactive(input$send_button, {
    if (input$user_input != "") {
      # Call the API and get the response
      model_response <- call_ollama_api(input$model_choice, input$user_input)
      
      # Update the conversation history
      updated_history <- paste(conversation_history(), 
                               paste("User:", input$user_input),
                               paste("Assistant:", model_response),
                               sep = "\n")
      conversation_history(updated_history)
      
      return(model_response)
    } else {
      return("Please enter a message.")
    }
  })
  
  # Output the response
  output$model_response <- renderText({
    response()
  })
  
  # Clear conversation history when "Start New Conversation" is clicked
  observeEvent(input$clear_button, {
    conversation_history("")
    updateTextInput(session, "user_input", value = "")
    output$model_response <- renderText({ "" })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
