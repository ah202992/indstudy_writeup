library(ellmer)
library(glue)
library(tidyverse)
library(shiny)

# Load lookup table from CSV
lookup_table <- read_csv("file_lookup.csv")

# Function to get file path from lookup table
get_file_path <- function(lab, question, type) {
  path <- lookup_table |>
    filter(lab == lab, question == question, type == type) %>%
    pull(file_path)
  
  if (length(path) == 0) {
    stop("File not found in lookup table")
  }
  
  return(path)
}

# Function to read local or remote files
read_file <- function(file_path) {
  if (grepl("^https?://", file_path)) {
    return(readLines(url(file_path)) |> 
             glue_collapse() |> 
             str_remove("---.*?---"))
  } else {
    return(readLines(file_path) |>
             glue_collapse() |>
             str_remove("---.*?---"))
  }
}

# Define the prompt function with file retrieval
prompt <- function(lab, question, student_answer) {
  
  # Retrieve file paths from lookup table
  question_file <- get_file_path(lab, question, "question")
  rubric_file <- get_file_path(lab, question, "rubric")
  answer_file <- get_file_path(lab, question, "answer")
  
  # Read file contents
  question_text <- read_file(question_file)
  rubric_text <- read_file(rubric_file)
  answer_text <- read_file(answer_file)
  
  # Set up chat with OpenAI
  chat <- chat_openai(
    system_prompt = "You are a helpful course instructor teaching a course on data science with the R programming language."
  )
  
  # Generate feedback from LLM
  feedback <- chat$chat(
    glue(
      "Carefully read the following question: '{question_text}' and the rubric: '{rubric_text}'. 
      Then evaluate the student's answer: '{student_answer}' against the rubric and provide feedback."
    )
  )
  
  return(feedback)
}

# Shiny gadget UI
ui <- fluidPage(
  titlePanel("Submit Your Code for Feedback"),
  sidebarLayout(
    sidebarPanel(
      selectInput("lab", "Choose Lab", choices = 1:8),
      selectInput("question", "Choose Question", choices = 1:10),
      textAreaInput("student_code", "Submit your code here:", "", height = "300px"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      textOutput("question_text"),
      textOutput("feedback_text")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Observe when the submit button is clicked
  observeEvent(input$submit, {
    # Get the student's submitted code
    student_code <- input$student_code
    
    # Call the prompt function with the selected lab, question, and student's code
    feedback <- prompt(input$lab, input$question, student_code)
    
    # Output the feedback (from LLM)
    output$feedback_text <- renderText({
      paste("Feedback:\n", feedback)
    })
    
    # Retrieve and display the question text
    question_file <- get_file_path(input$lab, input$question, "question")
    question_text <- read_file(question_file)
    
    output$question_text <- renderText({
      paste("Question:\n", question_text)
    })
  })
  
}

# Run the Shiny gadget
shiny::runGadget(ui, server)

# Run in console
# source("gadget_feedback.R")

