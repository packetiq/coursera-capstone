library(shiny)
shinyUI(   fluidPage(
    headerPanel("Next Word Predictor Application"
    ),
    sidebarPanel(
        h3("Introducton"),
        p("This application predicts the next most likely word in a phrase or sentence. To see it in action, type a word(s) in the text field and up to 4 possible 'next words' will display in buttons below the field. Click on your intended match to add it to the field, or just keep typing to see additional predictions."),
        p("This application was developed for the Capstone Course of the Coursera Data Science Specialization. It uses natural language proccessing models that include the use of n-grams and selected techniques of Hidden Markov models and Katz's back-off model to produce the predictions."),
        p("Please note that to obtain acceptable performance in a demonstration setting, the size of the corpus and therefore the accuracy and utility of this application has been reduced.")
        
    ),
    mainPanel(
        h3("What do you want to say?"),
        textInput("inputTxt", "Type words below. You can click suggestion buttons or keep typing:", width = "90%"),
        uiOutput("words"),
        br(),
        wellPanel(
            h4("Technical Details"),
            HTML("<p>The source code for this application is freely available at: <a href='https://github.com/packetiq/coursera-capstone' target='_blank'>https://github.com/packetiq/coursera-capstone</a></p>"),
            HTML("<p>A detailed application architecture presentation is available at: <a href='http://rpubs.com/packetiq/data-science-capstone' target='_blank'>http://rpubs.com/packetiq/data-science-capstone</a></p>"),
            h4("Author:"),
            p("James H. Baxter")
        )
        
    )
)) 