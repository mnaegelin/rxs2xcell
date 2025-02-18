library(shiny)
library(shinyjs)
library(bslib)


# allow to create new options in dropdown select:
# selectizeInput(
#   'e3', '3. Item creation', choices = state.name,
#   options = list(create = TRUE)
# ),

# Define UI
ui <- fluidPage(
  theme = bs_theme(primary = "#006268", secondary = "#00919A",
                   font_scale = 0.8L, preset = "cosmo"),
  useShinyjs(),  # Include shinyjs

  titlePanel('Metadata collection app for rxs2xcell'),

  navset_card_tab(
    nav_panel(
      title = 'Prefill metadata',
      textOutput("file_status"),
      fileInput("file_upload", "Load metadata from file (csv)", accept = ".csv"),
      tableOutput("prefill_meta")
      ),

  nav_panel(
    title = "General Info",
    h4("Dataset"),
    textInput("ds_name", "Dataset name",
              placeholder = "Specify a name for your dataset (max 64 char.)"),
    textAreaInput("ds_desc", "Description", rows = 4,
                  placeholder = "Describe your dataset"),

    # Author section
    h4("Authors"),
    splitLayout(cellWidths = c("70%", "15%", "15%"), cellArgs = list(style='white-space: normal;'),
      p("Please list all authors (data owners) of the dataset. The order provided
        here will be used as the order of authorship."),
      actionButton("add_author_btn", "Add author", style = "width: 100px",
                   class = "btn btn-primary"),
      actionButton("del_author_btn", "Delete author", style = "width: 100px",
                   class = "btn btn-danger")),
    h5("Author 1"),
    splitLayout(
      textInput("aut1name", NULL, "", placeholder = "Last name, first name"),
      textInput("aut1mail", NULL, "", placeholder = "name@example.com")
      ),
    # dynamic author inputs
    uiOutput("author_inputs"),

    # dynamic contact person
    h5('Contact person'),
    uiOutput('contact_person')

    ),

  nav_panel("Study Details",
                 selectInput("study_type", "Study Type", choices = c("Experimental", "Observational", "Survey")),
                 numericInput("sample_size", "Sample Size", value = 10, min = 1),
                 dateInput("study_date", "Study Date", value = Sys.Date())
        ),
  nav_panel("Locations",
                 uiOutput("dynamic_location_inputs")  # Dynamically generated inputs
        ),
  nav_panel("Preview & Save",
                 tableOutput("preview_table"),
                 actionButton("save_btn", "Save Metadata", icon = icon("save")),
                 textOutput("save_status")
            )
  )
)

# Define Server
server <- function(input, output, session) {

  # TAB: prefill metadata
  # Get already available metadata from env or file
  prefilled_meta <- reactive({
    if (is.null(input$file_upload)) {
      # check if there is df_meta available in the environment
      if (exists('df_meta')){
        return(list(df = df_meta, source = 'Using df_meta from environment'))
      } else {
        return(list(df = NULL, source = 'No metadata provided yet'))
      }
    } else { # load from provided input
      df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      return(list(df = df, source = paste('Using provided file',input$file_upload$name)))
    }
  })

  # Print source of prefilled metadata
  output$file_status <- renderText(prefilled_meta()$source)

  # Print table of prefilled metadata
  output$prefill_meta <- renderTable({
    prefilled_meta()$df
  })

  # TAB: general
  runjs("$('#ds_name').attr('maxlength', 64)")

  author_count <- reactiveVal(1)

  # Enable delete button only if there are more than 1 authors
  observeEvent(author_count(), {
    if (author_count() > 1) {
      enable("del_author_btn")
    } else {
      disable("del_author_btn")
    }
  })

  # Add author input
  observeEvent(input$add_author_btn, {
    author_count(author_count() + 1)
    author_id <- paste0('aut', author_count())
    insertUI(
      selector = "#author_inputs",
      where = "beforeBegin",
      ui = div(id = author_id,
               tagList(
                 tags$h5(paste("Author", author_count())),
                 splitLayout(
                  textInput(paste0(author_id,'name'), NULL, "", placeholder = "Last name, first name"),
                  textInput(paste0(author_id,'mail'), NULL, "", placeholder = "name@example.com")
               )))
    )
  })

  # Delete author input
  observeEvent(input$del_author_btn, {
    removeUI(selector = paste0("#aut", author_count()))
    author_count(author_count() - 1)
  })

  # Dynamic selection of contact person
  output$contact_person <- renderUI({
    radioButtons("contact_person", NULL,
                 choices = paste("Author", 1:author_count()))
  })



  # Reactive values to store uploaded data
  study_data <- reactiveVal(NULL)
  # Dynamically generate location inputs based on the number of study sites
  output$dynamic_location_inputs <- renderUI({
    df <- df_meta_pre()

    if (is.null(df)) {
      return("No study site data uploaded yet.")
    }

    num_sites <- nrow(df)

    site_inputs <- lapply(1:num_sites, function(i) {
      wellPanel(
        h4(paste("Study Site", i)),
        textInput(paste0("country_", i), "Country", df$Country[i]),
        textInput(paste0("city_", i), "City", df$City[i]),
        numericInput(paste0("latitude_", i), "Latitude", df$Latitude[i], min = -90, max = 90),
        numericInput(paste0("longitude_", i), "Longitude", df$Longitude[i], min = -180, max = 180)
      )
    })

    do.call(tagList, site_inputs)
  })

  # Collect metadata
  metadata <- reactive({
    df <- study_data()

    if (is.null(df)) {
      locations <- list(
        list(Country = "", City = "", Latitude = NA, Longitude = NA)
      )
    } else {
      locations <- lapply(1:nrow(df), function(i) {
        list(
          Country = input[[paste0("country_", i)]] %||% df$Country[i],
          City = input[[paste0("city_", i)]] %||% df$City[i],
          Latitude = input[[paste0("latitude_", i)]] %||% df$Latitude[i],
          Longitude = input[[paste0("longitude_", i)]] %||% df$Longitude[i]
        )
      })
    }

    list(
      Title = input$title,
      Author = input$author,
      Description = input$description,
      Study_Type = input$study_type,
      Sample_Size = input$sample_size,
      Study_Date = as.character(input$study_date),
      Locations = locations
    )
  })

  # Show metadata preview
  output$preview_table <- renderTable({
    data <- metadata()
    locations_df <- do.call(rbind, lapply(data$Locations, as.data.frame))
    cbind(data.frame(Title = data$Title, Author = data$Author,
                     Description = data$Description, Study_Type = data$Study_Type,
                     Sample_Size = data$Sample_Size, Study_Date = data$Study_Date),
          locations_df)
  })

  # Save metadata to file
  observeEvent(input$save_btn, {
    data <- metadata()
    locations_df <- do.call(rbind, lapply(data$Locations, as.data.frame))

    final_df <- cbind(data.frame(
      Title = data$Title,
      Author = data$Author,
      Description = data$Description,
      Study_Type = data$Study_Type,
      Sample_Size = data$Sample_Size,
      Study_Date = data$Study_Date
    ), locations_df)

    write.csv(final_df, "metadata.csv", row.names = FALSE)

    output$save_status <- renderText("Metadata saved to metadata.csv")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
