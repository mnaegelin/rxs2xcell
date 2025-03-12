library(shiny)
# library(shinysurveys)
library(shinyjs)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(reactable)
library(reactable.extras)
library(shinyvalidate) # NOTE: need remote mnaegelin/shinyvalidate@remove_rules


# allow to create new options in dropdown select:
# selectizeInput(
#   'e3', '3. Item creation', choices = state.name,
#   options = list(create = TRUE)
# ),

# searchable choose inputs (e.g. for species)
# selectInput('in4', 'Options', c(Choose='', state.name), selectize=TRUE)
# and should also be able to customize labels to show species code
# see https://shiny.posit.co/r/articles/build/selectize/

next_button <- function(btn_id) {
  card(
    class="border border-0",
    card_body(
      fillable = FALSE,
      actionButton(btn_id, 'Next', icon = icon('angle-double-right')))
  )}

author_input <- function(author_nr){
  tagList(
    span(p(paste0('Author ', author_nr)),style="font-weight:bold; color: #006268"),
    splitLayout(
      textInput(paste0('autname_', author_nr), NULL, "", placeholder = "Last name, first name"),
      textInput(paste0('autmail_', author_nr), NULL, "", placeholder = "name@example.com"),
      textInput(paste0('autaff_', author_nr), NULL, "", placeholder = "University of ABC")
    )
  )
}

max_char_limit <- function(value, limit) {
  if (nchar(value) > limit) paste0("Input exceeds ", limit, " characters.")
}


theme <- bs_theme(primary = "#006268", #, secondary = "#00919A"
                  font_scale = 0.8, bootswatch = "yeti")

reactable_theme <- reactableTheme(
  backgroundColor = "#dfe7e8"
  )


# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  # theming
  theme = theme,

  useShinyjs(),  # Include shinyjs
  reactable.extras::reactable_extras_dependency(),

  # title
  titlePanel('Metadata collection app for rxs2xcell'),

  navset_card_tab(
    id = 'tabs',

    # TAB: prefill metadata ----------------------------------------------------
    nav_panel(
      title = 'Prefill metadata',
      card(
        card_header('Source of prefilled metadata'),
        p("We rely on the output of the rxs2xcell::collect_metadata() function
           to prefill some of the metadata fields in this app. If a df_meta
           object is available in the environment, it can be used here direclty.
           Alternatively, you can browse for a saved metadata file."),
        fileInput("file_upload", "Load metadata from file (csv)", accept = ".csv"),
      ),
      card(
        card_header('Overview of available partial metadata'),
        span(textOutput("file_status"), style="font-weight:bold; color: #006268"),
        reactableOutput("partial_meta_out"),
      ),
      next_button('next_btn_meta')
      ),

    # TAB: general -------------------------------------------------------------
    nav_panel(
      title = "General Info",

      ## Dataset info
      card(
        card_header('Dataset'),
        textInput("ds_name", "Dataset name", value = NA,
                  placeholder = "Specify a name for your dataset (max 64 char.)"),
        textAreaInput("ds_desc", "Description", rows = 4,
                      placeholder = "Describe your dataset"),
      ),

      ## Author info
      card(
        card_header('Authors'),
        p("Please list all authors (data owners) of the dataset. Note that
           the order provided here will be used as the order of authorship."),
        card_body(
          fillable = FALSE,
          actionButton("add_author_btn", "Add author", style = "width: 100px",
                       class = "btn btn-primary"),
          actionButton("del_author_btn", "Delete author", style = "width: 100px",
                       class = "btn btn-danger")
        ),

        # first author (we always require at least one .author)
        author_input(1),

        # the dynamic author inputs
        uiOutput("author_inputs"),

        # dynamic contact person
        h5('Contact person'),
        uiOutput('contact_person')),

      verbatimTextOutput("check_val"),
      next_button('nex_btn_general')
      ),

    # TAB: study details -------------------------------------------------------
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


# server -----------------------------------------------------------------------
server <- function(input, output, session) {

  # TAB: prefill metadata ------------------------------------------------------
  # Get already available metadata from env or file
  prefilled_meta <- reactive({
    if (is.null(input$file_upload)) {
      # check if there is df_meta available in the environment
      if (exists('df_meta')){
        return(list(df = df_meta, source = 'Using df_meta available in R environment.'))
      } else {
        return(list(df = NULL, source = 'Please provide a metadata file.'))
      }
    } else { # load from provided input
      df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      return(list(df = df, source = paste('Using provided file',input$file_upload$name)))
    }
  })

  # Print source of prefilled metadata
  output$file_status <- renderText(prefilled_meta()$source)

  # Preprocess partial metadata
  partial_meta_out <- reactive({
    req(prefilled_meta()$df)
    df <- prefilled_meta()$df %>% select(site, woodpiece_code, image_code,
                                         software:img_height, fname_image)
    reactable(df,
              groupBy = c('site', 'woodpiece_code'),
              theme = reactable_theme)
  })

  # Print table of prefilled metadata
  output$partial_meta_out <- renderReactable({
    partial_meta_out()
  })

  # Next button
  # toggle: only enable in case we have df_meta
  observe({
    toggleState(id = "next_btn_meta", condition = !is.null(prefilled_meta()$df))
  })
  # functionality: switch to next tab
  observeEvent(input$next_btn_meta, {
    nav_select(id = 'tabs', selected = "General Info")
  })



  # TAB: general ---------------------------------------------------------------
  iv_gen <- InputValidator$new()

  iv_gen$add_rule("ds_name", sv_required())
  iv_gen$add_rule("ds_name", max_char_limit, limit = 64)

  iv_gen$add_rule("autname_1", sv_required())
  iv_gen$add_rule("autmail_1", sv_required())

  # Keep track of the number of authors
  author_count <- reactiveVal(1)

  # Enable delete button only if there are more than 1 authors
  observe({
    toggleState(id = "del_author_btn", condition = author_count() > 1)
  })

  # Add author input if button is clicked
  observeEvent(input$add_author_btn, {
    author_count(author_count() + 1)
    author_id <- paste0('aut', author_count())
    insertUI(
      selector = "#author_inputs",
      where = "beforeBegin",
      ui = div(id = author_id,
               author_input(author_count()))
    )
    iv_gen$add_rule(paste0('autname_',author_count()), sv_required())
  })

  # Delete author input
  observeEvent(input$del_author_btn, {
    removeUI(selector = paste0("#aut", author_count()))
    iv_gen$remove_rules(paste0('autname_',author_count()))
    author_count(author_count() - 1)

  })

  # Dynamic selection of contact person
  output$contact_person <- renderUI({
    radioButtons("contact_person", NULL,
                 choices = paste("Author", 1:author_count()))
  })

  iv_gen$enable()


  output$check_val <- renderPrint({
    iv_gen$validate()

  })

  # Next button
  observeEvent(input$nex_btn_general, {
    nav_select(id = 'tabs', selected = "Study Details")
  })


  # TAB: study details ---------------------------------------------------------
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
