## Only run examples in interactive R sessions

# shiny validate
# library(shiny)
#
# ui <- fluidPage(
#   checkboxGroupInput('in1', 'Check some letters', choices = head(LETTERS)),
#   selectizeInput('in2', 'Select a state', choices = state.name),
#   plotOutput('plot')
# )
#
# server <- function(input, output) {
#   output$plot <- renderPlot({
#     validate(
#       need(input$in1, 'Check at least one letter!'),
#       need(input$in2 != '', 'Please choose a state.')
#     )
#     plot(1:10, main = paste(c(input$in1, input$in2), collapse = ', '))
#   })
# }
#
# shinyApp(ui, server)

# download button
# library(shiny)
# library(shinyjs)
#
# ui <- fluidPage(
#   useShinyjs(),  # Initialize shinyjs
#   actionButton("generate_data", "Generate Data"),
#   downloadButton("download_data", "Download Data", disabled = TRUE)  # Initially disabled
# )
#
# server <- function(input, output, session) {
#   input_data <- reactiveValues(df_rings = NULL)
#
#   observeEvent(input$generate_data, {
#     # Example of generating some data and assigning it to input_data$df_rings
#     input_data$df_rings <- data.frame(x = rnorm(100), y = rnorm(100))
#   })
#
#   # observe({
#   #   if (!is.null(input_data$df_rings)) {
#   #     enable("download_data")  # Enable the download button
#   #   } else {
#   #     disable("download_data")  # Disable the download button
#   #   }
#   # })
#
#   output$download_data <- downloadHandler(
#     filename = function() {
#       paste("data-", Sys.Date(), ".csv", sep = "")
#     },
#     content = function(file) {
#       write.csv(c(1), file)
#     }
#   )
# }
#
# shinyApp(ui, server)

# link_shiny <- tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")
# link_posit <- tags$a(shiny::icon("r-project"), "Posit", href = "https://posit.co", target = "_blank")
#
# ui <- page_navbar(
#   title = "My App",
#   navbar_options = navbar_options(
#     bg = "#0062cc",
#     underline = TRUE
#   ),
#   nav_panel(title = "One", p("First tab content.")),
#   nav_panel(title = "Two", p("Second tab content.")),
#   nav_panel(title = "Three", p("Third tab content")),
#   nav_spacer(),
#   nav_menu(
#     title = "Links",
#     align = "right",
#     nav_item(link_shiny),
#     nav_item(link_posit)
#   )
# )
#
# server <- function(input, output, session) {
#
# }
# shinyApp(ui, server)

# library(shiny)
# library(bslib)
#
# # UI
# ui <- page_fluid(
#   theme = bs_theme(),
#
#   tags$head(
#     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
#     tags$script(src = "script.js")
#   ),
#
#   div(class = "nav-container",
#       tags$button(icon("chevron-left"), class = "scroll-button scroll-left"),
#
#       navset_underline(
#         nav_panel("Very Long Name 1", "Content Tab 1"),
#         nav_panel("Very Long Name 2", "Content Tab 2"),
#         nav_panel("Very Long Name 3", "Content Tab 3"),
#         nav_panel("Very Long Name 4", "Content Tab 4"),
#         nav_panel("Very Long Name 5", "Content Tab 5"),
#         nav_panel("Very Long Name 6", "Content Tab 6"),
#         nav_panel("Very Long Name 7", "Content Tab 7"),
#         nav_panel("Very Long Name 8", "Content Tab 8"),
#         nav_panel("Very Long Name 9", "Content Tab 9")
#       ),
#
#       tags$button(icon("chevron-right"), class = "scroll-button scroll-right")
#   )
# )
#
# # Server
# server <- function(input, output, session) {
# }
#
# shinyApp(ui = ui, server = server)


# library(shiny)
# library(bslib)
#
# # Define UI for the application
# ui <- fluidPage(
#   theme = bs_theme(version = 4, bootswatch = "flatly"),
#
#   navbarPage(
#     title = "Shiny App with Sidebar",
#     id = "navbar",
#
#     tabPanel("Home",
#              sidebarLayout(
#                sidebarPanel(
#                  h3("Navigation"),
#                  navlistPanel(
#                    id = "sidebar",
#                    tabPanel("Home", value = "tab_home"),
#                    tabPanel("Page 1", value = "tab_page1"),
#                    tabPanel("Page 2", value = "tab_page2")
#                  )
#                ),
#                mainPanel(
#                  h3("Welcome to the Home Page")
#                )
#              )
#     ),
#
#     tabPanel("Page 1",
#              sidebarLayout(
#                sidebarPanel(
#                  h3("Navigation"),
#                  navlistPanel(
#                    id = "sidebar",
#                    tabPanel("Home", value = "tab_home"),
#                    tabPanel("Page 1", value = "tab_page1"),
#                    tabPanel("Page 2", value = "tab_page2")
#                  )
#                ),
#                mainPanel(
#                  h3("Welcome to Page 1")
#                )
#              )
#     ),
#
#     tabPanel("Page 2",
#              sidebarLayout(
#                sidebarPanel(
#                  h3("Navigation"),
#                  navlistPanel(
#                    id = "sidebar",
#                    tabPanel("Home", value = "tab_home"),
#                    tabPanel("Page 1", value = "tab_page1"),
#                    tabPanel("Page 2", value = "tab_page2")
#                  )
#                ),
#                mainPanel(
#                  h3("Welcome to Page 2")
#                )
#              )
#     )
#   )
# )
#
# server <- function(input, output, session) {
#   observeEvent(input$sidebar, {
#     updateNavbarPage(session, "navbar", selected = input$sidebar)
#   })
# }
#
# # Run the application
# shinyApp(ui = ui, server = server)
# library(shiny)
#
# create_sidebar_link <- \(
#   id,
#   label,
#   class = "link-body-emphasis d-inline-flex text-decoration-none rounded w-100",
#   active = FALSE
# ) {
#   if (active) {
#     class <- paste(class, "active")
#   }
#
#   tagList(
#     tags$li(
#       actionLink(
#         inputId = id,
#         label = label,
#         class = class
#       )
#     )
#   )
# }
#
# create_sidebar_menu_header <- \(
#   title,
#   title_class = "ps-2",
#   data_bs_target,
#   data_bs_toggle = "collapse",
#   class = "btn btn-toggle d-inline-flex align-items-center rounded border-0 collapsed w-100",
#   aria_expanded = "false"
# ) {
#   tagList(
#     tags$button(
#       class = class,
#       `data-bs-toggle` = data_bs_toggle,
#       `data-bs-target` = data_bs_target,
#       `aria-expanded` = aria_expanded,
#       tags$span(
#         class = title_class,
#         title
#       )
#     )
#   )
# }
#
# ui <- tags$html(
#   lang = "en",
#   `data-bs-theme` = "auto",
#   tags$head(
#     tags$meta(charset = "utf-8"),
#     tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
#     tags$title("Sidebar Demo"),
#     # bootstrap css:
#     tags$link(
#       href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css",
#       rel = "stylesheet",
#       integrity = "sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM",
#       crossorigin = "anonymous"
#     ),
#     # styles.css:
#     tags$link(
#       href = "styles.css",
#       rel = "stylesheet"
#     ),
#     # google fonts:
#     tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
#     tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
#     tags$link(
#       href = "https://fonts.googleapis.com/css2?family=Quicksand:wght@300;400;500;600;700&display=swap",
#       rel = "stylesheet"
#     )
#   ),
#   tags$body(
#     class = "bg-light",
#     bootstrapLib(theme = bslib::bs_theme(version = 5)),
#     suppressDependencies("bootstrap"),
#     tags$div(
#       class = "d-flex vh-100",
#       # sidebar
#       tags$div(
#         class = "flex-shrink-0 p-3 bg-white border-end shadow-sm",
#         style = "width: 280px;",
#         tags$a(
#           href = "https://shiny.posit.co/",
#           class = paste(
#             "d-flex align-items-center pb-3 mb-3 link-body-emphasis",
#             "text-decoration-none border-bottom"
#           ),
#           # tags$img(
#           #   src = "shiny-solo.png",
#           #   alt = "Shiny Logo",
#           #   width = 50,
#           #   height = 25
#           # ),
#           tags$span(
#             class = "fs-5 fw-semibold ps-2",
#             "Sidebar Showcase"
#           )
#         ),
#         tags$ul(
#           class = "list-unstyled ps-0",
#           tags$li(
#             class = "mb-1",
#             create_sidebar_menu_header(
#               title = "Home",
#               data_bs_target = "#home-collapse",
#               aria_expanded = "true"
#             ),
#             tags$div(
#               class = "collapse show",
#               id = "home-collapse",
#               tags$ul(
#                 class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
#                 create_sidebar_link(id = "overview", label = "Overview", active = TRUE),
#                 create_sidebar_link(id = "updates", label = "Updates"),
#                 create_sidebar_link(id = "reports", label = "Reports")
#               )
#             )
#           ),
#           tags$li(
#             class = "mb-1",
#             create_sidebar_menu_header(
#               title = "Dashboard",
#               data_bs_target = "#dashboard_collapse"
#             ),
#             tags$div(
#               class = "collapse",
#               id = "dashboard_collapse",
#               tags$ul(
#                 class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
#                 create_sidebar_link(id = "weekly", label = "Weekly"),
#                 create_sidebar_link(id = "monthly", label = "Monthly"),
#                 create_sidebar_link(id = "annually", label = "Annually")
#               )
#             )
#           ),
#           tags$li(
#             class = "mb-1",
#             create_sidebar_menu_header(
#               title = "Orders",
#               data_bs_target = "#orders_collapse"
#             ),
#             tags$div(
#               class = "collapse",
#               id = "orders_collapse",
#               tags$ul(
#                 class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
#                 create_sidebar_link(id = "new_orders", label = "New"),
#                 create_sidebar_link(id = "processed_orders", label = "Processed"),
#                 create_sidebar_link(id = "shipped_orders", label = "Shipped"),
#                 create_sidebar_link(id = "returned_orders", label = "Returned")
#               )
#             )
#           ),
#           tags$li(class = "border-top my-3"),
#           tags$li(
#             class = "mb-1",
#             create_sidebar_menu_header(
#               title = "Account",
#               data_bs_target = "#account_collapse"
#             ),
#             tags$div(
#               class = "collapse",
#               id = "account_collapse",
#               tags$ul(
#                 class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
#                 create_sidebar_link(id = "new_account", label = "New..."),
#                 create_sidebar_link(id = "profile", label = "Profile"),
#                 create_sidebar_link(id = "account_settings", label = "Settings"),
#                 create_sidebar_link(id = "sign_out", label = "Sign Out")
#               )
#             )
#           ),
#         )
#       ),
#       # main
#       tags$div(
#         class = "p-3",
#         tabsetPanel(
#           id = "tabs",
#           type = "hidden",
#           tabPanelBody(
#             value = "overview",
#             tags$h3("Overview")
#           ),
#           tabPanelBody(
#             value = "updates",
#             tags$h3("Updates")
#           ),
#           tabPanelBody(
#             value = "reports",
#             tags$h3("Reports")
#           ),
#           tabPanelBody(
#             value = "weekly",
#             tags$h3("Weekly Dashboard")
#           ),
#           tabPanelBody(
#             value = "monthly",
#             tags$h3("Monthly Dashboard Summary")
#           ),
#           tabPanelBody(
#             value = "annually",
#             tags$h3("Annual Dashboard Analytics")
#           ),
#           tabPanelBody(
#             value = "new_orders",
#             tags$h3("New Orders")
#           ),
#           tabPanelBody(
#             value = "processed_orders",
#             tags$h3("Processed Orders")
#           ),
#           tabPanelBody(
#             value = "shipped_orders",
#             tags$h3("Here are the shipped orders")
#           ),
#           tabPanelBody(
#             value = "returned_orders",
#             tags$h3("Returned orders here")
#           ),
#           tabPanelBody(
#             value = "new_account",
#             tags$h3("Create New Account")
#           ),
#           tabPanelBody(
#             value = "profile",
#             tags$h3("View your profile")
#           ),
#           tabPanelBody(
#             value = "account_settings",
#             tags$h3("Your account settings")
#           ),
#           tabPanelBody(
#             value = "sign_out",
#             tags$h3("You're now signed out")
#           )
#         )
#       )
#     ),
#     # bootstrap js:
#     tags$script(
#       src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js",
#       integrity = "sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz",
#       crossorigin = "anonymous"
#     ),
#     # script.js:
#     tags$script(src = "script.js")
#   )
# )
#
# server <- function(input, output, session) {
#   sidebar_link_ids <- c(
#     "overview", "updates", "reports",
#     "weekly", "monthly", "annually",
#     "new_orders", "processed_orders", "shipped_orders", "returned_orders",
#     "new_account", "profile", "account_settings", "sign_out"
#   )
#   # add observers to switch to the clicked link's tab:
#   lapply(sidebar_link_ids, \(id) {
#     observeEvent(input[[id]], {
#       freezeReactiveValue(input, "tabs")
#       updateTabsetPanel(session = session, inputId = "tabs", selected = id)
#     })
#   })
# }
#
# shinyApp(ui, server)


#' library(shiny)
#' library(shinyTree)
#' library(shinyjs)
#'
#' #' Define UI for application that demonstrates a simple Tree editor
#' #' @author Jeff Allen \email{jeff@@trestletech.com}
#' ui <- shinyUI(
#'   pageWithSidebar(
#'     # Application title
#'     headerPanel("shinyTree with checkbox controls"),
#'
#'     sidebarPanel(
#'       helpText(HTML("An example of a shinyTree with the <code>checkbox</code> parameter enabled to allow users to more easily make multiple selections in the tree.
#'                   <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
#'     ),
#'     mainPanel(
#'       # Show a simple table.
#'       shinyTree("tree123", checkbox = TRUE, tie_selection = TRUE, whole_node = FALSE, three_state = TRUE, contextmenu = FALSE),
#'       verbatimTextOutput("sel_names"),
#'       verbatimTextOutput("sel_slices"),
#'       verbatimTextOutput("sel_classid")
#'     ))
#' )
#' server <- shinyServer(function(input, output, session) {
#'   log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
#'
#'
#'   print_tree <- function(tree) {
#'     if (is(tree, "Node")) {
#'       do.call(print, c(x = tree, as.list(tree$fieldsAll)))
#'     } else {
#'       str(tree)
#'     }
#'   }
#'
#'   output$tree123 <- renderTree({
#'     nested_list <- list(
#'       root1 = structure(list(leaf0=""), stselected = TRUE, stopened = TRUE),
#'       root2 = structure(list(
#'         SubListA = structure(list(leaf1 = "", leaf2 = "", leaf3=""), stselected = TRUE, stopened = TRUE),
#'         SubListB = structure(list(leafA = "", leafB = ""))), stopened = TRUE)
#'     )
#'     # shinyTree::treeToJSON(dtree, pretty = TRUE)
#'   })
#'
#'   output$sel_names <- renderPrint({
#'     tree <- input$tree123
#'     req(tree)
#'     get_selected(tree)
#'   })
#'
#'   output$sel_slices <- renderPrint({
#'     tree <- input$tree123
#'     str(tree)
#'   })
#'   output$sel_classid <- renderPrint({
#'     tree <- input$tree123
#'     req(tree)
#'     get_selected(tree, format = "classid")
#'   })
#' })
#'
#' shinyApp(ui, server)

# library(shiny)
# library(networkD3)
# library(htmlwidgets)
#
# URL <- "https://raw.githubusercontent.com/christophergandrud/networkD3/master/JSONdata/flare.json"
# Flare <- jsonlite::fromJSON(URL, simplifyDataFrame = FALSE)
#
# # Modified JavaScript code to send the selected node to Shiny
# clickJS <- '
# d3.selectAll(".node").on("click", function(d){
#   Shiny.onInputChange("selected_node", d.data.name);
# })
# '
#
# server <- function(input, output) {
#   output$simple <- renderDiagonalNetwork({
#     onRender(diagonalNetwork(List = Flare, fontSize = 10, opacity = 0.9), clickJS)
#   })
#
#   # Display the selected node in verbatimTextOutput
#   output$selectedNode <- renderText({
#     input$selected_node
#   })
# }
#
# ui <- fluidPage(
#   diagonalNetworkOutput("simple"),
#   verbatimTextOutput("selectedNode")
# )
#
# shinyApp(ui = ui, server = server)


# # # editbl example ----
# library(shiny)
# library(editbl)
#
# # hover: https://stackoverflow.com/questions/74792847/hover-tooltip-popover-for-dynamic-column-headers-in-dt-datatable-for-shiny-app
# # https://www.r-bloggers.com/2020/02/tooltips-for-the-headers-of-a-datatable-in-shiny/
#
# # not_default_row <- function(row){
# #   if (!is.na(row[,'first_name']) & row[,'first_name'] == "First Name") {
# #     FALSE
# #   } else {
# #     TRUE
# #   }
# # }
#
# # JavaScript code
# js_code <- "
# Shiny.addCustomMessageHandler('colDisplayText', function(colDisplayText) {
#   var table = $('#example').DataTable();
#
#   $('#example').on('mousemove', 'th', function(e) {
#      var headerText = $(this).text();
#      var displayText = colDisplayText[headerText] || headerText;
#      $('#tooltip').text(displayText).animate({ left: e.pageX, top: e.pageY }, 1);
#      if (!$('#tooltip').is(':visible')) $('#tooltip').show();
#   });
#
#   $('#example').on('mouseleave', 'th', function(e) {
#     $('#tooltip').hide();
#   });
# });
# "
#
# col_display_text <- list(
#   first_name = "First Name Display",
#   last_name = "Last Name Display"
# )
#
# col_display_text_json <- jsonlite::toJSON(col_display_text, auto_unbox = TRUE)
#
# default_data <- dplyr::as_tibble(data.frame(
#   first_name = NA_character_,
#   last_name = NA_character_,
#   email = NA_character_,
#   orcid = NA_character_,
#   org_name = NA_character_,
#   rorid = NA_character_,
#   department = NA_character_,
#   address = NA_character_,
#   stringsAsFactors = FALSE
# ))
#
# ui <- fluidPage(
#   eDTOutput('data'),
#   DT::dataTableOutput('data_table'),
#   tags$div(id = "tooltip", style = "position: absolute; display: none; background: lightgray; padding: 5px; border-radius: 5px;")
# )
#
# ui <- tagList(
#   ui,
#   tags$script(HTML(js_code))
# )
#
# server <- function(input, output, session) {
#   modifiedData <- eDT(
#     id = 'data',
#     data = default_data,
#     # canEditRow = not_default_row,
#     # canDeleteRow = not_default_row,
#     )
#
#   observe({
#     # Data when 'save' is clicked
#     print('result')
#     print(modifiedData$result())
#   })
#
#   observe({
#     # Data as it's being modified
#     print('state')
#     print(modifiedData$state())
#   })
#
#   output$data_table <- DT::renderDT({
#     DT::datatable(default_data,
#                   rownames = FALSE,
#                   options = list(ordering =F))
#   })
#   # Pass the named vector to JavaScript
#   session$sendCustomMessage(type = 'colDisplayText', col_display_text)
#
# }
#
# shinyApp(ui, server)

## rhandsontable example --------
# library(rhandsontable)
# library(shiny)
#
# DF <- data.frame(Value = 1:10, Status = TRUE, Name = LETTERS[1:10],
#                  Date = seq(from = Sys.Date(), by = "days", length.out = 10),
#                  stringsAsFactors = FALSE)
# outdir <- getwd()
# outfilename <- "table"
#
# ui <- shinyUI(fluidPage(
#
#   titlePanel("Edit and save a table"),
#   sidebarLayout(
#     sidebarPanel(
#       helpText("Shiny app based on an example given in the rhandsontable package.",
#                "Right-click on the table to delete/insert rows.",
#                "Double-click on a cell to edit"),
#
#       wellPanel(
#         h3("Table options"),
#         radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
#       ),
#       br(),
#
#       wellPanel(
#         h3("Save table"),
#         div(class='row',
#             div(class="col-sm-6",
#                 actionButton("save", "Save")),
#             div(class="col-sm-6",
#                 radioButtons("fileType", "File type", c("ASCII", "RDS")))
#         )
#       )
#
#     ),
#
#     mainPanel(
#       wellPanel(
#         uiOutput("message", inline=TRUE)
#       ),
#
#       actionButton("cancel", "Cancel last action"),
#       br(), br(),
#
#       rHandsontableOutput("hot"),
#       br(),
#
#       wellPanel(
#         h3("Add a column"),
#         div(class='row',
#             div(class="col-sm-5",
#                 uiOutput("ui_newcolname"),
#                 actionButton("addcolumn", "Add")),
#             div(class="col-sm-4",
#                 radioButtons("newcolumntype", "Type", c("integer", "double", "character"))),
#             div(class="col-sm-3")
#         )
#       )
#
#     )
#   )
# ))
#
# server <- shinyServer(function(input, output) {
#
#   values <- reactiveValues()
#
#   ## Handsontable
#   observe({
#     if (!is.null(input$hot)) {
#       values[["previous"]] <- isolate(values[["DF"]])
#       DF = hot_to_r(input$hot)
#     } else {
#       if (is.null(values[["DF"]]))
#         DF <- DF
#       else
#         DF <- values[["DF"]]
#     }
#     values[["DF"]] <- DF
#   })
#
#   output$hot <- renderRHandsontable({
#     DF <- values[["DF"]]
#     if (!is.null(DF))
#       rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
#   })
#
#   ## Save
#   observeEvent(input$save, {
#     fileType <- isolate(input$fileType)
#     finalDF <- isolate(values[["DF"]])
#     if(fileType == "ASCII"){
#       dput(finalDF, file=file.path(outdir, sprintf("%s.txt", outfilename)))
#     }
#     else{
#       saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
#     }
#   }
#   )
#
#   ## Cancel last action
#   observeEvent(input$cancel, {
#     if(!is.null(isolate(values[["previous"]]))) values[["DF"]] <- isolate(values[["previous"]])
#   })
#
#   ## Add column
#   output$ui_newcolname <- renderUI({
#     textInput("newcolumnname", "Name", sprintf("newcol%s", 1+ncol(values[["DF"]])))
#   })
#   observeEvent(input$addcolumn, {
#     DF <- isolate(values[["DF"]])
#     values[["previous"]] <- DF
#     newcolumn <- eval(parse(text=sprintf('%s(nrow(DF))', isolate(input$newcolumntype))))
#     values[["DF"]] <- setNames(cbind(DF, newcolumn, stringsAsFactors=FALSE), c(names(DF), isolate(input$newcolumnname)))
#   })
#
#   ## Message
#   output$message <- renderUI({
#     if(input$save==0){
#       helpText(sprintf("This table will be saved in folder \"%s\" once you press the Save button.", outdir))
#     }else{
#       outfile <- ifelse(isolate(input$fileType)=="ASCII", "table.txt", "table.rds")
#       fun <- ifelse(isolate(input$fileType)=="ASCII", "dget", "readRDS")
#       list(helpText(sprintf("File saved: \"%s\".", file.path(outdir, outfile))),
#            helpText(sprintf("Type %s(\"%s\") to get it.", fun, outfile)))
#     }
#   })
#
# })
#
# ## run app
# runApp(list(ui=ui, server=server))

# # DT example ----------------
# library(shiny)
# library(DT)
#
# # UI
# ui <- fluidPage(
#   titlePanel("Author Information Submission"),
#   sidebarLayout(
#     sidebarPanel(
#       actionButton("add_row", "Add Row"),
#       actionButton("delete_row", "Delete Row"),
#       actionButton("save_data", "Save Data")
#     ),
#     mainPanel(
#       dataTableOutput("author_table")
#     )
#   )
# )
#
# # Server
# server <- function(input, output, session) {
#   # Initialize data frame
#   author_data <- reactiveVal(data.frame(
#     firstname = character(1),
#     lastname = character(1),
#     university = character(1),
#     email = character(1),
#     orcid = character(1),
#     contactperson = logical(1),
#     stringsAsFactors = FALSE
#   ))
#
#   # Render editable table
#   output$author_table <- renderDataTable({
#     datatable(author_data(), editable = TRUE, extensions = 'AutoFill', options = list(
#       autoFill = TRUE
#     ))
#   }, server = FALSE)
#
#   # Observe add row button
#   observeEvent(input$add_row, {
#     new_row <- data.frame(
#       firstname = "", lastname = "", university = "", email = "",
#       orcid = "", contactperson = FALSE, stringsAsFactors = FALSE
#     )
#     author_data(rbind(author_data(), new_row))
#   })
#
#   # Observe delete row button
#   observeEvent(input$delete_row, {
#     req(nrow(author_data()) > 1)
#     author_data(author_data()[-nrow(author_data()), ])
#   })
#
#   # Update data frame on cell edit
#   observeEvent(input$author_table_cell_edit, {
#     info <- input$author_table_cell_edit
#     str(info)
#     temp_data <- author_data()
#     temp_data[info$row, info$col] <- info$value
#     author_data(temp_data)
#   })
#
#   # Email validation function
#   validateEmail <- function(email) {
#     grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)
#   }
#
#   # Observe save button
#   observeEvent(input$save_data, {
#     data <- author_data()
#
#     # Validation checks
#     if (any(!sapply(data$firstname, is.character)) || any(!sapply(data$lastname, is.character))) {
#       showModal(modalDialog(
#         title = "Validation Error",
#         "First name and last name must be characters.",
#         easyClose = TRUE,
#         footer = NULL
#       ))
#       return(NULL)
#     }
#
#     if (any(!validateEmail(data$email))) {
#       showModal(modalDialog(
#         title = "Validation Error",
#         "Invalid email addresses.",
#         easyClose = TRUE,
#         footer = NULL
#       ))
#       return(NULL)
#     }
#
#     if (sum(data$contactperson) != 1) {
#       showModal(modalDialog(
#         title = "Validation Error",
#         "Exactly one contact person must be selected.",
#         easyClose = TRUE,
#         footer = NULL
#       ))
#       return(NULL)
#     }
#
#     # Save to CSV
#     write.csv(data, file = "author_data.csv", row.names = FALSE)
#     showModal(modalDialog(
#       title = "Success",
#       "Data saved successfully!",
#       easyClose = TRUE,
#       footer = NULL
#     ))
#   })
# }
#
# # Run the application
# shinyApp(ui = ui, server = server)

library(shiny)
library(rhandsontable)

# UI
ui <- fluidPage(
  titlePanel("Author Information Submission"),
  sidebarLayout(
    sidebarPanel(
      actionButton("add_row", "Add Row"),
      actionButton("delete_row", "Delete Row"),
      actionButton("save_data", "Save Data")
    ),
    mainPanel(
      rHandsontableOutput("author_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize data frame
  author_data <- reactiveVal(data.frame(
    firstname = character(1),
    lastname = character(1),
    university = character(1),
    email = character(1),
    orcid = character(1),
    contactperson = logical(1),
    stringsAsFactors = FALSE
  ))

  # Render editable table
  output$author_table <- renderRHandsontable({
    rhandsontable(author_data(), rowHeaders = NULL) %>%
      hot_col("contactperson", type = "checkbox")
  })

  # Observe add row button
  observeEvent(input$add_row, {
    new_row <- data.frame(
      firstname = "", lastname = "", university = "", email = "",
      orcid = "", contactperson = FALSE, stringsAsFactors = FALSE
    )
    author_data(rbind(author_data(), new_row))
  })

  # Observe delete row button
  observeEvent(input$delete_row, {
    req(nrow(author_data()) > 1)
    author_data(author_data()[-nrow(author_data()), ])
  })

  # Update data frame on table edit
  observe({
    if (!is.null(input$author_table)) {
      updated_data <- hot_to_r(input$author_table)
      author_data(updated_data)
    }
  })

  # Email validation function
  validateEmail <- function(email) {
    grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)
  }

  # Observe save button
  observeEvent(input$save_data, {
    data <- author_data()

    # Validation checks
    if (any(!sapply(data$firstname, is.character)) || any(!sapply(data$lastname, is.character))) {
      showModal(modalDialog(
        title = "Validation Error",
        "First name and last name must be characters.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }

    if (any(!validateEmail(data$email))) {
      showModal(modalDialog(
        title = "Validation Error",
        "Invalid email addresses.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }

    if (sum(data$contactperson) != 1) {
      showModal(modalDialog(
        title = "Validation Error",
        "Exactly one contact person must be selected.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }

    # Save to CSV
    write.csv(data, file = "author_data.csv", row.names = FALSE)
    showModal(modalDialog(
      title = "Success",
      "Data saved successfully!",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
