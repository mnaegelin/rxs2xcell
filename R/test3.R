library(shiny)
library(rhandsontable)
library(htmltools)
library(shinyjs)

# Sample data
df_tbl1 <- data.frame(
  site_label = c("A", "B", "C"),
  latitude = c(as.numeric(145.2), 46.5, NA),
  longitude = c(7.3, "R", 6.7),
  elevation = c(44, 150, "R"),
  stringsAsFactors = FALSE
)

# Custom renderer for mandatory fields
renderer_mandatory_char <- function(max_length) {
  htmlwidgets::JS(htmltools::HTML(sprintf(
    "
    function(instance, td, row, col, prop, value, cellProperties) {
      if (!window.invalidColumns) {
        window.invalidColumns = new Set();
      }

      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      let isInvalid = false;

      if (value === null || value === '') {
        td.style.background = 'pink';
        isInvalid = true;
        tippy(td, { content: 'required field' });
      } else if (value.length > %s) {
        td.style.background = 'pink';
        isInvalid = true;
        tippy(td, { content: 'too long' });
      } else {
        td.style.background = '';
      }

      if (isInvalid) {
        window.invalidColumns.add(col);
      }

      Handsontable.renderers.TextRenderer.apply(this, arguments);
      return td;
    }", max_length)))
}

# UI
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(src = "https://unpkg.com/tippy.js@6"),
    tags$script(HTML("
      function highlightInvalidHeaders(instance) {
        const headers = instance.rootElement.querySelectorAll('thead th');
        headers.forEach((th, idx) => {
          if (window.invalidColumns && window.invalidColumns.has(idx)) {
            th.style.background = 'red';
            th.style.color = 'white';
          } else {
            th.style.background = '';
            th.style.color = '';
          }
        });
      }

      function setAfterChangeHook() {
        setTimeout(function() {
          const hotInstances = Handsontable.instances;
          if (!hotInstances || hotInstances.length === 0) return;

          hotInstances.forEach(function(hot) {
            if (!hot.__afterChangeHookSet) {
              hot.addHook('afterChange', function(changes, source) {
                if (source !== 'loadData') {
                  highlightInvalidHeaders(this);
                }
              });
              hot.__afterChangeHookSet = true;
            }
          });
        }, 500);
      }

      Shiny.addCustomMessageHandler('initAfterChangeHook', function(message) {
        setAfterChangeHook();
      });
    "))
  ),

  titlePanel("Highlight Column Header if Any Cell is Invalid"),

  rHandsontableOutput("tbl1")
)

# Server
server <- function(input, output, session) {
  data_in <- reactiveValues(tbl1 = df_tbl1)

  output$tbl1 <- renderRHandsontable({
    table <- rhandsontable(data_in$tbl1, rowHeaders = NULL, contextMenu = FALSE, stretchH = 'all') %>%
      hot_col('site_label', renderer = renderer_mandatory_char(5)) %>%
      hot_col('latitude', renderer = renderer_mandatory_char(6))

    invalidateLater(1000, session)
    session$sendCustomMessage("initAfterChangeHook", list())

    table
  })
}

shinyApp(ui, server)
