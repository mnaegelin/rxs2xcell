renderer_char <- function(required = NULL, min_length = NULL, max_length = NULL, regex_pattern = NULL, unique = NULL){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  minl <- ifelse(is.null(min_length), -1, min_length)
  maxl <- ifelse(is.null(max_length), 10000, max_length)
  regp <- ifelse(is.null(regex_pattern), "", regex_pattern)
  check_regex <- ifelse(is.null(regex_pattern), "false", 'true')
  check_unique <- ifelse(is.null(unique), "false", ifelse(unique, "true", "false"))

  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';

      // check if value is empty
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      // check char length
      } else if (value.length < %s || value.length > %s) {
        isValid = false;
        message = 'invalid length';
      // check regex pattern
      } else if (%s) {
        var regex = new RegExp('%s');
        if (!regex.test(value)) {
          isValid = false;
          message = 'invalid format';
        }
      // check uniqueness
      } else if (%s) {
        var data = instance.getDataAtCol(col);
        var duplicates = data.filter(function(val, index, arr) {
          return arr.indexOf(val) !== index && val === value;
        });
        if (duplicates.length > 0) {
          isValid = false;
          message = 'duplicate values';
        }
      }

      if (!isValid) {
        // set background color and tooltip
        td.style.background = 'pink';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }

      Handsontable.renderers.TextRenderer.apply(this, arguments);

      return td;
    }", check_required, minl, maxl, check_regex, regp, check_unique)))
}

renderer_drop <- function(required = NULL, options){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  options_js <- paste0("[", paste0(sprintf("'%s'", options), collapse = ", "), "]")

  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if (td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';

      // check if value is empty
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      } else if (%s.indexOf(value) === -1) {
        // check if value is in options
        isValid = false;
        message = 'invalid choice';
      }

      if (!isValid) {
        // set background color and tooltip
        td.style.background = 'pink';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }


      Handsontable.renderers.DropdownRenderer.apply(this, arguments);

      return td;
    }", check_required, options_js)))

}

renderer_auto <- function(required = NULL, options){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  options_js <- paste0("[", paste0(sprintf("'%s'", options), collapse = ", "), "]")

  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if (td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';

      // check if value is empty
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      } else if (%s.indexOf(value) === -1) {
        // check if value is in options
        isValid = false;
        message = 'invalid choice';
      }

      if (!isValid) {
        // set background color and tooltip
        td.style.background = 'pink';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }


      Handsontable.renderers.AutocompleteRenderer.apply(this, arguments);

      return td;
    }", check_required, options_js)))

}

renderer_num <- function(required = NULL, min_val = NULL, max_val = NULL){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  check_min_val <- ifelse(is.null(min_val), "false", "true")
  minv <- ifelse(is.null(min_val), "null", min_val)
  check_max_val <- ifelse(is.null(max_val), "false", "true")
  maxv <- ifelse(is.null(max_val), "null", max_val)

  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';
      var num_value = parseFloat(value)

      // check if value is empty, and if required, set invalid
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      } else if (isNaN(num_value)) {
        // check if value is a number
        isValid = false;
        message = 'not a number';
      } else {
        // check value against min_val if provided
        if (%s && num_value < %s) {
          isValid = false;
          message = 'out of range';
        }
        // check value againts max_val if provided
        if (%s && num_value > %s) {
          isValid = false;
          message = 'out of range';
        }
      }

      if (!isValid) {
        // set background color and tooltip
        td.style.background = 'pink';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }

      Handsontable.renderers.NumericRenderer.apply(this, arguments);

      return td;
    }", check_required, check_min_val, minv, check_max_val, maxv)))
}

renderer_check <- function(required = NULL, min_checks = NULL, max_checks = NULL){
  # if required, we need at least one checked box
  mincb <- ifelse(is.null(required), 0, ifelse(required, 1, 0))
  # if min_checks is given and > 1, we update mincb
  mincb <- ifelse(is.null(min_checks), mincb, ifelse(min_checks > 1, min_checks, mincb))
  maxcb <- ifelse(is.null(max_checks), 10000, max_checks)

  htmlwidgets::JS(htmltools::HTML(sprintf("
  function(instance, td, row, col, prop, value, cellProperties) {
    // remove old tippy if necessary
        if(td.hasOwnProperty('_tippy')) {
          td._tippy.destroy();
        }

    var isValid = true;
    var message = '';

    // find how many boxes are checked
    var data = instance.getDataAtCol(col);
    var true_count = data.filter(function(val) { return val === true; }).length;
    console.log(true_count);

    if (true_count < %s) {
      isValid = false;
      message = 'too few checked';
    } else if (true_count > %s) {
      isValid = false;
      message = 'too many checked';
    }

    if (!isValid) {
      // set background color and tooltip
      td.style.background = 'pink';
      tippy(td, { content: message });
    } else {
      td.style.background = '';
    }

    Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
    return td;
  }", mincb, maxcb)))
}


renderer_date <- function(required = NULL){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))

  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';
      // expect YYYY-MM-DD format
      var dateRegex = /^\\d{4}-\\d{2}-\\d{2}$/;

      // check if value is empty, and if required, set invalid
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      } else if (!dateRegex.test(value) || isNaN(Date.parse(value))) {
        isValid = false;
        message = 'invalid date';
      }

      if (!isValid) {
          // set background color and tooltip
          td.style.background = 'pink';
          tippy(td, { content: message });
      } else {
        td.style.background = '';
      }

      Handsontable.renderers.DateRenderer.apply(this, arguments);
      return td;
      }", check_required)))
}

hot_col_wrapper <- function(ht, col, col_config) {
  readOnly <- ifelse(is.null(col_config$readOnly), FALSE, col_config$readOnly)
  # for char cols:
  if (col_config$type == 'character'){
    renderer_js <- renderer_char(
      required = col_config$required,
      min_length = col_config$min_length,
      max_length = col_config$max_length,
      regex_pattern = col_config$regex_pattern,
      unique = col_config$unique
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        renderer = renderer_js,
        readOnly = readOnly
      )
    # for numeric cols:
  } else if (col_config$type == 'numeric') {
    renderer_js <- renderer_num(
      required = col_config$required,
      min_val = col_config$min_val,
      max_val = col_config$max_val
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'numeric',
        renderer = renderer_js,
        readOnly = readOnly
      )
    # for dropdown cols:
  } else if (col_config$type == 'dropdown') {
    renderer_js <- renderer_drop(
      required = col_config$required,
      options = col_config$options
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'dropdown',
        source = col_config$options,
        renderer = renderer_js,
        readOnly = readOnly
      )
  # or autocomplete
  } else if (col_config$type == 'autocomplete') {
    renderer_js <- renderer_auto(
      required = col_config$required,
      options = col_config$options
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'autocomplete',
        source = col_config$options,
        renderer = renderer_js,
        readOnly = readOnly
      )
  # for checkbox cols:
  } else if (col_config$type == 'checkbox') {
    renderer_js <- renderer_check(
      required = col_config$required,
      min_checks = col_config$min_checks,
      max_checks = col_config$max_checks
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'checkbox',
        renderer = renderer_js,
        readOnly = readOnly
      )
    # for date cols:
  } else if (col_config$type == 'date') {
    renderer_js <- renderer_date(
      required = col_config$required
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'date',
        dateFormat = "YYYY-MM-DD",
        renderer = renderer_js,
        readOnly = readOnly
      )
  }
}
