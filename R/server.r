
# library(glue)
# library(shiny)
# library(flextable)
# library(shinylogs)
# library(shinyalert)
# library(reactlog)
# library(tidyverse)

# library(crosstable)

# options(shiny.reactlog = TRUE)
# Sys.setenv(LANG = "en")

# source("utils.R")
#TODO changer le tooltip du dataset chooser
#TODO https://shiny.rstudio.com/articles/validation.html
#TODO demander à esquisse si on peut mettre une valeur par défaut au select
#TODO benchmarker les reactive pour qu'ils ne se lancent qu'une fois
#TODO survival

#' Server
#'
#' @importFrom glue glue
#' @importFrom knitr knit_print
#' @importFrom purrr map_lgl
#' @importFrom expss var_lab
#' @importFrom stringr str_subset
#' @importFrom crosstable crosstable as_flextable
#' @importFrom dplyr %>%
#' @import shiny
#' @export
crosstableServer = function(input, output, session, data=NULL) {

  dataset = callModule(
    esquisse::chooseDataServer, id="choose_dataset",
    data=data$data, name=data$name,
    # dataModule="ImportFile",
    selectedTypes=c("discrete", "time", "continuous"),
    coerceVars=FALSE,
    launchOnStart = is.null(data) || is.null(data$data)
  )

  if(is.null(data)){
    data=list(name = "mtcars2", data = mtcars2)
  }

  observeEvent(reactiveValuesToList(dataset), {
    updateRadioGroupButtons(session, "by", selected = "NULL")
  })

  observeEvent(input$by,{
    if(!is.null(input$by) && !input$by=="NULL"){
      removeModal(session)
    }
  })

  observeEvent(input$test,{
    if(input$test==TRUE){
      showModal(modalDialog(
        title = "Warning: Automatic testing",
        renderUI(HTML("Automatic testing can cause extensive <a href='https://en.wikipedia.org/wiki/Multiple_comparisons_problem'>alpha inflation</a> and lead to false discoveries. <br> For this reason, it should only be conducted in an exploratory context.")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })

  observeEvent(input$by_button, {
    showModal(modalDialog(
      shinyWidgets::materialSwitch("by_modal_sort", label="Sort alphabetically", status="primary", value=FALSE),
      uiOutput('by_radiobuttons'),
      easyClose=TRUE,
      footer=NULL
    ))
  })

  output$dataset_placeholder = renderText({
    .data = reactiveValuesToList(dataset)
    glue("{.data$name} ({nrow(.data$data)} rows, {ncol(.data$data)} cols)")
  })

  output$by_radiobuttons = renderUI({
    dataset = reactiveValuesToList(dataset)$data
    radio_opts = if(input$by_modal_sort) sort(names(dataset)) else names(dataset)
    radioGroupButtons("by", "By column", choices=c("NULL", radio_opts), selected=get_by())
  })

  output$by_placeholder = renderText({
    .by = get_by()
    .data = reactiveValuesToList(dataset)$data
    # browser()
    if(is.data.frame(.data) && !is.null(.by) && !is.null(.data[[.by]])){
      classes =  class(.data[[.by]]) %>% sort(TRUE) %>% paste(collapse = ", ")
      glue("{.by} ({classes})")
    } else{
      "No `by` column"
    }
  })

  get_by = reactive({
    x=reactiveValuesToList(dataset)
    if(is.null(unlist(x)) || is.null(input$by) || input$by=="NULL") NULL else input$by
  })
  get_margin = reactive({
    if(is.null(get_by())) {
      if(input$margin2) "column" else "none"
    }
    else if(is.null(input$margin)) "none"
    else input$margin
  })
  get_total = reactive({
    if(is.null(input$total)) "none"
    else if(setequal(input$total, c("row", "column"))) "both"
    else input$total
  })

  mdata = reactive({
    x=reactiveValuesToList(dataset)
    .data=x$data
    .data_name=x$name

    if(is.null(unlist(x)))
      return(list(err="First, select a dataset"))

    if(!is.data.frame(.data)){
      data_class = class(.data) %>% paste(collapse=", ")
      error = glue("Selected dataset '{.data_name}' is of class [{data_class}] but should be of class `data.frame`. Please select a proper dataset.")
      return(list(err=error))
    }

    .by=get_by()
    .margin=get_margin()
    .total=get_total()

    # console_log("**CROSSTABLE GENERATION**", title=TRUE)
    # console_var(.data_name, dim(.data))
    # console_var(input$by, .by)
    # console_var(input$margin, .margin)
    # console_var(input$total, .total)
    # console_var(input$cor_method)

    warn = err = NULL
    rtn = withCallingHandlers(
      tryCatch(
        crosstable(data=.data, by=.by,
                   margin=.margin, total=.total, showNA=input$showNA, label=input$label,
                   cor_method=input$cor_method,
                   unique_numeric=input$unique_numeric,
                   percent_digits=input$percent_digits,
                   test=input$test, effect=input$effect),
        error=function(e) {
          err <<- append(err, conditionMessage(e))
          NULL
        }), warning=function(w) {
          warn <<- append(warn, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
    )

    warn = warn %>% unique %>% sort %>% str_subset("automatic global testing", negate=TRUE)

    list(crosstable=rtn, warn=warn, err=err)
  })


  has_label = reactive({
    .data=reactiveValuesToList(dataset)$data
    if(is.null(unlist(.data))) return(FALSE)
    has_label = .data %>% map_lgl(~!is.null(var_lab(.x))) %>% any
    console_var(has_label)
    has_label
  })
  output$has_label = reactive({has_label()})

  get_by_class = reactive({
    .data=reactiveValuesToList(dataset)$data
    if(is.null(unlist(.data))) return("null")

    by_is_null = is.null(get_by())
    by_is_dummy = !by_is_null && length(unique(.data[[get_by()]]))==1
    by_is_num = !by_is_null && !by_is_dummy && is.numeric(.data[[get_by()]])
    by_is_nonnum = !by_is_null && !is.numeric(.data[[get_by()]])

    if(by_is_null || by_is_dummy|| by_is_num){
      console_log("total row disable")
      selected = if("column" %in% input$total) "column" else character(0)
      updateCheckboxGroupButtons(session, "total", selected=selected)
      enableCheckboxGroupButton("total", "row", FALSE)
    } else {
      console_log("total row enable")
      enableCheckboxGroupButton("total", "row", TRUE)
    }

    by_class = if(by_is_null) "null" else if (by_is_dummy) "dummy" else if (by_is_num) "num" else "nonnum"
    console_var(by_class)
    by_class
  })
  output$by_class = reactive({get_by_class()})

  # Results -----------------------------------------------------------------

  output$result_flextable = renderUI({
    if(!is.null(mdata()$crosstable)){
      mdata()$crosstable %>% as_flextable(keep_id=input$keep_id) %>% knit_print %>% HTML
    }
  })

  output$result_FT_message = renderUI({
    warns = mdata()$warn
    errors = mdata()$err
    rtn=NULL
    if(length(errors)>0){
      rtn = paste0("<strong>Errors</strong> <ul><li>",
                   paste0(errors, collapse = "</li><li>"), "</li></ul>") %>% HTML
    } else if(length(warns)>0){
      rtn = paste0("<strong>Warnings</strong> <ul><li>",
                   paste0(warns, collapse = "</li><li>"), "</li></ul>") %>% HTML
    }
    rtn
  })

  output$result_crosstable = DT::renderDataTable({
    mdata()$crosstable
  })

  output$result_dataset = DT::renderDataTable({
    reactiveValuesToList(dataset)$data
  })

  output$result_full_code = renderPrint({
    .by=get_by()
    if(is.null(.by)) .by="NULL"
    .margin=get_margin()
    if(length(.margin)>1)
      .margin=glue('c("{paste(.margin, collapse="\\", \\"")}")')
    else
      .margin=glue('"{.margin}"')

    .total=get_total()
    .dataset = reactiveValuesToList(dataset)
    selection = names(.dataset$data) %>% setdiff(.by) %>% paste(collapse=", ")
    nl=paste0("\n", strrep(" ", nchar("ct = crosstable(")))
    glue('ct = crosstable(data={.dataset$name}, {nl}c({selection}), {nl}by={.by}, {nl}margin={.margin}, {nl}total="{.total}", {nl}percent_digits={input$percent_digits}, {nl}showNA="{input$showNA}", {nl}label={input$label}, {nl}cor_method="{input$cor_method}", {nl}unique_numeric={input$unique_numeric}, {nl}test={input$test}, {nl}effect={input$effect})\nas_flextable(ct)') %>% cat
  })

  output$result_simple_code = renderText({
    .dataset = reactiveValuesToList(dataset)
    by=get_by()
    .by_class=get_by_class()
    selection = names(.dataset$data) %>% setdiff(by)
    full_selection = try(names(get(.dataset$name)) %>% setdiff(by), silent=TRUE)
    if(!setequal(selection, full_selection)){
      if(length(setdiff(full_selection, selection))<length(selection)){
        .selection="-c({paste(setdiff(full_selection, selection), collapse=', ')})"
      } else {
        .selection="c({paste(selection, collapse=', ')})"
      }
    } else {
      .selection=NULL
    }

    if(!is.null(by)) .by="by={by}" else .by=NULL
    margin=get_margin()
    # if(!identical(margin, "row") && !is.null(by)) {
    #   # browser()
    #   if(length(margin)==1)
    #     .margin='margin="{margin}"'
    #   else
    #     .margin='margin=c("{paste(margin, collapse="\\", \\"")}")'
    # } else {
    #   .margin=NULL
    # }
    if(identical(margin, "row") || is.null(by)) {
      .margin=NULL
    } else if(setequal(margin, c("row", "column", "cell"))){
      .margin='margin="all"'
    } else {
      if(length(margin)==1)
        .margin='margin="{margin}"'
      else
        .margin='margin=c("{paste(margin, collapse="\\", \\"")}")'
    }



    percent_digits=input$percent_digits
    if(percent_digits!=2) .percent_digits='percent_digits={percent_digits}' else .percent_digits=NULL
    total=get_total()
    if(total!="none") .total='total="{total}"' else .total=NULL
    showNA=input$showNA
    if(showNA!="ifany") .showNA='showNA="{showNA}"' else .showNA=NULL
    label=input$label
    if(label==FALSE && has_label()) .label='label={label}' else .label=NULL
    cor_method=input$cor_method
    if(cor_method!="pearson" && .by_class=="num") .cor_method='cor_method="{cor_method}"' else .cor_method=NULL
    test=input$test
    if(test==TRUE && !.by_class %in% c("null", "dummy")) .test='test={test}' else .test=NULL
    effect=input$effect
    if(effect==TRUE && !.by_class %in% c("null", "dummy")) .effect='effect={effect}' else .effect=NULL
    unique_numeric=input$unique_numeric
    if(unique_numeric!=3) .unique_numeric='unique_numeric={unique_numeric}' else .unique_numeric=NULL

    cross_params = glue(paste(c(.selection, .by, .margin, .percent_digits, .total, .showNA, .label, .cor_method, .test, .effect, .unique_numeric), collapse=", "))
    if(cross_params!="") cross_params = glue(", ", cross_params)

    glue("ct = crosstable({.dataset$name}{cross_params})\nas_flextable(ct)")
  })

  output$code_guide_label = renderUI({
    if(!has_label()){
      HTML('It seems that your dataset is not labelled. Labels are a good way to improve the readability of crosstables, as column naming is restricted in R, so your end reader may not understand your dataset columns. <br>
           Labels are easy to implement using <pre>Hmisc::label`</pre> or <pre>expss::var_lab`</pre>. You can see an example of the even more powerful function <pre>expss::apply_labels`</pre> by running <pre>`vignette("crosstable")`</pre> or by clicking <a href="https://github.com/DanChaltiel/crosstable/wiki/Basic-features#dataset-modified-mtcars">here</a>')
    }
  })

  output$result_flextable_dataset_name = renderText({
    .data = reactiveValuesToList(dataset)
    glue("Crosstable of '{.data$name}' as a flextable")
  })
  output$result_crosstable_dataset_name = renderText({
    .data = reactiveValuesToList(dataset)
    glue("Crosstable of '{.data$name}' as a data.frame")
  })
  output$dataset_name = renderText({
    .data = reactiveValuesToList(dataset)
    glue("Dataset '{.data$name}'")
  })

  # Options -----------------------------------------------------------------

  outputOptions(output, "by_class", suspendWhenHidden = FALSE, priority = 1000)
  outputOptions(output, "has_label", suspendWhenHidden = FALSE, priority = 1000)
}



