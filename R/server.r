
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
#TODO close modal on "by" choice
#TODO tooltips
# addTooltip(session, id = "margin", title = "This is MARGIN!!",
#            placement = "right", trigger = "hover")
#TODO alert confirm autotesting is bad
#TODO demander à esquisse si on peut mettre une valeur par défaut au select
#TODO benchmarker les reactive pour qu'ils ne se lancent qu'une fois

#' Server
#'
#' @importFrom glue glue
#' @import shiny
#' @import shinylogs
#' @import tidyverse
#' @import crosstable
#' @return
#' @export
crosstableServer = function(input, output, session, data=NULL) {

  if(is.null(data)){
    data=list(name = "mtcars2", data = mtcars2)
  }

  dataset = callModule(
    esquisse::chooseDataServer, id="choose_dataset",
    data=data$data, name=data$name,
    # dataModule="ImportFile",
    selectedTypes=c("discrete", "time", "continuous"),
    coerceVars=FALSE,
    launchOnStart = FALSE #TODO si pas de dataframe sélectionné ou si pas une DF, mettre TRUE
  )

  output$dataset_placeholder = renderText({
    .data = reactiveValuesToList(dataset)
    glue("{.data$name} ({nrow(.data$data)} rows, {ncol(.data$data)} cols)")
  })

  observeEvent(reactiveValuesToList(dataset), {
    updateRadioGroupButtons(session, "by", selected = "NULL")
  })

  observeEvent(input$by,{
    if(!is.null(input$by) && !input$by=="NULL")
      toggleModal(session, "by_modal", toggle="close")
  })

  output$by_radiobuttons = renderUI({
    dataset = reactiveValuesToList(dataset)$data
    radio_opts = if(input$by_modal_sort) sort(names(dataset)) else names(dataset)
    radioGroupButtons("by", "By column", choices=c("NULL", radio_opts))
  })

  output$by_placeholder = renderText({
    .by = input$by
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
    if(is.null(input$by) || input$by=="NULL") NULL else input$by
  })
  get_margin = reactive({
    if(is.null(get_by())) {
      if(input$margin2) "col" else "none"
    }
    else if(is.null(input$margin)) "none"
    else input$margin
  })
  get_total = reactive({
    if(is.null(input$total)) "none"
    else if(setequal(input$total, c("row", "col"))) "both"
    else input$total
  })

  mdata = reactive({
    x=reactiveValuesToList(dataset)
    .data=x$data
    .data_name=x$name

    if(is.null(unlist(reactiveValuesToList(dataset))))
      return(list(err="First, select a dataset"))

    .by=get_by()
    .margin=get_margin()
    .total=get_total()

    console_log("**CROSSTABLE GENERATION**", title=TRUE)
    console_var(input$by, .by)
    console_var(input$margin, .margin)
    console_var(input$total, .total)
    console_var(input$cor_method)

    warn = err = NULL

    if(!is.data.frame(.data)){
      data_class = class(.data) %>% paste(collapse=", ")
      error = glue("Selected data should be a data.frame, it is now a [{data_class}]. Please select another dataset.")
      return(list(err=error))
    }

    rtn = withCallingHandlers(
      tryCatch(
        crosstable(data=.data, by=.by,
                   margin=.margin, total=.total, showNA=input$showNA, label=input$label,
                   cor_method=input$cor_method,
                   test=input$test, effect=input$effect),
        error=function(e) {
          err <<- append(err, conditionMessage(e))
          NULL
        }), warning=function(w) {
          warn <<- append(warn, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
    )
    # browser()
    list(crosstable=rtn, warn=warn, err=err)
  })


  output$has_label = reactive({
    .data=reactiveValuesToList(dataset)$data
    if(is.null(unlist(.data))) return(FALSE)
    has_label = .data %>% map_lgl(~!is.null(var_lab(.x))) %>% any
    console_var(has_label)
    has_label
  })

  output$by_class = reactive({
    .data=reactiveValuesToList(dataset)$data
    if(is.null(unlist(.data))) return("null")
    # browser()

    by_is_null = is.null(input$by) || input$by=="NULL"
    by_is_num = !by_is_null && is.numeric(.data[[input$by]])
    by_is_nonnum = !by_is_null && !is.numeric(.data[[input$by]])

    if(by_is_null || by_is_num){
      console_log("total row disable")
      selected = if("col" %in% input$total) "col" else character(0)
      updateCheckboxGroupButtons(session, "total", selected=selected)
      enableCheckboxGroupButton("total", "row", FALSE)
    } else {
      console_log("total row enable")
      enableCheckboxGroupButton("total", "row", TRUE)
    }

    rtn = if(by_is_null) "null" else if (by_is_num) "num" else "nonnum"
    rtn
  })

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

  output$result_code = renderText({
    .by=get_by()
    if(is.null(.by)) .by="NULL"
    .margin=get_margin()
    .total=get_total()
    .dataset = reactiveValuesToList(dataset)
    selection = names(.dataset$data) %>% setdiff(.by) %>% paste(collapse=", ")
    # browser()
    #TODO: guide sur comment gérer les colonnes,
    #TODO: guide sur comment ajouter les labels avec expss
    #TODO: lien vers wiki, vignette...
    #TODO: un code avec full parameters, un code simplifié avec les par défaut
    glue('crosstable(data={.dataset$name}, c({selection}), by={.by},
          margin="{.margin}", total="{.total}", showNA="{input$showNA}", label={input$label},
          test={input$test})')
  })

  # Options -----------------------------------------------------------------

  outputOptions(output, "by_class", suspendWhenHidden = FALSE, priority = 1000)
  outputOptions(output, "has_label", suspendWhenHidden = FALSE, priority = 1000)
}



