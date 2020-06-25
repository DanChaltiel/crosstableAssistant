
# https://github.com/dreamRs/esquisse

mtcars2 = crosstable::mtcars2
iris2 = crosstable::iris2
mpg = ggplot2::mpg

mtcars = datasets::mtcars
iris = datasets::iris

mtcars2_dummy = dplyr::mutate(crosstable::mtcars2, dummy="dummy")


glyphicon = list(yes=shiny::icon("ok", lib = "glyphicon"))

#' User Interface
#'
#' @import shinyWidgets
#' @import shiny
#' @import miniUI
#' @export
crosstableUI = function(){
  fluidPage(
    shinyjs::useShinyjs(),
    title="Crosstable",
    gadgetTitleBar("Crosstable Assistant", left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("close_paste", "Paste code and close", primary = TRUE)),
    sidebarLayout(

      sidebarPanel(
        tags$h3("Dataset Chooser"),

        fluidRow(
          column(5, esquisse::chooseDataUI(id="choose_dataset", label="Choose Data")),
          column(4, textOutput("dataset_placeholder"))
        ),
        fluidRow(
          column(5, actionButton("by_button", label="By :", width = "100%", height = "100%")),
          column(4, textOutput("by_placeholder")),
        ),

        tags$h3("Options"),

        #Correlations (only numerics)
        conditionalPanel(
          condition = "output.by_class == 'num'",
          radioGroupButtons("cor_method", label="Correlation method (single choice)",
                            choices=c("Pearson"="pearson", "Kendall"="kendall", "Spearman"="spearman"),
                            justified = TRUE,
                            individual = TRUE),
        ),

        #Margins (multi if nonnum, binary if null or dummy, not shown if num)
        conditionalPanel(
          condition = "output.by_class == 'nonnum'",
          checkboxGroupButtons("margin", label="Margin Percentages (multiple choice)",
                               choices=c("On rows"="row", "On columns"="column", "On cells"="cell"),
                               selected=c("row"), justified = TRUE, checkIcon = glyphicon),
        ),
        conditionalPanel(
          condition = "output.by_class == 'null' || output.by_class == 'dummy'",
          materialSwitch("margin2", label="Margin Percentages", status="primary", value=TRUE)
        ),
        numericInput("percent_digits", label="Percentage decimal places", min=0, max=10, value=2),

        #Total (always, except for numerical)
        conditionalPanel(
          # condition = "output.by_class =! 'num'",
          condition = "output.by_class == 'null' || output.by_class == 'dummy' || output.by_class == 'nonnum'",
          checkboxGroupButtons("total", label="Total (multiple choice)",
                               choices=c("For rows"="row", "For columns"="column"),
                               justified = TRUE, checkIcon = glyphicon),
          helpText("Total for columns applies only on categorial variables."),
        ),

        #showNA
        radioGroupButtons("showNA", label="Show Missing (single choice)",
                          choices=c("If Any"="ifany", "Always"="always", "No"="no"),
                          justified = TRUE, individual = TRUE),
        helpText("Show Missing applies only on categorial variables."),

        #Unique for numeric
        numericInput("unique_numeric", label="Number of unique values to be considered as numeric", min=0, value=3),

        #Binary options:
        conditionalPanel(
          condition = "output.has_label == true",
          materialSwitch("label", label="Display Labels", status="primary", value=TRUE),
        ),
        conditionalPanel(
          condition = "output.by_class == 'num' || output.by_class == 'nonnum'",
          materialSwitch("test", label="Perform Tests", status="primary", value=FALSE),
        ),
        conditionalPanel(
          condition = "output.by_class == 'nonnum'",
          materialSwitch("effect", label="Compute Effect", status="primary", value=FALSE),
        ),
        conditionalPanel(
          condition = "output.by_class != 'num'",
          checkboxGroupButtons("funs", label="Functions to apply",
                               choices=c("Min / Max"="minmax", "min", "max"),
                               selected = c("minmax"),
                               checkIcon = glyphicon
          ),
          checkboxGroupButtons("funs2", label=NULL,
                               choices=c("Median [IQR]"="mediqr", "median", "IQR"),
                               selected = c("mediqr"),
                               checkIcon = glyphicon
          ),
          checkboxGroupButtons("funs3", label=NULL,
                               choices=c("Mean (std)"="moystd", "mean", "sd"),
                               selected = c("moystd"),
                               checkIcon = glyphicon
          ),
          checkboxGroupButtons("funs4", label=NULL,
                               choices=c("N (NA)"="nna", "N", "NA"),
                               selected = c("nna"),
                               checkIcon = glyphicon
          )
        ),
        width = 4
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Flextable", fluidPage(
            uiOutput("result_FT_message"),
            materialSwitch("keep_id", label="Keep .id column ?", status="primary", value=TRUE),
            h3(textOutput("result_flextable_dataset_name", inline=TRUE)),
            uiOutput("result_flextable")
          )),
          tabPanel("Crosstable", fluidPage(
            h3(textOutput("result_crosstable_dataset_name", inline=TRUE)),
            DT::dataTableOutput("result_crosstable")
          )),
          tabPanel("Dataset", fluidPage(
            h3(textOutput("dataset_name", inline=TRUE)),
            DT::dataTableOutput("result_dataset")
          )),
          tabPanel("Code", fluidPage(
            h4("Simplified code"),
            verbatimTextOutput("result_simple_code"),
            actionButton("code_insert_console", label="Insert this code into Source"),
            h4("Full code (all parameters)"),
            verbatimTextOutput("result_full_code"),
            h4("Advices"),
            tags$ul(
              tags$li(HTML('Getting help: <a href="https://danchaltiel.github.io/crosstable/">Documentation website</a>.')),
              htmlOutput("code_guide_label"),
            ),
          ))
        ),
        tags$style(type="text/css", "#ttf, #table_summary {white-space: pre-wrap;}"),
        # tags$script(type="javascript", '"$(#choose1-chooseData-selected-help-select-vars").attr("data-content", "prout")'),
        width = 8,
      )
    ),
    # shinyBS::bsTooltip(id = "showNA", title = "This is an input", placement = "right", trigger = c("hover", "click"))
  )
}
