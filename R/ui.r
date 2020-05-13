
# https://github.com/dreamRs/esquisse

mtcars2 = crosstable::mtcars2

my_data1 = datasets::mtcars
my_data2 = crosstable::mtcars2
# my_data22 = cbind(crosstable::mtcars2, crosstable::mtcars2, crosstable::mtcars2, crosstable::mtcars2) %>% as_tibble(.name_repair = "unique")
my_data3 = datasets::iris

# library(shinyWidgets)
# library(shinyBS) #Bootstrap
# source("utils.R")

glyphicon=list(yes = icon("ok", lib = "glyphicon"))

modal_by_radio = fluidPage(
  shinyWidgets::materialSwitch("by_modal_sort", label="Sort alphabetically", status="primary", value=FALSE),
  uiOutput('by_radiobuttons')
)


#' User Interface
#'
#' @import shinyWidgets
#' @import shinyBS
#' @return
#' @export
crosstableUI = function(){
  fluidPage(
    shinyjs::useShinyjs(),
    title="Crosstable",
    # theme="styles.css",
    titlePanel("Crosstable helper"),
    sidebarLayout(

      sidebarPanel(
        tags$h3("Dataset Chooser"),

        fluidRow(
          column(5, esquisse::chooseDataUI(id="choose_dataset")),
          column(4, textOutput("dataset_placeholder"))
        ),
        fluidRow(
          column(5, actionButton("by_button", label="By :", width = "100%", height = "100%")),
          column(4, textOutput("by_placeholder")),
          bsModal("by_modal", title="Choose the `by` column", trigger="by_button", size="large", modal_by_radio)
        ),

        tags$h3("Options"),

        #Correlations (only numerics)
        conditionalPanel(
          condition = "output.by_class =='num'",
          radioGroupButtons("cor_method", label="Correlation method (single choice)",
                            choices=c("Pearson"="pearson", "Kendall"="kendall", "Spearman"="spearman"),
                            justified = TRUE,
                            individual = TRUE),
        ),

        #Margins (multi if nonnum, binary if null, not shown if num)
        #TODO binary if only one level
        conditionalPanel(
          condition = "output.by_class =='nonnum'",
          checkboxGroupButtons("margin", label="Margin Percentages (multiple choice)",
                               choices=c("On rows"="row", "On columns"="column", "On cells"="cell"),
                               selected=c("row"), justified = TRUE, checkIcon = glyphicon),
        ),
        conditionalPanel(
          condition = "output.by_class =='null'",
          materialSwitch("margin2", label="Margin Percentages", status="primary", value=TRUE)
        ),

        #Total
        conditionalPanel(
          condition = "output.by_class =='null' || output.by_class =='nonnum'",
          checkboxGroupButtons("total", label="Total (multiple choice)",
                               choices=c("For rows"="row", "For columns"="col"),
                               justified = TRUE, checkIcon = glyphicon),
          helpText("Total for columns applies only on categorial variables."),
        ),

        #showNA
        radioGroupButtons("showNA", label="Show Missing (single choice)",
                          choices=c("If Any"="ifany", "Always"="always", "No"="no"),
                          justified = TRUE, individual = TRUE),

        #Binary options:
        conditionalPanel(
          condition = "output.has_label == true",
          materialSwitch("label", label="Display Labels", status="primary", value=TRUE),
        ),
        materialSwitch("test", label="Perform Tests", status="primary", value=FALSE),
        materialSwitch("effect", label="Compute Effect", status="primary", value=FALSE),

        # actionButton("todo", "TODO :"),
        #
        # checkboxGroupInput("funs ", "Functions to apply",
        #                    c("default", "mean", "sd", "median", "IQR", "min", "max", "N", "NA"),
        #                    selected = "default",
        #                    # TODO Si un autre est cliqué, on déclique+désactive default.
        #                    # Si rien n'est cliqué on reclique+active default
        #                    inline = TRUE),
        # textInput("funs_text", label="Manual functions", placeholder="Write functions here as lambda functions, separated by '___'"),
        width = 4
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Flextable", fluidPage(
            uiOutput("result_FT_message"),
            materialSwitch("keep_id", label="Keep .id column ?", status="primary", value=TRUE),
            uiOutput("result_flextable")
          )),
          tabPanel("Crosstable", fluidPage(
            DT::dataTableOutput("result_crosstable")
          )),
          tabPanel("Dataset", fluidPage(
            DT::dataTableOutput("result_dataset")
          )),
          tabPanel("Code", fluidPage(
            verbatimTextOutput("result_code")
          ))
        ),
        tags$style(type="text/css", "#ttf, #table_summary {white-space: pre-wrap;}"),
        #TODO tooltip à changer :-(
        tags$script(type="javascript", '"$(#choose1-chooseData-selected-help-select-vars").attr("data-content", "prout")'),
        width = 8,
      )
    ),
    bsTooltip(id = "showNA", title = "This is an input", placement = "right", trigger = c("hover", "click"))
  )
}

# ui = ui()
