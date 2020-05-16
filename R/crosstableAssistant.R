
# iris2
# iris2=crosstable::iris2
# iris=iris

#' Crosstable Assistant
#' @examples
#' crosstableAssistant()
#' crosstableAssistant(mtcars)
crosstableAssistant=function(.data=NULL, viewer="dialog"){

  if(!isNamespaceLoaded("crosstable")){
    attachNamespace("crosstable")
  }

  context = rstudioapi::getSourceEditorContext()
  selected_dataname = context$selection[[1]]$text
  # selected_dataname="ris"
  # selected_dataname="letters"

  data = list()
  if(is.null(.data) && nzchar(selected_dataname)) {
    data$name = selected_dataname
    data$data = mget(selected_dataname, inherit=TRUE, ifnotfound=list(NULL))[[1]]
  } else {
    data$name = deparse(substitute(.data))
    data$data = .data
  }

  viewer = "pane"

  if (viewer == "browser") {
    inviewer <- browserViewer(browser = getOption("browser"))
  } else if (viewer == "pane") {
    inviewer <- paneViewer(minHeight = "maximize")
  } else {
    inviewer <- dialogViewer(
      "Crosstable Builder",
      width = 1000, height = 750
    )
  }

  runGadget(
    app = crosstableUI(),
    server = function(input, output, session) {
      crosstableServer(input, output, session, data=data)
    },
    viewer = inviewer
  )
}


