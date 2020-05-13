
# iris2
# iris2=iris2


crosstableAssistant=function(.data=NULL, viewer="dialog"){

  if(!isNamespaceLoaded("crosstable")){
    attachNamespace("crosstable")
  }

  context = rstudioapi::getSourceEditorContext()
  selected_dataname = context$selection[[1]]$text
  selected_dataname="ris"
  selected_dataname="letters"

  data = list()
  if(is.null(.data) && nzchar(selected_dataname)) {
    data$name = selected_dataname
    data$data = get(selected_dataname, envir = parent.frame())
    #TODO checker que selected_dataname est une df/tbl etc
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