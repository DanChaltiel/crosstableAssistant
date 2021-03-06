% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{console_log}
\alias{console_log}
\title{Prints something in the JS console}
\usage{
console_log(x, ..., color = NULL, bg = NULL, title = FALSE)
}
\description{
You can use the `glue` syntax to include variables in the log. You can parametrize the color and the background. The title argument simply put the background in red if TRUE.
}
\examples{
console_log("Debug: I am on the line 437... For now at least...")
console_log("There was {nrow(iris)} rows in the iris dataset")
console_log("**Main Object Construction**, title=TRUE")
}
\seealso{
shinyjs::showLog console_var
}
