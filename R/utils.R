


#' Enable or disable a button option from a group of `checkboxGroupButtons`
#'
#' @param inputId the same `inputId` as in `checkboxGroupButtons`
#' @param value the value corresponding to the `choices` argument in `checkboxGroupButtons`
#' @param enable whether to enable (`TRUE`) or disable (`FALSE`) the button
#'
#' @importFrom shinyjs runjs
#' @importFrom glue glue
#' @return
#' @export
enableCheckboxGroupButton = function(inputId, value, enable=TRUE){
  if(enable)
    runjs(glue("$(\"input[name='{inputId}'][value='{value}']\").parents('button').removeAttr('disabled');"))
  else
    runjs(glue("$(\"input[name='{inputId}'][value='{value}']\").parents('button').prop('disabled', true);"))
}




#' Prints something in the JS console
#'
#' You can use the `glue` syntax to include variables in the log. You can parametrize the color and the background. The title argument simply put the color to "red" if TRUE.
#'
#' @importFrom shinyjs runjs
#' @importFrom glue glue
#' @seealso shinyjs::showLog console_var
#' @examples
#' console_log("Debug: I am on the line 437... For now at least...")
#' console_log("There was {nrow(iris)} rows in the iris dataset")
#' console_log("**Main Object Construction**, title=TRUE")
console_log = function(x, ..., color=NULL, bg=NULL, title=FALSE){
  msg = glue(as.character(x), .envir = parent.frame())
  if(is.null(color)) color='#f00'
  if(is.null(bg)) bg='#fff'
  if(title==TRUE)
    runjs(glue("console.log('%c{msg}', 'background:{color}; color:{bg}');"))
  else
    runjs(glue("console.log('{capture.output(msg)}');"))
}


#' Prints variables in the JS console
#'
#' Print one or several variables into the Javascript console. Values will be coerced to character and pasted with `collapse=", "` (see examples).
#'
#' @importFrom shinyjs runjs
#' @importFrom glue glue
#' @importFrom rlang enquos as_label
#' @importFrom purrr map_chr
#' @seealso shinyjs::showLog console_log
#' @examples
#' #in server.R
#' console_var(letters[1], letters[2:4])
#' #output in the JS console:
#' #letters[1]=[a], letters[2:3]=[b, c, d]
console_var = function(...){
  env=parent.frame()
  labs = enquos(...) %>% map_chr(as_label)
  vals = glue("{{{labs}}}") %>%
    map(~glue(.x, .envir = env)) %>%
    map(~glue("[{xx}]", xx=paste(as.character(.x), collapse=", ")))
  msg = glue("{labs}={vals}") %>% paste(collapse=", ")
  msg = glue("console.log('{msg}');")
  runjs(msg)
}

#' test_condition("is_num", undefined="true")
test_condition = function(x, go=TRUE, undefined="true", accessor="do_show"){
  glue("
     {if(!go)'!'}((typeof output!=='undefined' && typeof output.{accessor}!=='undefined' && typeof output.{accessor}.{x}!=='undefined') ? output.{accessor}.{x} : {undefined})
  ")
}

test_condition2 = function(x, equals="true", undefined="true", accessor="do_show"){
  glue("
     ((typeof output!=='undefined' && typeof output.{accessor}!=='undefined' && typeof output.{accessor}.{x}!=='undefined') ? output.{accessor}.{x}=='{equals}' : {undefined})
  ")
}


