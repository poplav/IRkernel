#' The Inspector
#'
#' Has methods to support inspecting the code content in the notebook environment
#'
#' @include logging.r
Inspector <- setRefClass(
    'Inspector',
    fields = list(
        send_response = 'function'
    ),
    methods = list(
        #' @name get_code_to_inspect
        #' @param code  The currently executing code to inspect
        #' @param cursor_pos  The position of the cursor in the current code to inspect
        #' @export
        get_code_to_inspect = function(code, cursor_pos) {
            #the start index is the first space or new line before the current cursor_pos
            cursor_pos_list <- Filter(function(x) x < cursor_pos, gregexpr(" |\n", code, perl=TRUE)[[1]])
            start_index <- if (length(cursor_pos_list) > 0) tail(cursor_pos_list, n=1) else -1
            code_to_inspect <- substr(code, start_index+1, cursor_pos)
        },
        inspect = function(request) {
            code <- request$content$code
            cursor_pos <- request$content$cursor_pos
            code_to_inspect <- get_code_to_inspect(code, cursor_pos)
            found <- exists(code_to_inspect)
            if (found) {
                type <- class(get(code_to_inspect))
                data_response <- paste("Type:       ", type)
                if (type %in% c("numeric", "integer", "character", "logical", "list")) {
                    string_form <- paste("String form:", toString(get(code_to_inspect)))
                    data_response <- paste(data_response, string_form, sep="\n")
                } else if(type == "function") {
                    log_info('Inspecting functions is not supported yet, but we do accept PRs')
                }
                data <- setNames(list(c(data_response)), c("text/plain"))
            } else {
                data <- namedlist()
            }
            metadata <- namedlist()
            send_response('inspect_reply', request, 'shell', list(
                status = 'ok',
                found = found,
                data = data,
                metadata = metadata))
        },
        initialize = function(...) {
            callSuper(...)
        }
    )
)