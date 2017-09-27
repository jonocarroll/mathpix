#' Detect mathpix credentials
#'
#' Checks environmental variables for `MATHPIX_APP_ID` and `MATHPIX_APP_KEY` values
#'
#' @md
#' @keywords internal
#' @return a list of detected credentials (or this package's credentials)
credentials <- function() {

    app_id <- Sys.getenv("MATHPIX_APP_ID")
    app_key <- Sys.getenv("MATHPIX_APP_KEY")

    if (app_id == "" || app_key == "") {
        app_id = 'jcarroll'
        app_key = '13f1584b2f9edb8220bf619c0b4e3d5a'
    }

    return(list(app_id = app_id, app_key = app_key))
}

# curl -X POST https://api.mathpix.com/v3/latex \
#     -H 'app_id: trial' \
#     -H 'app_key: 34f1a4cea0eaca8540c95908b4dc84ab' \
#     -H 'Content-Type: application/json' \
#     --data '{ "url": "data:image/jpeg;base64,'$(base64 -i limit.jpg)'" }'

## NOTE THAT IN TESTING IN SHELL
## curl 7.35.0 (x86_64-pc-linux-gnu) libcurl/7.35.0
## THE BASE64 OUTPUT WAS WRAPPED SO
## -w0 WAS REQUIRED

#' Connect to the mathpix API and translate an image to LaTeX
#'
#' @param img image to be converted to LaTeX
#' @param trial should the trial API key be used (see Details)?
#'
#' @details I have obtained an API key for use with this app, which I have
#'   included. If you have your own API key feel free to save that in your
#'   environment (e.g. `~/.Renviron`) with the identifiers `MATHPIX_APP_ID` and
#'   `MATHPIX_APP_KEY`. If this fails for some reason, the `trial` API key can
#'   be used (as found on the 'Mathpix' API documentation site).
#'
#' @return a character string of LaTeX commands (or NULL if fails).
#'
#' @import httr
#' @importFrom purrr safely
#' @importFrom base64enc base64encode
#' @keywords internal
#' @export
mathpix_api <- function(img, trial = FALSE) {

    if (!file.exists(img)) {
        warning("Unable to locate that image.")
        return(NULL)
    }

    creds <- credentials()
    app_id <- creds$app_id
    app_key <- creds$app_key
    if (trial || app_id == "" || app_key == "") {
        app_id = 'trial'
        app_key = '34f1a4cea0eaca8540c95908b4dc84ab'
    }

    message(paste0("Using Mathpix APP_ID=", app_id))

    url <- "https://api.mathpix.com/v3/latex"
    body <- paste0('{"url": "data:image/jpeg;base64, ',
           base64enc::base64encode(img),
           '\" }')

    safePOST <- purrr::safely(httr::POST)

    res <- safePOST(url,
                      httr::add_headers(app_id = app_id,
                                  app_key = app_key),
                      httr::content_type_json(),
                      body = body)

    if (!is.null(res$error)) stop(res$error)
    if (!identical(httr::status_code(res$result), 200L)) {
        warning(paste("httr POST returned", httr::status_code(res$result)))
        return(NULL)
    } else {
        return(httr::content(res$result)$latex)
    }

}

#' Add LaTeX commands as an `rmarkdown` equation
#'
#' Converts an image to LaTeX then inserts that into an `rmarkdown` equation
#' (surrounded by $$).
#'
#' @md
#' @inheritParams mathpix_api
#'
#' @details I have obtained an API key for use with this app, which I have
#'   included. If you have your own API key feel free to save that in your
#'   environment (e.g. `~/.Renviron`) with the identifiers `MATHPIX_APP_ID` and
#'   `MATHPIX_APP_KEY`. If this fails for some reason, the `trial` API key can
#'   be used (as found on the mathpix API documentation site).
#'
#' @return an `rmarkdown` equation block
#' @keywords internal
#' @export
rmarkdown_block <- function(img, trial = FALSE) {

    header <- "$$\n"
    footer <- "\n$$"

    body <- mathpix_api(img = img, trial = trial)

    return(paste(header, body, footer))
}

#' Convert an image of an equation to a 'LaTeX' expression
#'
#' Given an image file location, \code{mathpix} performs the relevant
#' transformations and send the data to the 'Mathpix' API, which returns a
#' 'LaTeX' expression which should generate the typeset equation/expression in
#' that image. When using 'RStudio', the resulting 'LaTeX' expression is
#' automatically inserted into the current `rmarkdown` document.
#'
#' @md
#'
#' @inheritParams mathpix_api
#'
#' @details I have obtained an API key for use with this app, which I have
#'   included. If you have your own API key feel free to save that in your
#'   environment (e.g. `~/.Renviron`) with the identifiers `MATHPIX_APP_ID` and
#'   `MATHPIX_APP_KEY`. If this fails for some reason, the `trial` API key can
#'   be used (as found on the mathpix API documentation site). Refer to
#'   \url{https://docs.mathpix.com/} for full details.
#'
#' @return (invisibly) the `rmarkdown` equation
#'
#' @importFrom rstudioapi insertText isAvailable
#'
#' @references \url{https://mathpix.com/}
#'
#' @export
#'
#' @examples
#' mathpix(system.file("eq_no_01.png", package = "mathpix"))
#' ## returns
#' ## $$\n \\int \\frac { 4x } { \\sqrt { x ^ { 2} + 1} } d x \n$$
mathpix <- function(img, trial = FALSE) {

    block <- rmarkdown_block(img, trial)
    if (rstudioapi::isAvailable() && interactive()) rstudioapi::insertText(text = block)
    return(invisible(block))

}

#' Convert a 'LaTeX' expression to an image (render)
#'
#' This calls \code{\link[texPreview]{texPreview}} to render a 'LaTeX'
#' expression into an image, either as a temporary file or saved to disk.
#'
#' @param latex 'LaTeX' code to be evaluated. Surround in \code{$} or \code{$$}.
#' @param fileDir directory in which to save the image to (defaults to
#'   `/tmp/tempfile()`).
#' @param ... other options to pass to \code{\link[texPreview]{texPreview}}.
#'
#' @return NULL (invisibly)
#'
#' @importFrom purrr safely
#'
#' @export
#' @examples
#' \dontrun{
#' ## requires pdflatex
#' latex_expression <- "$$\\int \\frac { 4 x } { \\sqrt { x ^ { 2 } + 1 } } d x$$"
#' render_latex(latex_expression)}
render_latex <- function(latex, fileDir = NULL, ...) {

    safe_png <- purrr::safely(texPreview::texPreview)
    img <- safe_png(obj = latex, stem = "eq", fileDir = fileDir)
    if (!is.null(img$error)) stop(img$error)
    return(invisible(NULL))

}
