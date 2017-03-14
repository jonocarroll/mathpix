#' Detect mathpix credentials
#'
#' Checks environmental variables for `MATHPIX_APP_ID` and `MATHPIX_APP_KEY` values
#'
#' @md
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
#'   be used (as found on the mathpix API documentation site).
#'
#' @return a character string of LaTeX commands (or NULL if fails).
#'
#' @import httr
#' @importFrom purrr safely
#' @importFrom base64enc base64encode
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
#'
rmarkdown_block <- function(img, trial = FALSE) {

    header <- "$$\n"
    footer <- "\n$$"

    body <- mathpix_api(img = img, trial = trial)

    return(paste(header, body, footer))
}

#' Convert an image of an equation to LaTeX and insert into `rmarkdown` (if using RStudio)
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
#' @return (invisibly) the `rmarkdown` equation
#'
#' @importFrom rstudioapi insertText isAvailable
#'
#' @export
#'
mathpix <- function(img, trial = FALSE) {

    block <- rmarkdown_block(img, trial)
    if (rstudioapi::isAvailable() && interactive()) rstudioapi::insertText(text = block)
    return(invisible(block))

}

#' Convert a LaTeX block to a PNG image
#'
#' @param latex LaTeX code to be evaluated
#' @param file filename (with directory) to save the image to (defaults to `/tmp/tempfile()`)
#'
#' @return (invisibly) the filename (with directory) where the image was saved.
#'
#' @importFrom purrr safely
#'
#' @export
#'
render_latex <- function(latex, file = NULL) {

    # latex <- gsub("\\", "\\\\", latex)

    safe_png <- purrr::safely(latexreadme::png_latex)

    img <- safe_png(latex)
    if (!is.null(img$error)) stop(img$error)

    # if no target file was provided, copy the file to the
    # current directory with the temp filename
    if (is.null(file)) file <- paste0("./", basename(img$result))
    file.copy(img$result, file)

    return(invisible(file))

}
