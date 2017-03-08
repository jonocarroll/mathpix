
credentials <- function() {

    app_id <- Sys.getenv("MATHPIX_APP_ID")
    app_key <- Sys.getenv("MATHPIX_APP_KEY")

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

mathpix <- function(img, trial = FALSE) {

    if (!file.exists(img)) {
        warning("Unable to locate that image.")
        return(NULL)
    }

    if (trial) {
        app_id = 'trial'
        app_key = '34f1a4cea0eaca8540c95908b4dc84ab'
    } else {
        creds <- credentials()
        app_id <- creds$app_id
        app_key <- creds$app_key
    }

    url <- "https://api.mathpix.com/v3/latex"
    body <- paste0('{"url": "data:image/jpeg;base64, ',
           base64enc::base64encode(img),
           '\" }')

    safePOST <- purrr::safely(POST)

    res <- safePOST(url,
                      add_headers(app_id = app_id,
                                  app_key = app_key),
                      content_type_json(),
                      body = body)

    if (!is.null(res$error)) stop(res$error)
    if (!identical(status_code(res$result), 200L)) {
        warning(paste("httr POST returned", status_code(res$result)))
        return(NULL)
    } else {
        return(content(res$result)$latex)
    }

}
