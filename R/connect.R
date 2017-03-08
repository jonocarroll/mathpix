
credentials <- function() {

    app_id <- Sys.getenv("MATHPIX_APP_ID")
    app_key <- Sys.getenv("MATHPIX_APP_KEY")

    return(list(app_id = app_id, app_key = app_key))
}

