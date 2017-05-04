base64enc <- function(filename) {
    filesize <- file.info(filename)$size

    if (filesize > 0) {
        pngdata <- base64_enc(readBin(filename, "raw", n = filesize))
        paste0("data:image/png;base64,", pngdata, collapse = "")
    } else {
        warning(paste(sQuote(filename), "is empty"))
        filename
    }
}
