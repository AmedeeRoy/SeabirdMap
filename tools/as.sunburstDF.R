# format dt for sunburst
as.sunburstDF <- function(DF, valueCol = NULL, total = TRUE) {
    require(data.table)
    require(stringr)
    
    DT <- data.table(DF, stringsAsFactors = FALSE)
    DT[, `:=`(root, "total")]
    setcolorder(DT, c("root", names(DF)))
    
    hierarchyList <- list()
    if (!is.null(valueCol)) {
        setnames(DT, valueCol, "values", skip_absent = TRUE)
    }
    hierarchyCols <- setdiff(names(DT), "values")
    
    for (i in seq_along(hierarchyCols)) {
        currentCols <- names(DT)[1:i]
        if (is.null(valueCol)) {
            currentDT <- unique(DT[, ..currentCols][, `:=`(values, .N), by = currentCols], by = currentCols)
        } else {
            currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by = currentCols, .SDcols = "values"]
        }
        setnames(currentDT, length(currentCols), "labels")
        hierarchyList[[i]] <- currentDT
    }
    
    hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
    
    parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
    hierarchyDT[, `:=`(parents, apply(.SD, 1, function(x) {
        fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))
    })), .SDcols = parentCols]
    hierarchyDT[, `:=`(ids, apply(.SD, 1, function(x) {
        paste(x[!is.na(x)], collapse = " - ")
    })), .SDcols = c("parents", "labels")]
    hierarchyDT[, `:=`(c(parentCols), NULL)]
    
    if (!total) {
        hierarchyDT <- hierarchyDT[2:nrow(hierarchyDT), ]
        for (i in 1:nrow(hierarchyDT)) {
            line <- hierarchyDT[i, c("parents", "ids")]
            line <- sapply(line, function(k) {
                if (k == "total") {
                  k = NA
                } else {
                  k = word(k, 2, -1, sep = " - ")
                }
            })
            hierarchyDT[i, "parents"] <- line["parents"]
            hierarchyDT[i, "ids"] <- line["ids"]
        }
    }
    return(hierarchyDT)
}
