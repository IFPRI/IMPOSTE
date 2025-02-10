#' readFAOSTAT
#'
#' @param data Which data to read from faostat
#' @param entropy_folder Folder where entropy calculation is made
#' @param write
#'
#' @import Hmisc dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom openxlsx2 write_xlsx
#'
#' @return FAOSTAT cleaned data
#' @export
#'
#' @examples
#' \dontrun{
#' readFAOSTAT()
#' }
readFAOSTAT <- function(data = "crop",
                        entropy_folder,
                        write = FALSE) {

    if (write) return <- FALSE
    if (!write) return <- TRUE

    if (!dir.exists(entropy_folder)) {dir.create(entropy_folder)}

    out <- NULL
    if(data == "crop") {
        fpath <- system.file("extdata",
                             "FAOSTAT_data_en_2-10-2025-crop.csv",
                             package="IMPOSTE")

        dfx <- df <- read.csv(file = fpath)

        names(df) <- tolower(names(df))

        names(df) <- gsub(pattern = "\\.\\.|\\.",
                          replacement = "",
                          x = names(df))

        skip_cols <- c("domaincode", "domain",
                       "yearcode",
                       "elementcode",
                       "flag", "flagdescription",
                       "item",
                       "note")

        df <- df[,!names(df) %in% skip_cols]

        names(df) <- gsub(pattern = "codefao",
                          replacement = "code",
                          x = names(df))

        df <- df[df$element %nin% "Yield", ]

        df <- df %>% arrange(itemcode, areacode, year, element)

        df$areacode <- as.numeric(df$areacode)
        df$itemcode <- as.numeric(df$itemcode)
        df$year <- as.numeric(df$year)

        skip_cty <- sort(c(96, 214, 128, 41, 277))

        skip_cty_names <- unique(df[df$areacode %in% skip_cty,]$area)

        cat("---- Removing data for countries: ", skip_cty_names, sep = "\n")
        df <- df[df$areacode %nin% skip_cty, ]

        df$value <- df$value / 1e3
        df$unit <- paste0("000 ", df$unit)

        df$element <- as.factor(df$element)
        df$element <- factor(df$element,
                             levels = c("Area harvested", "Production"),
                             labels = c("area", "prod"))

        df <- df[, names(df) %nin% "unit"]

        df <- df %>%
            pivot_wider(names_from = year,
                        values_from = value,
                        names_glue = "y{year}")

        drop_itemcodes <- c(30, 162, 217, 226, 256, 263, 275, 299,
                            331, 446, 692, 723, 777, 813, 311, 51,
                            165, 222, 237, 257, 265, 277, 305, 334,
                            564, 693, 748, 771, 826, 675, 60 , 210,
                            224, 244, 258, 268, 281, 310, 336, 677,
                            698, 754, 800, 1242, 778, 101, 216, 225,
                            252, 261, 271, 290, 329, 414, 687, 702,
                            767, 809)

        skip_items <- unique(dfx[dfx$Item.Code..FAO. %in% drop_itemcodes,]$Item)

        cat("---- Removing data for items: ", sort(skip_items), sep = "; ")
        df <- df[df$itemcode %nin% drop_itemcodes, ]

        df <- df %>% replace_na(list(y2021 = 0,
                                     y2022 = 0,
                                     y2023 = 0))

        df <- df %>% arrange(itemcode, areacode, element)

        out <- df[, names(df) %nin% "area"]
    }

    if (data == "livestock") {
        fpath <- system.file("extdata",
                             "FAOSTAT_data_en_2-10-2025-livestock.csv",
                             package = "IMPOSTE")

        dfx <- df <- read.csv(file = fpath)

        names(df) <- tolower(names(df))

        names(df) <- gsub(pattern = "\\.\\.|\\.",
                          replacement = "",
                          x = names(df))

        skip_cols <- c("domaincode", "domain",
                       "yearcode",
                       "elementcode",
                       "flag", "flagdescription",
                       "note")

        df <- df[ ,!names(df) %in% skip_cols]

        names(df) <- gsub(pattern = "codefao",
                          replacement = "code",
                          x = names(df))

        df <- df %>% arrange(itemcode, areacode, year, element)

        df$areacode <- as.numeric(df$areacode)
        df$itemcode <- as.numeric(df$itemcode)
        df$year <- as.numeric(df$year)

        skip_cty <- sort(c(96, 214, 128, 41, 277))

        skip_cty_names <- unique(df[df$areacode %in% skip_cty,]$area)

        df <- df[df$areacode %nin% skip_cty, ]

        cat("---- Removing data for countries: ", skip_cty_names, sep = "\n")

        keepitems <- c(947, 951, 1130, 867 , 1058, 882,
                       1069, 1017 , 1020, 1073, 1035,
                       1083, 977, 982, 1080, 1062, 1091)

        skip_items <- unique(dfx[dfx$Item.Code..FAO. %nin% keepitems,]$Item)
        cat("---- Removing data for items:\n", sort(skip_items), sep = "; ")
        df <- df[df$itemcode %in% keepitems, ]

        # FAOSTAT's entries for the element variable are not IMPACT-compliant,
        # so you need to make them so, also need to scale the values properly
        # For IMPACT
        #     - Livestock numbers are in units of 1000 head
        #     - Livestock production is in 1000 tonnes
        #     - Livestock yields are in units of hg/animal (= 0.1g/an = 100 mg/an)

        df <- df[df$unit %nin% c("No/An", "1000 No"), ] # We do not want to count number of eggs

        df <- df %>%
            mutate(value = ifelse(unit %in% c("An", "No/An", "t"),
                                  value/1000,
                                  value))

        df <- df %>%
            mutate(element = ifelse(unit %in% c("An",
                                                "1000 An"),
                                    "Number",
                                    element))

        df <- df %>%
            mutate(element = ifelse(unit %in% c("t", "1000 No"),
                                    "Prod",
                                    element))

        df <- df %>%
            mutate(element = ifelse(unit %in% c("100 g/An",
                                                "0.1 g/An",
                                                "100 mg/An"),
                                    "Yield",
                                    element))

        df <- df[, names(df) %nin% "unit", ]

        df <- df %>%
            pivot_wider(names_from = year,
                        values_from = value,
                        names_glue = "y{year}")


        df <- df %>% replace_na(list(y2021 = 0,
                                     y2022 = 0,
                                     y2023 = 0))

        df <- df %>% arrange(itemcode, areacode, element)

        out <- df[, names(df) %nin% "area"]
    }

    if (!return) {
        file <- paste0(entropy_folder,
                       "\\",
                       data,
                       "_",
                       format(Sys.time(), "%Y%m%d"),
                       ".xlsx")
        write_xlsx(x = out,
                   file = file)
        cat("file saved as:\n", file)
    }

    if (return) return(out)
}
