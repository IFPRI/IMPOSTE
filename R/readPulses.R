#' readPulses
#'
#' @param src Source
#'
#' @return FAOSTAT sua table for pulses (disaggregated)
#' @importFrom stringr str_extract_all
#' @importFrom collapse join
#' @import dplyr tidyr
#' @export
#'
#' @examples
#' \dontrun{
#' readPulses()
#' }
readPulses <- function(srcpath, data = "pulses", write = FALSE) {

    # Pulses (disaggregated) data from SUA

    if (write) return <- FALSE
    if (!write) return <- TRUE

    src <- "SUA_Crops_Livestock_E_All_Data"
    pulses <- read.csv(file = paste0(srcpath, "/", src, "/", src, ".csv"))

    df <- pulses

    names(df) <- tolower(names(df))

    names(df) <- gsub(pattern = "\\.\\.|\\.",
                      replacement = "",
                      x = names(df))

    keepcols <- c("areacode", "area",
                  "itemcode", "item",
                  "elementcode", "element",
                  "y2020", "y2021", "y2022")

    df <- df[, names(df) %in% keepcols]

    dropelements <- c(511, 664 , 674 , 684,
                      665, 5170, 5171, 645,
                      661, 671, 681)

    df <- df[df$elementcode %nin% dropelements, ]

    skip_cty <- sort(c(96, 214, 128, 41, 277,
                       unique(df$areacode)[unique(df$areacode) > 4500]))

    skip_cty_names <- unique(df[df$areacode %in% skip_cty,]$area)

    df <- df[df$areacode %nin% skip_cty, ]

    cat("---- Removing data for countries: ", skip_cty_names, sep = "\n")

    keepitem <- c(203, 213, 181, 212, 211, 210, 197,
                  205, 176, 187, 201, 195, 191)

    skip_items <- unique(df[df$item %nin% keepitem,]$item)
    cat("---- Removing data for items:\n", sort(skip_items), sep = "; ")
    df <- df[df$itemcode %in% keepitem, ]

    df <- df %>%
        pivot_longer(cols = c("y2020", "y2021", "y2022"),
                     values_to = "value",
                     names_to = "year")

    df$value <- df$value / 1e3

    remove_elements <- df %>%
        group_by(elementcode, element) %>%
        summarise(n = n())

    remove_elements$element <- tolower(remove_elements$element)

    remove_elements_vector <-
        remove_elements[!remove_elements$element %in% c("calories/year",
                                                        "fats/year",
                                                        "proteins/year",
                                                        "opening stocks",
                                                        "residuals",
                                                        "tourist consumption"),]

    df <- df[df$elementcode %in% unique(remove_elements_vector$elementcode), ]

    df$element <- tolower(df$element)

    df$element[df$element %in% "stock variation"] <- "stocks"
    df$element[df$element %in% "food supply quantity (tonnes)"] <- "food"
    df$element[df$element %in% "export quantity"] <- "export_quantity"
    df$element[df$element %in% "import quantity"] <- "import_quantity"
    df$element[df$element %in% "other uses (non-food)"] <- "other_demand"
    df$element[df$element %in% "loss"] <- "losses"
    df$element[df$element %in% "processed"] <- "processing"

    df <- df %>% replace_na(value = 0)

    df <- df %>% arrange(itemcode, areacode, element)

    df$year <- gsub(pattern = "y", replacement = "", x = df$year)
    df$year <- as.numeric(df$year)

    df <- df %>%
        pivot_wider(names_from = element,
                    id_cols = c("areacode", "itemcode", "year"),
                    values_from = value,
                    values_fill = 0)

    df$domestic_supply <- df$production + df$import_quantity - df$export_quantity - df$stocks

    df <- df[, c("areacode", "itemcode", "year",
                 "production", "stocks", "food", "feed",
                 "other_demand", "processing",
                 "import_quantity", "export_quantity",
                 "domestic_supply", "losses", "seed")]
    out <- df

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
