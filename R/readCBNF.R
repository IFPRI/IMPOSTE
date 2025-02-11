#' readCBNF
#'
#' @param src Source
#'
#' @return FAOSTAT Commodity Balance for Non-Food (CBNF)
#' @importFrom stringr str_extract_all
#' @importFrom collapse join
#' @import dplyr tidyr
#' @export
#'
#' @examples
#' \dontrun{
#' readCBNF()
#' }
readCBNF <- function(srcpath, data = "cbnf", write = FALSE) {

    # Commodity Balance Non-Food

    if (write) return <- FALSE
    if (!write) return <- TRUE

    fpath <- system.file("extdata",
                         "old_itemcodes_nonfood.csv",
                         package = "IMPOSTE")

    extract_codes <- read.csv(fpath)

    oldcodes <- c(2590, 2591, 2592, 2593, 2594, 2595, 2596,
                  2597, 2598, 2661, 2662, 2663, 2664, 2665,
                  2666, 2667, 2671, 2672, 2815)

    extract_codes <- extract_codes[extract_codes$Item.Code %in% oldcodes, ]
    names(extract_codes)[1] <- "old_itemcode"

    extract_codes <- extract_codes %>%
        mutate(itemcode = str_extract_all(Description, "\\d+")) %>%  # Extract numbers
        unnest(itemcode)  # Expand into separate rows
    extract_codes <- extract_codes[,c(1, ncol(extract_codes))]

    # Now we need two tables, one for commodity balance non food
    # other is SUA
    # FAOSTAT decided to distribute these numbers in two different places

    src <- "CommodityBalances_(non-food)_(2010-)_E_All_Data"
    cbnf <- read.csv(file = paste0(srcpath, "/", src, "/", src, ".csv"))

    src <- "SUA_Crops_Livestock_E_All_Data"
    sua <- read.csv(file = paste0(srcpath, "/", src, "/", src, ".csv"))

    df <- rbind(cbnf, sua)

    names(df) <- tolower(names(df))

    names(df) <- gsub(pattern = "\\.\\.|\\.",
                      replacement = "",
                      x = names(df))

    keepcols <- c("areacode", "area",
                  "itemcode", "item",
                  "elementcode", "element",
                  "y2020", "y2021", "y2022")

    df <- df[, names(df) %in% keepcols]

    skip_cty <- sort(c(96, 214, 128, 41, 277,
                       unique(df$areacode)[unique(df$areacode) > 4500]))

    skip_cty_names <- unique(df[df$areacode %in% skip_cty,]$area)

    df <- df[df$areacode %nin% skip_cty, ]

    cat("---- Removing data for countries: ", skip_cty_names, sep = "\n")

    df <- df[df$itemcode %in% unique(extract_codes$itemcode), ]

    # add again "old codes"
    df <- join(df, extract_codes, on = "itemcode")

    # merge multiples in one
    df$itemcode <- df$old_itemcode
    df <- df %>%
        group_by(areacode, itemcode, area, elementcode, element) %>%
        summarise(y2020 = sum(y2020, na.rm = TRUE),
                  y2021 = sum(y2021, na.rm = TRUE),
                  y2022 = sum(y2021, na.rm = TRUE))

    keepitem <- c(2590, 2591, 2592, 2593, 2594, 2595, 2596,
                  2597, 2598, 2661, 2662, 2663, 2664, 2665,
                  2666, 2667, 2671, 2672, 2815)

    df <- df[df$itemcode %in% keepitem, ]

    df <- df %>%
        pivot_longer(cols = c("y2020", "y2021", "y2022"),
                     values_to = "value",
                     names_to = "year")

    df$value <- df$value / 1e3

    dropelements <- c(511, 664 , 674 , 684,
                      665, 5170, 5171, 645,
                      661, 671, 681)

    df <- df[df$elementcode %nin% dropelements, ]

    df$element <- tolower(df$element)

    remove_elements <- df %>%
        group_by(elementcode, element) %>%
        summarise(n = n())

    remove_elements_vector <-
        remove_elements[!remove_elements$element %in% c("calories/year",
                                                       "fats/year",
                                                       "proteins/year",
                                                       "opening stocks",
                                                       "residuals",
                                                       "tourist consumption"),]

    df <- df[df$elementcode %in% unique(remove_elements_vector$elementcode), ]

    df$element[df$element %in% "stock variation"] <- "stocks"
    df$element[df$element %in% "food supply quantity (tonnes)"] <- "food"
    df$element[df$element %in% "export quantity"] <- "export_quantity"
    df$element[df$element %in% "import quantity"] <- "import_quantity"
    df$element[df$element %in% "other uses (non-food)"] <- "other_demand"
    df$element[df$element %in% "loss"] <- "losses"
    df$element[df$element %in% "processed"] <- "processing"

    df <- as.data.frame(df)

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
