#' readFoodBalances
#'
#' @param src Source
#'
#' @return FAOSTAT sua table
#' @export
#'
#' @examples
#' \dontrun{
#' readFoodBalances()
#' }
readFoodBalances <- function(src, data = "sua", write = FALSE) {

    # Its called "SUA" but actually reads food balances data

    if (write) return <- FALSE
    if (!write) return <- TRUE

    fb <- read.csv(src)

    df <- fb

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

    keepitem <- c(2615, 2513, 2546, 2731, 2630, 2532, 2633, 2744,
                  2552, 2732, 2514, 2848, 2517, 2515, 2516, 2520,
                  2534, 2551, 2642, 2616, 2572, 2576, 2577, 2733,
                  2531, 2734, 2807, 2574, 2558, 2571, 2573, 2557,
                  2518, 2555, 2611, 2612, 2613, 2614, 2618, 2619,
                  2537, 2536, 2542, 2533, 2635, 2617, 2620, 2560,
                  2561, 2563, 2570, 2575, 2578, 2580, 2586, 2579,
                  2601, 2602, 2605, 2640, 2641, 2511, 2535)

    skip_items <- unique(df[df$item %nin% keepitem,]$item)
    cat("---- Removing data for items:\n", sort(skip_items), sep = "; ")
    df <- df[df$itemcode %in% keepitem, ]

    df <- df %>%
        pivot_longer(cols = c("y2020", "y2021", "y2022"),
                     values_to = "value",
                     names_to = "year")

    df$element <- tolower(df$element)
    df$element[df$element %in% "stock variation"] <- "stocks"
    df$element[df$element %in% "domestic supply quantity"] <- "domestic_supply"
    df$element[df$element %in% "export quantity"] <- "export_quantity"
    df$element[df$element %in% "import quantity"] <- "import_quantity"
    df$element[df$element %in% "other uses (non-food)"] <- "other_demand"

    df <- df %>% replace_na(list(value = 0))

    df <- df %>% arrange(itemcode, areacode, element)

    df$year <- gsub(pattern = "y", replacement = "", x = df$year)
    df$year <- as.numeric(df$year)

    df <- df %>%
        pivot_wider(names_from = element,
                    id_cols = c("areacode", "itemcode", "year"),
                    values_from = value,
                    values_fill = 0)
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
