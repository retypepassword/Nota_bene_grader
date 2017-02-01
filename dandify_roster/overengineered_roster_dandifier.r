library('xml2')
roster <- read_html(readline("Enter filename: "))

crn_pos <- xml_find_num(roster, "count((//th[b='CRN'])[1]/preceding-sibling::*)+1")
crns <- xml_find_all(roster, paste("//th[b='CRN']/../following-sibling::*[1]/td[", crn_pos, "]", sep = ""))

sec_pos <- xml_find_num(roster, "count((//th[b='SEC'])[1]/preceding-sibling::*)+1")
sections <- xml_find_all(roster, paste("//th[b='SEC']/../following-sibling::*[1]/td[", sec_pos, "]", sep = ""))

get_pos <- function(header) {
    double_width_preceding <- xml_find_num(roster, paste("count((//th[b='", header, "'])[1]/preceding-sibling::th[@colspan = 2])", sep = ""))
    if (length(xml_find_all(roster, paste("(//th[b='", header, "'])[1]", sep = ""))) == 0)
        return(0)
    xml_find_num(roster, paste("count((//th[b='", header, "'])[1]/preceding-sibling::th)+1", sep = "")) + double_width_preceding
}

generate_xpath <- function(section_pos, item_name, item_pos) {
    xml_find_all(roster, paste("(//th[b='", item_name,"'])[", section_pos, "]/../following-sibling::tr[position() > 1]/td[", item_pos ,"]", sep = ""))
}

get_items <- function(section_pos, item_name) {
    if (positions[[item_name]] == 0) return()
    if (length(xml_find_all(roster, paste("(//th[b='", item_name, "' and @colspan = 2])[1]", sep = ""))) > 0) {
        items <- list(
                xml_text(generate_xpath(section_pos, item_name, positions[[item_name]])),
                xml_text(generate_xpath(section_pos, item_name, positions[[item_name]] + 1))
        )
        names(items) <- c(
            xml_text(xml_find_all(roster, paste("(//th[b='", item_name,"'])[1]/../following-sibling::tr[1]/th[", positions[[item_name]],"]", sep = ""))),
            xml_text(xml_find_all(roster, paste("(//th[b='", item_name,"'])[1]/../following-sibling::tr[1]/th[", positions[[item_name]] + 1,"]", sep = "")))
        )
        
        return(items)
    }
    xml_text(generate_xpath(section_pos, item_name, positions[[item_name]]))
}

items <- c("Seq", "SID", "Name", "First Name", "Level", "Units", "Class", "Major", "Grade", "Status", "Status Date", "Email")
positions <- sapply(items, FUN = get_pos)

data <- data.frame(crn = numeric(0), section = numeric(0), as.data.frame(sapply(items, FUN = function(item) { numeric(0) } )))
for (i in seq_along(crns)) {
    if (length(get_items(i, "Seq")) == 0) break

    class_data <- sapply(items, FUN = function(item) { get_items(i, item) } )
    class_data <- class_data[!sapply(class_data, is.null)]

    # Flatten list (remove lists within the list and add sublists to end of list)
    list_order <- NULL
    sublist_prev <- 0
    flattened <- class_data
    sublist_idxs <- which(sapply(class_data, is.list))
    for (idx in sublist_idxs) {
        sublists <- class_data[[idx]]
        list_order <- c(list_order, seq(sublist_prev, idx - 1), seq(length(flattened) + 1, length(flattened) + length(class_data[[idx]])))
        flattened <- as.list(c(flattened, sublists))
        sublist_prev <- idx + 1
    }
    list_order <- c(list_order, seq(sublist_prev, length(class_data)))

    # Reorder so that the sublists are where the nested list was
    class_data <- flattened[list_order]
    # Remove the nested lists (if it's still present for some reason)
    class_data <- class_data[!sapply(class_data, is.list)]

    # Truncate long items so that everything's the same length
    class_data <- lapply(class_data, function(item) { item[seq(1, min(sapply(class_data, length)))] } )

    data <- rbind(data, data.frame(
        crn = xml_text(crns[i]),
        section = xml_text(sections[i]),
        as.data.frame(class_data)
    ))
}
write.csv(data, file = "dandified_roster.csv", row.names = FALSE, na = "")
