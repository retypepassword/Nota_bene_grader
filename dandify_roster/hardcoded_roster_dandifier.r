library('xml2')
roster <- read_html(readline("Enter filename: "))
crns <- xml_find_all(roster, "//th[b='CRN']/../following-sibling::*[1]/td[2]")
get_items <- function(section_pos, item_name)
    xml_text(xml_find_all(roster, paste("(//th[b='Seq'])[", section_pos, "]/../following-sibling::tr[position() > 1 and position() < last() - 1]/td[", match(item_name, items) ,"]", sep = "")))
items <- c("Seq", "SID", "Last Name", "First Name", "Level", "Units", "Class", "Major", "Grade", "Status", "Status Date", "Email")
write.csv(Reduce(function(data, i) {
    if (length(get_items(i, "Seq")) == 0) return(data)
    rbind(data, data.frame(
        crn = xml_text(crns[i]),
        section = xml_text(xml_find_all(roster, paste("(//th[b='SEC'])[", i, "]/../following-sibling::*[1]/td[6]", sep = ""))),
        as.data.frame(sapply(items, FUN = function(item) { get_items(i, item) }, simplify = FALSE))
    )) }, seq_along(crns), data.frame(crn = numeric(0), section = numeric(0), as.data.frame(sapply(items, FUN = function(item) { numeric(0) } )))),
file = "roster.csv", row.names = FALSE, na = "")
