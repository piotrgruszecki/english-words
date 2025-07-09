# Tue Nov  1 12:07:15 2022 ------------------------------

library(data.table, crossword.r)

x_dt <- fread("../english-words/words_alpha.txt")
x_dt[, `:=` (length = stringr::str_length(a))]

x_dt[, .N, length][order(-length)]

x_dt[length == 3, .(a)][order(a)][stringr::str_detect(a, pattern = "^a"), a]

x_dt[length == 4, .(a)][order(a)][stringr::str_detect(a, pattern = "^a"), a] |>
    stringr::str_c(collapse = " ") |>
    writeLines(con = "a_4.txt")


w1 <- x_dt[length %in% c(3, 4)][sample(20), a]

cw <- Crossword$new(rows = 4, columns = 4)
cw$add_words(c("back", "nasa", "kick", "nuk", "ic", "sic"))
cw
cw$letters
cw$words
cw$density()

cw <- Crossword$new(rows = 5, columns = 5)
cw$add_words(w1)
cw


w2 <- x_dt[length %in% c(5)][, sample(x = a, size = 20)]
w2
cw <- Crossword$new(rows = 10, columns = 10)
cw$add_words(w2)
cw$words
cw$print()

w3 <- crossword.r::cw_wordlist_animal_en$words
cw <- Crossword$new(rows = 20, columns = 30)
cw$add_words(w3)
cw_dt <- data.table(cw)
cw$message()
cw_dt <- cw$letters |> as.data.table()
cw_a_dt <- cw_dt[, lapply(.SD, stringr::str_replace_all, pattern = "[A-Z]", replacement = "_"), .SD = c(1:32)]
cw_a_dt |> fwrite(file = "cw.csv")
cw$words

cw_instr <- cw$words
cw_words <- crossword.r::cw_wordlist_animal_en
setDT(cw_words)
cw_words[, `:=` (words = stringr::str_to_upper(words))]
setnames(cw_words, c("words", "clues"), c("word", "clue"))

setDT(cw_instr)
cw_instr$clue <- NULL

setkey(cw_words, word)
setkey(cw_instr, word)

cw_instr3_dt <- cw_instr[cw_words]
cw_instr2_dt <- cw_instr3_dt[, .SD, .SDcols = !c("word")]

cw_instr2_dt |> split(f = "length")
cw_instr2_lst <- split(x = cw_instr2_dt, by = "direction")

cw_instr2_lst$right <- cw_instr2_lst$right[order(col, row)][, .(col, row, length, clue)]
cw_instr2_lst$down <- cw_instr2_lst$down[order(row, col)][, .(row, col, length, clue)]


purrr::map2(.x = cw_instr2_lst, .y = glue::glue("{names(cw_instr2_lst)}.csv"), .f = fwrite)
cw_instr3_dt[order(row, col)] |> fwrite("solution.csv")

