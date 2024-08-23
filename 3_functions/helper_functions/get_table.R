# get_table
# 2024.03.09

# Wrapper for table() that has usaNA = always

get_table <- function(x) {
    table(x, useNA = 'always')
}
