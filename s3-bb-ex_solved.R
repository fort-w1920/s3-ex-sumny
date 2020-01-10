new_bb <- function(text) {
  ### replace ((consonants) (1-2 vowels) (consonants)) with ((consonants)
  ### (vowels) b (same vowels again) (consonants)):
  matches <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  gsub(pattern = matches, replacement = "\\1\\2b\\2\\3", x = text)
}

bb <- function(object) {
  UseMethod("bb")
}

bb.default <- function(object) {
  if (!inherits(object, what = "character") && !is.character(object)) {
    stop(bb("non-character objects cannot be turned into bb-objects!"))
  }
  bebe <- new_bb(object)
  class(bebe) <- c("bb", class(bebe))
  bebe
}

bb.list <- function(object) {
  bebe <- lapply(object, bb)
  class(bebe) <- c("bb", class(bebe))
  bebe
}

### for factors and ordered factors, why should the order change as described
### above the test?
bb.factor <- function(object) {
  bebe <- factor(new_bb(object), ordered = is.ordered(object))
  class(bebe) <- c("bb", class(bebe))
  bebe
}
