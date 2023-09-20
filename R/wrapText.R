#' Create new line for long string

#' This function identifies which strings have charaters greater than
#' the specified `nchar` value. If the string is greater than the
#' specified `nchar`, it checks if the nchar(th) character is a space.
#' If it is, a new line is created. If it isn't, it looks for
#' the next space and creates a new line there. This can be used
#' in legends when funding source names are longer than desired.
#'
#' @param string a character string or list of character strings
#' @param nchar the number indicating the position to create a
#'new line in a string
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @examples
#' \dontrun{
#'  # 1. Try a string less than 20 characters
#'  cat(wrapText("This is a test", debug=1))
#'
#'  # 2. Wrap when 20th character is a space
#'  cat(wrapText("1234567890123456789 1234"))
#'
#'  # 3. Wrap text when 20th character is not a space
#'  # and there is a space after
#'  cat(wrapText("ABCDEFGH IJKLMONP QRS TU W ZYZ"))
#'
#' # 4. wrap text when 20th character not space
#' and there is no space after
#' cat(wrapText("ABCDEF GHIJHLMOPDSDABBBBAAAASASASJH"))
#' }

wrapText <- function(string, nchar=20, debug=0) {
  if (debug > 0) {
    message("string is ", paste0(string, collapse=","), " and nchar is ", nchar)
  }
  final <- NULL
  for (i in seq_along(string)) {
    lab <- string[i]
    if (nchar(lab) > nchar) { # If more than nchar characters
      if (debug > 0) {
        message("string ", string[i], " has more than ", nchar, " characters" )
      }
      match <- substr(lab, nchar, nchar)
      if (match == " ") { # If the nchar value is not a space
          # split the label here
        if (debug > 0) {
          message("The ", nchar, "th character for ", string[i], " is a space.")
        }
        newString <- paste0(substring(lab, 1, nchar), "\n", substring(lab, (nchar+1), nchar(lab)))
        final[[i]] <- newString
        } else {
          if (debug > 0) {
            message("The ", nchar, "th character for ", string[i], " was not a space. It was ", match)
          }
          # split at the next space
          # Find the first space after the 20th character
          reg <- paste0(".{",nchar,"}\\s")
          match_positions <- gregexpr(reg, lab)
          # Check if a match was found
          if (match_positions[[1]][[1]] >= 0) {
            if (debug > 0) {
              message("A space was found after the nchar, which was not a space")
            }
            # Extract the position of the first space
            first_space_position <- match_positions[[1]][[1]] + 20
            newString <- paste0(substring(lab, 1, first_space_position), "\n", substring(lab, (first_space_position+1), nchar(lab)))
            final[[i]] <- newString
          } else {
            if (debug > 0) {
              message("It was necessary to find a space before nchar")
            }
            # We reached the end, so split at the space before 20th character
            # Find the space right before the 20th character
            sub<- substr(lab, 1, (nchar-1))
            space <- max(grep(" ", strsplit(sub, "")[[1]]))
            newString <- paste0(substring(lab, 1, space), "\n", substring(lab, (space+1), nchar(lab)))
            final[[i]] <- newString
          }
      }
    } else {
      # Label wasn't too long
      final[[i]] <- lab
    }
  }
  return(unlist(final))
}
