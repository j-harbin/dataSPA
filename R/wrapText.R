#' Create new line for long string
#'
#' This function identifies which strings have charaters greater than
#' the specified `nchar` value. If the string is greater than the
#' specified `nchar`, it checks if the nchar(th) character is a space.
#' If it is, a new line is created. If it isn't, it looks for
#' the next space and creates a new line there. This can be used
#' in legends when funding source names are longer than desired.
#'
#' @param string a character string or list of character strings
#' to be dived by nchar times
#' @param nchar the number indicating the position to create a
#' new line in a string
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#'
#' @return a string with line breaks every nchar characters

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
#' @author Jaimie Harbin
#' @export

wrapText <- function(string, nchar=20, debug=0) {
  if (debug > 0) {
    message("string is ", paste0(string, collapse=","), " and nchar is ", nchar, '. The length of string is ', length(string))
  }
  final <- NULL
  for (i in seq_along(string)) {
    lab <- string[i]
    if (nchar(lab) > nchar) { # If more than nchar characters
      if (debug > 0) {
        message("string ", string[i], " has more than ", nchar, " characters" )
      }
      #browser()
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
            first_space_position <- match_positions[[1]][[1]] + nchar
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
  final2 <- NULL
  for (i in seq_along(final)) {
    if (nchar(final[[i]]) > nchar) {
    lab <- strsplit(final[[i]], "\n")[[1]][2]
    } else {
      lab <- string[[i]]
    }
    if (nchar(lab) > nchar) { # If more than nchar characters
      if (debug > 0) {
        message("string ", lab[i], " has more than ", nchar, " characters (round 2)" )
      }
      #browser()
      match <- substr(lab, nchar, nchar)
      if (match == " ") { # If the nchar value is not a space
        # split the label here
        if (debug > 0) {
          message("The ", nchar, "th character for ", lab[i], " is a space (round 2)")
        }
        newString <- paste0(substring(lab, 1, nchar), "\n", substring(lab, (nchar+1), nchar(lab)))
        final2[[i]] <- newString
      } else {
        if (debug > 0) {
          message("The ", nchar, "th character for ", string[i], " was not a space. It was ", match, " (round 2)")
        }
        # split at the next space
        # Find the first space after the 20th character
        reg <- paste0(".{",nchar,"}\\s")
        match_positions <- gregexpr(reg, lab)
        # Check if a match was found
        if (match_positions[[1]][[1]] >= 0) {
          if (debug > 0) {
            message("A space was found after the nchar, which was not a space (round 2)")
          }
          # Extract the position of the first space
          first_space_position <- match_positions[[1]][[1]] + nchar
          newString <- paste0(substring(lab, 1, first_space_position), "\n", substring(lab, (first_space_position+1), nchar(lab)))
          final2[[i]] <- newString
        } else {
          if (debug > 0) {
            message("It was necessary to find a space before nchar (round 2)")
          }
          # We reached the end, so split at the space before 20th character
          # Find the space right before the 20th character
          sub<- substr(lab, 1, (nchar-1))
          space <- max(grep(" ", strsplit(sub, "")[[1]]))
          newString <- paste0(substring(lab, 1, space), "\n", substring(lab, (space+1), nchar(lab)))
          final2[[i]] <- newString
        }
      }
    } else {
      # Label wasn't too long
      final2[[i]] <- lab
    }
  }
  # Determine if we need to split again
final3 <- NULL
for (i in seq_along(final)) {
  if (!(identical(final[[i]], final2[[i]]))) {
    final3[[i]] <- paste(substr(final[[i]], start = 1, stop = regexpr("\n", final[[i]]) - 1), paste0("\n", final2[[i]]))
  } else {
    final3[[i]] <- final[[i]]
  }
}
#browser()
  return(unlist(final3))
}
