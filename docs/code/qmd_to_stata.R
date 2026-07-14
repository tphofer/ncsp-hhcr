# ---------------------------------------------------------------------------
# qmd_to_stata.R
#
# Intent:
#   Convert a Quarto answer block (markdown prose + ```stata / ```{stata} code
#   fences) into a Stata program that, when run, prints the prose as SMCL and
#   executes the embedded Stata commands (echoing each command first).
#
#   Mapping rules:
#     - "#### Answer"            -> display as text _n "{bf:Answer to Problem N:}" _n
#     - prose paragraph          -> di as text "{p}{txt} ...{p_end}" _n
#                                   (soft-wrapped with /// continuations)
#     - inline `code`            -> {cmd: code}
#     - inline **bold** / *it*   -> {bf:..} / {it:..}
#     - inline [text](url)       -> {browse "url":text}
#     - every code line          -> di ". <line>"  then  <line>   (echo + run)
#     - a `more` pause is inserted before each code block after the first.
#
#   Code blocks echo EVERY line (setup lines such as `run ...` / `qui ...` are
#   emitted too; delete them by hand after conversion).
#
# Usage (primary):
#   1. Copy the answer block out of the .qmd to the clipboard.
#   2. source(here::here("code", "qmd_to_stata.R"))
#      -> it reads the clipboard, asks for a program name (and optional problem
#         number), and copies the finished Stata program back to the clipboard
#         (also echoing it to the console) so you can paste it into a do-file.
#   Re-run run_clip_conversion() any time to convert the next block.
#
# Usage (other entry points):
#   clip_to_stata(prog_name = "rpt_sbp", problem = 4)                      # -> console
#   clip_to_stata(prog_name = "rpt_sbp", problem = 4, output = "clipboard")# -> clipboard
#
#   # File-to-file (a file holding ONE answer block):
#   qmd_answer_to_do(
#     infile    = here::here("answers", "prob4.qmd"),
#     outfile   = here::here("code", "rpt_sbp.do"),
#     prog_name = "rpt_sbp",
#     problem   = 4
#   )
#
#   # In memory:
#   cat(convert_answer(readLines("prob4.qmd"), "rpt_sbp", 4), sep = "\n")
# ---------------------------------------------------------------------------

# Paths in the usage example use here::here() per project convention; the
# functions take explicit paths and have no hard dependency on `here`.

# Convert inline markdown spans to SMCL. -------------------------------------
md_inline_to_smcl <- function(text) {
  # links first so their () and [] are gone before other passes
  text <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", "{browse \"\\2\":\\1}", text, perl = TRUE)
  # code spans before emphasis so backticked * is left alone
  text <- gsub("`([^`]+)`", "{cmd: \\1}", text, perl = TRUE)
  text <- gsub("\\*\\*(.+?)\\*\\*", "{bf:\\1}", text, perl = TRUE)
  text <- gsub("\\*(.+?)\\*", "{it:\\1}", text, perl = TRUE)
  text
}

# Wrap one prose paragraph into a di-as-text statement. ----------------------
wrap_prose <- function(text, width = 70L) {
  text  <- md_inline_to_smcl(trimws(text))
  words <- strsplit(text, "\\s+")[[1]]

  # greedy word wrap
  segs <- character(0)
  cur  <- ""
  for (w in words) {
    if (!nzchar(cur)) {
      cur <- w
    } else if (nchar(cur) + 1L + nchar(w) <= width) {
      cur <- paste(cur, w)
    } else {
      segs <- c(segs, cur)
      cur  <- w
    }
  }
  if (nzchar(cur)) segs <- c(segs, cur)

  n <- length(segs)
  segs[1] <- paste0("{p}{txt} ", segs[1])
  segs[n] <- paste0(segs[n], "{p_end}")

  # compound quotes if the prose itself contains a double quote (e.g. a link)
  has_dq <- any(grepl("\"", segs, fixed = TRUE))
  qo <- if (has_dq) "`\"" else "\""
  qc <- if (has_dq) "\"'" else "\""

  if (n == 1L) {
    return(paste0("di as text ", qo, segs[1], qc, " _n"))
  }
  out <- paste0("di as text ", qo, segs[1], " ", qc, " ///")
  if (n > 2L) {
    out <- c(out, paste0("\t", qo, segs[2:(n - 1L)], " ", qc, " ///"))
  }
  out <- c(out, paste0("\t", qo, segs[n], qc, " _n"))
  out
}

# Echo + run every line of a code fence. -------------------------------------
convert_code_block <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  out   <- character(0)
  for (ln in lines) {
    ln <- sub("\\s+$", "", ln)
    if (grepl("\"", ln, fixed = TRUE)) {
      out <- c(out, paste0("\tdi `\". ", ln, "\"'"))   # compound quotes
    } else {
      out <- c(out, paste0("\tdi \". ", ln, "\""))
    }
    out <- c(out, paste0("\t", ln))
  }
  out
}

# Fence open detector: returns the language token, or NA if not a fence. -----
fence_lang <- function(line) {
  m <- regmatches(line, regexec("^\\s*```+\\s*\\{?([A-Za-z]*)", line))[[1]]
  if (length(m) == 0L) return(NA_character_)
  tolower(m[2])
}

is_fence <- function(line) grepl("^\\s*```+", line)

# Main: convert the markdown of ONE answer block to Stata program lines. -----
convert_answer <- function(md, prog_name = "rpt_answer", problem = NA, width = 70L) {
  if (length(md) == 1L) md <- strsplit(md, "\n", fixed = TRUE)[[1]]

  body        <- character(0)
  para        <- character(0)
  in_fence    <- FALSE
  fence_buf   <- character(0)
  fence_is_stata <- FALSE
  n_code      <- 0L

  flush_para <- function() {
    if (length(para) && any(nzchar(para))) {
      body <<- c(body, wrap_prose(paste(para, collapse = " "), width))
    }
    para <<- character(0)
  }

  for (line in md) {
    if (is_fence(line)) {
      if (!in_fence) {
        flush_para()
        in_fence  <- TRUE
        fence_buf <- character(0)
        lang      <- fence_lang(line)
        fence_is_stata <- is.na(lang) || lang == "" || lang == "stata"
      } else {
        # closing fence
        in_fence <- FALSE
        if (fence_is_stata) {
          n_code <- n_code + 1L
          if (n_code > 1L) body <- c(body, "more")
          body <- c(body, convert_code_block(fence_buf))
        } else {
          body <- c(body, paste0("* [skipped non-stata code block]"))
        }
      }
      next
    }
    if (in_fence) {
      fence_buf <- c(fence_buf, line)
      next
    }
    # outside a fence
    if (grepl("^\\s*#{1,6}\\s", line)) {
      flush_para()
      htext <- trimws(sub("^\\s*#{1,6}\\s+", "", line))
      # The "Answer" heading is emitted by the wrapper (after set more on), so
      # skip it here; other headings become bold subheadings.
      if (!grepl("^answer", htext, ignore.case = TRUE)) {
        body <- c(body, paste0("di as text _n \"{bf:", md_inline_to_smcl(htext), "}\" _n"))
      }
      next
    }
    if (!nzchar(trimws(line))) {
      flush_para()
    } else {
      para <- c(para, trimws(line))
    }
  }
  flush_para()

  header <- if (is.na(problem)) "{bf:Answer:}"
            else sprintf("{bf:Answer to Problem %s:}", problem)

  c(
    paste0("cap prog drop ", prog_name),
    paste0("prog define ", prog_name),
    "\tset more on",
    paste0("display as text _n \"", header, "\" _n"),
    body,
    "\tset more off",
    "end"
  )
}

# File-in / file-out convenience wrapper. ------------------------------------
qmd_answer_to_do <- function(infile, outfile, prog_name = "rpt_answer",
                             problem = NA, width = 70L) {
  md  <- readLines(infile, warn = FALSE)
  out <- convert_answer(md, prog_name = prog_name, problem = problem, width = width)
  writeLines(out, outfile)
  invisible(out)
}

# Clipboard helpers. ---------------------------------------------------------
# Use clipr if installed (cross-platform); otherwise fall back to the macOS
# pbpaste/pbcopy command-line tools.
read_clipboard <- function() {
  if (requireNamespace("clipr", quietly = TRUE)) return(clipr::read_clip())
  con <- pipe("pbpaste")
  on.exit(close(con))
  readLines(con, warn = FALSE)
}

write_clipboard <- function(text) {
  if (requireNamespace("clipr", quietly = TRUE)) {
    clipr::write_clip(text)
    return(invisible(text))
  }
  con <- pipe("pbcopy", "w")
  writeLines(text, con)
  close(con)
  invisible(text)
}

# Read a Quarto answer block from the clipboard, convert, and send the Stata
# program to the console (default), back to the clipboard, or to a file.
clip_to_stata <- function(prog_name = "rpt_answer", problem = NA, width = 70L,
                          output = c("console", "clipboard", "file"),
                          outfile = NULL) {
  output <- match.arg(output)
  md  <- read_clipboard()
  out <- convert_answer(md, prog_name = prog_name, problem = problem, width = width)
  switch(output,
    console   = cat(out, sep = "\n"),
    clipboard = {
      write_clipboard(out)
      message("Stata program copied to clipboard.")
    },
    file = {
      if (is.null(outfile)) stop("Provide outfile= when output = \"file\".")
      writeLines(out, outfile)
      message("Wrote ", outfile)
    }
  )
  invisible(out)
}

# Interactive driver: clipboard in -> prompt for name -> Stata program back to
# the clipboard (and echoed to the console).
# Pass prog_name/problem to skip the prompts (e.g. for scripting or testing).
run_clip_conversion <- function(prog_name = NULL, problem = NULL, width = 70L) {
  md <- read_clipboard()
  if (length(md) == 0L || !any(nzchar(md))) {
    message("Clipboard is empty -- copy a Quarto answer block first.")
    return(invisible(NULL))
  }
  if (is.null(prog_name)) prog_name <- trimws(readline("Program (wrapper) name: "))
  if (!nzchar(prog_name)) {
    message("No program name given; aborting.")
    return(invisible(NULL))
  }
  if (is.null(problem)) {
    pin     <- trimws(readline("Problem number (Enter to skip): "))
    problem <- if (nzchar(pin)) pin else NA
  }
  out <- convert_answer(md, prog_name = prog_name, problem = problem, width = width)
  write_clipboard(out)
  cat(out, sep = "\n")
  message("\n-- Stata program copied to clipboard --")
  invisible(out)
}

# When this file is sourced in an interactive session, run the flow at once.
if (interactive()) run_clip_conversion()
