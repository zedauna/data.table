library(data.table)
`%iscall%` = data.table:::`%iscall%`

# works to detect these numerics, but we need a way to further
#   distinguish valid vs invalid numerics -- even if we explicitly
#   write 0.0, it's not clear how to separate this from 0 in the AST or expression()
# TODO: investigate srcfile methods which come with parse(keep.source=TRUE)
has_int_as_numeric = function(e) {
  if (is.call(e)) {
    if (e[[1L]] == ':') {
      # 1:3 is fine, 1:x[2] is not
      return((is.language(e[[2L]]) && has_int_as_numeric(e[[2L]])) ||
               (is.language(e[[3L]]) && has_int_as_numeric(e[[3L]])))
    }
    return(any(sapply(e[-1L], has_int_as_numeric)))
  }
  # trunc(x) == x will be NA for NaN / NA
  #if (is.double(e) && is.finite(e) && trunc(e) == e) browser()
  return(is.double(e) && is.finite(e) && trunc(e) == e)
}

has_quoted_Call = function(e) {
  if (is.call(e)) {
    if (e[[1L]] == '.Call') return(is.character(e[[2L]]))
    return(any(sapply(e[-1L], has_quoted_Call)))
  }
  return(FALSE)
}

has_plain_T_F = function(e) {
  if (is.call(e)) return(any(sapply(e[-1L], has_plain_T_F)))
  return(is.symbol(e) && e %chin% c('T', 'F'))
}

has_call = function(e, f) {
  if (is.call(e)) return(e[[1L]] == f || any(sapply(e[-1L], has_call, f)))
  return(FALSE)
}
has_ifelse = function(e) has_call(e, 'ifelse')
has_system.time = function(e) has_call(e, 'system.time')

re_encode = function(x) gsub(' +', '[\\\\s\\\\n]*', gsub('([.()])', '\\\\\\1', x))
pairlist_to_str = function(x) {
  paste(sprintf('%s[\\s\\n]*=[\\s\\n]*%s', names(x), sapply(x, deparse, USE.NAMES = FALSE)),
        collapse = ',[\\s\\n]*')
}
peel_body = function(e) {
  tmp = tempfile()
  cat(as.character(e), file = tmp)
  on.exit(unlink(tmp))

  drop_regex = sprintf(
    paste(
      '^', '%s', '%s', 'function\\(%s\\)', '\\{?', '|', '\\}?', '$', sep = '[\\s\\n]*'
    ),
    re_encode(e[[1L]][[2L]]),
    as.character(e[[1L]][[1L]]),
    re_encode(pairlist_to_str(e[[1L]][[3L]][[2L]]))
  )
  fl = paste(readLines(tmp, warn = FALSE), collapse = '\n')
  # restore }\nelse as } else;
  #   see https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17726
  re = '(.*)\\}\\s*\\n\\s*else(.*)'
  while (grepl(re, fl)) fl = sub(re, '\\1} else\\2', fl)
  writeLines(gsub(drop_regex, '', fl, perl = TRUE), tmp)
  parse(tmp, keep.source = TRUE)
}

for (f in list.files('R', full.names = TRUE)) {
  t = parse(f, keep.source = TRUE)
  idx = which(sapply(t, has_int_as_numeric))
  if (length(idx)) {
    for (expri in idx) {
      e = peel_body(t[expri])
      idx = sapply(e, has_int_as_numeric)
    }
  }
  idx = sapply(t, has_quoted_Call)
  if (any(idx)) {
    cat('Found Call(" in', f, '\n\n')
    print(t[idx])
  }
  idx = sapply(t, has_plain_T_F)
  if (any(idx)) {
    cat('Found T or F" in', f, '\n\n')
    print(t[idx])
  }
  idx = sapply(t, has_ifelse)
  if (any(idx)) {
    cat('Found ifelse" in', f, '\n\n')
    print(t[idx])
  }
}

for (f in list.files('inst/tests', full.names = TRUE, pattern = 'Rraw')) {
  t = parse(f, keep.source = TRUE)
  #idx = sapply(t, has_int_as_numeric)
  idx = sapply(t, has_quoted_Call)
  if (any(idx)) {
    cat('Found Call(" in', f, '\n\n')
    print(t[idx])
  }
  idx = sapply(t, has_plain_T_F)
  if (any(idx)) {
    cat('Found T or F in', f, '\n\n')
    print(t[idx])
  }
  idx = sapply(t, has_ifelse)
  # TODO: add logic to escape the intentional ifelse usage in 2085.33
  if (any(idx)) {
    cat('Found ifelse in', f, '\n\n')
    print(t[idx])
  }
  if (f == 'inst/tests/benchmark.Rraw') next
  idx = sapply(t, has_system.time)
  # TODO: add logic to escape intentional system.time usage in 819, 820
  if (any(idx)) {
    cat('Found system.time in', f, '\n\n')
    print(t[idx])
  }
  cat('---------------------------------------------\n\n\n')
}
