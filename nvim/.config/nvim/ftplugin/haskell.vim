let hs_highlight_boolean    = 1
let hs_highlight_debug      = 1
let hs_highlight_delimiters = 1
let hs_highlight_delimiters = 1
let hs_highlight_types      = 1
let hs_highlight_more_types = 1

setlocal makeprg=cabal\ build\ --ghc-options=-Wall
let &errorformat = join([
  \ '%A%f:%l:%c:',
  \ '%A%f:%l:%c: %m',
  \ '%+C    %m',
  \ '%-Z%[%^ ]',
  \ ], ',')
