let g:haskell_enable_pattern_synonyms = 1

setlocal makeprg=cabal\ build\ --ghc-options=\"-fno-code\ -Wall\"
let &errorformat = join([
  \ '%A%f:%l:%c:',
  \ '%A%f:%l:%c: %m',
  \ '%+C    %m',
  \ '%-Z%[%^ ]',
  \ ], ',')
