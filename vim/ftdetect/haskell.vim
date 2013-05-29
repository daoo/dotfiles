fun! s:DetectRunhaskell()
  if getline(1) == '#!/usr/bin/env runhaskell'
    set filetype=haskell
  endif
endfun

autocmd BufNewFile,BufRead * call s:DetectRunhaskell()
