if filereadable("Build.hs")
  setlocal makeprg=./Build.hs\ -j4
endif
