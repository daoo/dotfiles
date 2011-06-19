function! FoldCopyright(...)
  if !exists( "b:foldedCopyright" )
    let b:foldedCopyright = 1
    silent! 1/BEGIN LICENSE BLOCK/;/END LICENSE BLOCK/fold
  endif
endfunction
