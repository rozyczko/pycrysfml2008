macro(add_suffix rootlist suffix)

  set(outlist)
  foreach(root ${${rootlist}})
    list(APPEND outlist ${root}${suffix})
  endforeach()
  set(${rootlist} ${outlist})

endmacro(add_suffix)
