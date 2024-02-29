macro(add_prefix prefix rootlist)

  set(outlist)
  foreach(root ${${rootlist}})
    list(APPEND outlist ${prefix}${root})
  endforeach()
  set(${rootlist} ${outlist})

endmacro(add_prefix)
