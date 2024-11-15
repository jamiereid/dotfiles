return {
  s("dkey", { t "'", i(1), t "': i['", extras.rep(1), t "']," }),
  s("ibreakpoint", { t "from IPython import embed; embed()" }),
  s("ipdb", { t "import ipdb; ipdb.set_trace()" }),
}
