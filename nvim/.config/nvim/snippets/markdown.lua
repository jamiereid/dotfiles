-- s = snippet
-- i = insert_node
-- t = text_node
return {
  s(
    "meeting",
    fmt(
      [[
### {}
    Time: {}
    Attendees: [ '{}' ]

#### Goals / agenda
     -

#### Discussion notes
     - {}

#### Action items
     - [ ]  ]],
      {
        i(1, "meeting title"),
        t(os.date "%H:%M"),
        i(2, "Person one"),
        i(3),
      }
    )
  ),
}, {}
