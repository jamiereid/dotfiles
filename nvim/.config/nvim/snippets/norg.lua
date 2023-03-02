return {
	s("meeting", fmt([[
*** {}
    Time: {}
    Attendees: [
	    {}
    ]

**** Goals / agenda
     -

**** Discussion notes
     - {}

**** Action items
     - ( ) ]],
	 {
		 i(1, "meeting title"),
		 t(os.date("%H:%M")),
		 i(2, "Person one"),
		 i(3)
	 })),

	s("journal", fmt([[
@document.meta
creation: {}
tags: [
	daily-note
]
@end

* Daily Log
{}
** Meeting Log

** Daily Checklist
*** Start of Day
	- ( ) TA Completed
	- ( ) Defer folder in email on todo list
	- ( ) unassigned queue tidy
*** End of Day
	- ( ) Flex completed
	- ( ) Migrate anything valuable in Working Memory to appropriate location
	- ( ) Delete any empty headers in daily note
	- ( ) Inboxes processed
]],
	{
		t(os.date("%Y-%m-%d %H:%M")),
		i(1),
	}))
}
