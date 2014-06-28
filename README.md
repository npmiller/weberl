This project is some kind of web framework using only erlang to handle page
rendering as well as the http server. It can currently serve files in a directory
as well as serving directory listings. There's also a part that uses pandoc to render
Markdown files from a directory tree. A templating engine that uses erlang's eval also
somewhat works. To use it, you just type erlang inside specific tags in your html files
and the return of your erlang code will be displayed.

There is a ton of security issues with this project, it's just a toy to have fun while 
practicing erlang programming.
