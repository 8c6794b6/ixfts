About ixfts
===========

Minimalistic full text indexer and searching web ui for html files,
using ixset and happstack.

After compiling with 'cabal configure && cabal build', invoke 'ixfts':

     $ ixfts --help

to get brief help for usage. Currently does not support unicode.

Quick example
=============

Sample session to show how to invoke html file indexer and web server.

    $ ls
    static
    $ ixfts index -d static -e html -o state
    Reading: static/foo.html
    Reading: static/bar.html
    Reading: static/buzz.html
    Creating index ....
    Done.
    $ ls
    state static
    $ ixfts serve -p 8000 -d state -s static
    Starting server with port: 8000

Access to http://localhost:8000/ and start searcihng. Note that,
initial query is slow since its loading index data to memory. From
second query, it is should be better.
