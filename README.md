# cl-htmlprag
Neil Van Dyke's popular HTMLPrag library, ported to Common Lisp.

HTMLPrag provides two simple functions. The first is `html->shtml`, which parses
HTML, being permissive about HTML's rules:

     CL-USER> (htmlprag:html->shtml "<html><body>Some simple <p> malformed  <br> HTML</br>.</html>")
     (:*TOP* (:HTML (:BODY "Some simple " (:P " malformed  " (:BR) " HTML" "."))))
The second function is `shtml->html`, which reverses the parse and creates a string:

    CL-USER> (htmlprag:shtml->html '(ul (@ (class "list-class"))
				                                (li "A snippet of HTML")
				                                (li "It's impossible to construct a bad parse tree with lists.")))
    "<UL CLASS=\"list-class\"><LI>A snippet of HTML</LI><LI>It's impossible to construct a bad parse tree with lists.</LI></UL>"

For the full documentation, see the documentation for the [Racket version](https://planet.racket-lang.org/package-source/neil/htmlprag.plt/1/7/planet-docs/htmlprag/index.html).

This version of HTMLPrag was ported from the original Scheme version of HTMLPrag, from before it was adapted
to be compatible with Racket's immutable lists. Most of the code is still written in Scheme, with a compatibility
library to recreate those parts of Scheme that HTMLPrag needs.

In this source tree you will also find a port of Neil Van Dyke's Testeez testing library, which was used to
test the original HTMLPrag and to validate this port.
