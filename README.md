# land-of-lisp-using-hunchentoot
Convert code for "Dice of Doom" from "Land of Lisp" to use Hunchentoot web server for version 3 and 4.

I recently went through "Land of Lisp" by Conrad Barski, a thoroughly enjoyable book! Unfortunately, I couldn't get the web server
in the book working, even when I tried the code from the website (to make sure I didn't fat finger something). I had previously
gone through "Lisp for the Web" by Adam Tornhill, where he used [Hunchentoot](https://edicl.github.io/hunchentoot/), and the callback
handlers seemed similar enough, so I went ahead and converted it.

A few notes:

1. I used [QuickLisp](https://www.quicklisp.org/) when implementing, then switched to [ASDF](https://common-lisp.net/project/asdf/); either
will work, use whatever you're comfortable with. I left the quicklisp code in, commented out, and ASDF seems to "just work" with less fuss,
probably because I'm on Debian and installed the `cl-hunchentoot` package.
2. There's not much difference in the handlers; I had to redirect the `*standard-output*` stream as in the original, but because Hunchentoot
appears to just grab returned strings and I'm new to Hunchentoot, I used a stringstream and returned a string from that at the end of the 
handler.
3. A lot of stuff is just taken care of by Hunchentoot (as any decent server side will do), so you don't need to gin up your own
`Content-type` or `HTTP/1.1` header (which was missing in the original "Dice of Doom" code). This includes path and parameter parsing, so 
`chosen` becomes a string if it's not `nil`.
4. I shamelessly stole some convenience functions for starting and stopping Hunchentoot; link is in the comment above them.

The relevant change is [here](https://github.com/npsimons/land-of-lisp-using-hunchentoot/commit/7f58d230589dcdefdd1948160caca53ed1eb96c3).

Beware! I am learning, so there may still be issues with this code. I also have to apologize for the sloppiness of the repository history.
I wasn't planning on releasing this publicly and it was mainly as a safety net and I was focused on learning the language, not Git. I swear
my shared repositories don't usually look this bad!
