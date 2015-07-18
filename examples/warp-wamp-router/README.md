## Proof of concept WAMP router

WAMP v2 Basic Profile *Router* over WebSocket transport with JSON encoding.

## How to run

Use the same sandbox you installed `wamp` into:

    cabal sandbox init --sandbox <sandbox-path>

And then:

    cabal run
    
Glance through [AutobahnJS](http://autobahn.ws/js/) code in `static/client.js` to see what is expected to happen.
Visit <http://localhost:3000> and watch browser console.
