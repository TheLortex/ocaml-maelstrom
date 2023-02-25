# Fly.io distributed systems challenges - Gossip Glomers

An OCaml take on the distributed systems challenge using OCaml 5 and Eio. 
Maybe Irmin at some point.

Details on the challenge here: https://fly.io/dist-sys/

Networking is simulated using Maelstrom: https://github.com/jepsen-io/maelstrom. 
Nodes communicate using `stdin`/`stdout` which makes it easy to interoperate
with OCaml programs.

In this repo I provide `maelstrom`, a library that facilitates the communication
with Maelstrom by implementing the JSON-based protocol and providing some helpers.

In `bin/` my solutions to the challenge (currently 1, 2, 3a, 3b). 
