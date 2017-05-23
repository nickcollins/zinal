# What?

Zinal (Zinal Is Not A Language) is a PoC [projectional-editor](https://martinfowler.com/bliki/ProjectionalEditing.html)/language-alternative demonstrably capable of self-hosting.

As a projectional editor, it allows a programmer to read and edit software via a GUI that is not necessarily a 1:1 reflection of
the logic's on-disk representation. Conventional editors (and IDEs) are text editors that store logic in text files and
represent the logic to the programmer as a character matrix (tho often with a good deal of sugaring). But zinal stores logic in a
binary database format (currently sqlite3, subject to change) and presents the user with a structured, tree-oriented interface
for viewing and editing the logic.

As a "Proof of Concept" (PoC), zinal is not the most mature or polished projectional editor, but unlike many projectional editors, zinal aims to be completely
general-purpose and is demonstrably capable of self hosting - that is, the zinal repo contains a zinal db that zinal can compile into a zinal executable.

# Why?

## Why projectional editing?

A comprehensive explanation of the rationale for projectional editing would be rather long and complicated. I'm working on coalescing and/or creating a
solid body of work to explain and defend the benefits of projectional editing over traditional editing. Until then, my quick sells are:

___"Syntax is boilerplate"___ and ___"Graphs and strings are both equally capable of representing formal logic and data, but graphs can be more 'expressive'"___

## Why zinal?

I believe that projectional editing is more ideal and, in the long run, more efficient than traditional text-based editing. But in order to prove
that, a projectional editing environment would have to do everything the current unix environment does, and do it as good or better. There are a lot
of fundamental properties of this environment that are critical to its success, popularity, and efficacy, one of which is that it self hosts. The C
compiler was probably compiled from C source code, Vim source code is probably edited in Vim, and GNU/Linux distros are probably developed on GNU/Linux
machines. If there's any hope of establishing a projectional or text-free (or even text-agnostic) alternative to this text-oriented environment, the
new alternative will have to likewise be capable of hosting and developing itself. Conceptually, it goes without saying that doing so is _possible_, but
I wanted to prove that doing so is _feasible_. Hence zinal.

Of course, the ability to self-host is far and away not the only critical and fundamental
property of the traditional unix environment; there are many other properties that would need to be fulfilled which zinal is currently lacking. Zinal is
not meant to be the end product - it's intended to encourage the idea of projectional editing and to help prove that the cost of rebuilding the dev environment
from the ground up, based on new premises, is not prohibitive.

# Try it out!

_**Disclaimer**: Zinal is at a very early stage of development - I wanted to put it out there as soon as it was capable of self-hosting to at least get that across, and get some
feedback, but the current UI is very feature poor (it doesn't even have undo/redo) and is kludgy, slow, and sometimes awkward. These are problems with the current
implementation, not with projectional editors in general - see [MPS](https://www.jetbrains.com/mps/) or [Lamdu](http://www.lamdu.org/) for examples of projectional editors with
better UIs._

All that should be necessary to run zinal is to have [sqlite3](https://sqlite.org/download.html) and [Racket](https://racket-lang.org/download/) installed on your machine.
**Make sure you download Racket from their website**
and not via `apt-get`, as zinal requires Racket v. 6.8 or above, and the Ubuntu package is outdated. On Ubuntu, you can install sqlite3 via
```
sudo apt-get install sqlite3 libsqlite3-dev
```
After downloading and installing Racket and sqlite3, you can run
```
racket main.rkt [<zinal-db>]
```

***Windows:*** These instructions may not work out of the box on Windows - if Racket doesn't work after installing it, try copying `C:\Program Files\Racket\collects` to

`C:\Users\<YOUR_USER_NAME>\AppData\Roaming\Racket\6.9\collects`

## Viewing and editing a zinal db

Run `racket main.rkt example.db` - zinal will display the example db's "main module". Type "right" then "down" to select the module's first statement.
Zinal doesn't currently support mouse input, so you'll have to interact with it exclusively via the keyboard:
- Use the arrow keys (or `hjkl`) to navigate around the module. To expand or collapse a list, use shift in conjunction with left and right movement.
- To navigate to a different module, type `e`.
- To see the complete body of a definition, navigate to it and press `enter`.
- To add nodes, press `I`, `A`, `i`, `a`, or `o`, and to delete them press `d` (or `s` to replace).

The key-bindings are intended to feel familiar to vim users - but you can view or edit them in the `key-bindings.rkt` file.

## Compiling

Oddly, while everything else can only be done with the keyboard, I haven't decided on a good keyboard shortcut for compilation, so for now there isn't one :unamused:.
To compile `example.db`, click
the `compile` item in the `compile` menu (of course, if you made any changes to `example.db`, it'll have to be in a compilable state - if it's not, you can just close zinal and
`git checkout example.db` to get it back to its original state). A dialog will prompt you for the
output file, and after compilation that file will be an executable that you can run directly (i.e. don't invoke the executable with `racket`).

You can also compile straight from the command line, via `racket main.rkt -o <dest-file> example.db`. You should close zinal before running this to avoid concurrent db
access.

## Create your own program

To create a new db do:
```
racket main.rkt my-db.db
```
Zinal will prompt you to name the initial module - after that, type `I` and `enter` to create a new "unassigned" node, which should
get you started.
- You can type `s` on an unassigned node (or most other nodes) to turn it into something useful.
- Semantically, zinal is almost equivalent to Racket (which is a variant of Scheme), so you'll probably be creating a lot of "list"s.
- When you create a list, you have to then type `I` or `A` in order to add the first item to it (inside of a list, you can also use `i`, `a`, or `o`).
- Remember the key bindings can be found (and edited) in `key-bindings.rkt`.

Play around and create whatever logic you want - if you're not familiar with Scheme primitives and semantics,
check out `example.db` for some ideas.

Before you compile, the program must have a main module. Navigate to
the appropriate module and type "left" enough times until the whole module is selected, then type "right" _twice_ (the current ui is finicky), then press `m`. Now you can
compile normally.

# Self hosting

To see zinal self-hosting in action, run:
```
racket main.rkt -o zinal zinal.db
```
This may take 10-20 minutes (I know - I'm working on it). After compilation is done, you can use `zinal` the same way as `racket main.rkt`.
Try running `zinal zinal.db`. Here you can see the source logic from which `zinal` was compiled - it is similar (tho not identical)
to the Scheme logic found in the `.rkt` files. Try using `e` to switch to the `ui-styles` module, change a few colors or something (see
https://docs.racket-lang.org/draw/color-database___.html), compile to `my-zinal`, then run
`my-zinal zinal.db`. You should see that the style changes you made are now manifest in the custom version of zinal you just created. Imagine if the missing editor features
were implemented, version control was added, and the ui was made more fluent and elegant; the Scheme code could be completely forgotten, each iteration of zinal being developed
and compiled by the previous one.

`zinal.db` was created using `translate.rkt`, which can convert _some_ Racket files into zinal modules. It won't work on most Racket code, as it is very persnickety about what
it can accurately interpret. Even if it does work, there is a lot of manual effort left to do in order to make everything proper in the newly minted zinal db.

# Understanding the code

_**Disclaimer**: Some of the code is pretty rough, and some of it is not very well factored. In order for zinal to self-host, the zinal logic must use a minimal set of Racket
features - a lot of the boilerplate and quality issues could be fixed by using Racket macros (which zinal doesn't support) or various special syntax that zinal has no way of
modeling currently. Otherwise, some things are disorganized or poorly factored because in the long-run, they need a huge overhaul, so I don't want to expend effort on long-tail
refactorings before completely redesigining the ent layer (into a zinal macro system) from the ground up._

The zinal logic is pretty large and complex. The best place to start is probably `db.rkt` - it's the best documented, it's the interface between the two most complex
parts of zinal (the sql db impl and the ent layer), and it represents an abstract description of zinal's "grammar".

The zinal logic has three basic layers: the front-end GUI, a middle layer of entities that interface with both the GUI and the backend, and a backend storage layer:
- The front-end layer is pretty simple, mostly being a thin interface between the human and the entity layer. `main.rkt` is the entry-point. After doing basic start-up and
set-up, it enters a simple loop of accepting user input, sending the input to the ent layer, receiving a ui response from the ent layer, and then displaying the response to
the user. The response is a `zinal:ui:item%%` as defined in `ui.rkt`. `ui.rkt` declares an abstract, read-only, line-oriented tree structure. `main.rkt` has to do some somewhat
fancy stuff for the sake of navigation performance, but at its heart it just runs through the ui tree it receives from the ent layer and lays down its contents into a
line-based text display.
- The storage layer is declared in `db.rkt` and defined in `sql-db.rkt`. `db.rkt` declares a completely abstract interface to a Scheme-like (with OOP) logic storage
model. The caller to the interface interacts with the storage layer via handles to stored db elements. The only current implementation is `sql-db.rkt`. `sql-db.rkt` implements
the zinal tree structure in a sqlite3 database. The db has (for the most part) a distinct table for each element type. Most tables have an `id` for the unique key, amongst all
tables, for identifying that element. Lists are implemented as linked lists.
- The ent layer, contained entirely in `ents.rkt` is the largest, worst organized, most complex part of the code, and is poorly factored and barely documented. Being stuck
between the GUI and the storage, both of which are stateful, it has a lot of complex responsibility that it must perform performantly (enough).
 - The ent layer parses the db,
   partitioning the db tree into "cones" (which are like subtrees, but technically different - see the docs in `ents.rkt`), and creating an ent for each cone. Each ent then
   creates a corresponding ui cone,
   corresponding to the db cone that the ui must represent, but with a potentially different structure.
 - When the ent layer receives an event from the GUI, it sends the event to
   the currently selected ui item, which handles the event according to the event handler assigned to it by its ent. If the db was (potentially) affected by the event handling,
   the ent layer reparses the db, only creating new ents for parts of the db that would be partitioned differently after the reparse.
   This is not a mere performance optimization - if
   the db creates a new ent for every cone, the new ent will not retain the GUI state (like which node is selected or which lists are expanded) of the previous ent, causing
   parts of the GUI unrelated to the current action to reset.
   The reparse logic is rather complicated and has to jump through some hoops to make sure that the selected item remains correct.
   After event-handling and reparsing, the same UI root is returned back to the front-end.
 - `ents.rkt` contains the logic for dialogs that get clarifying input from the user.
 - In addition to all the aforementioned logic, `ents.rkt` contains all abstractions and implementations of the ents themselves, and also of all the ui items declared in
   `ui.rkt`.
 - In the long run, a macro system for zinal ought to be built, at which point all the actual ents can be stored in the database itself, making zinal's logic more powerful and
   abstract while also removing clutter and boilerplate from it.

# Contact and Contributing

If you want to contact me or are interested in contributing, just ping me at [gitter](https://gitter.im/zinal-IDE/Lobby?source=orgpage). I don't expect high volume of pull
requests right now so I don't have a clear idea about what I'm looking for in terms of contributions, but I'd love to hear any ideas you have so just let me know what you're
thinking and we can discuss it there.

# Prior work

As was previously noted, more mature projectional editors exist, such as [MPS](https://www.jetbrains.com/mps/) and [Lamdu](http://www.lamdu.org/), but also
many others (see https://news.ycombinator.com/item?id=13773813, for example). I was not familiar with these when I started this project; had I been I probably wouldn't have
created a new thing. By the time I became familiar with them, I was already far enough along with zinal that I felt it was best to see it through.

Given that more mature
projectional editors exist, the main point of zinal is to demonstrate an example of a projectional editor that can self-host. MPS might be capable of self-hosting with some
effort, but based on my current understanding that seems quite challenging to make work. Lamdu has self-hosting as a long-term goal, but isn't ready for it at time of writing.
I hope that demonstrating the closed
loop, and what it looks like, will help inspire interest in projectional editors in general, help prove that projectional editors are a viable alternative to traditional
editors, and encourage developers of existing projectional editors to consider or prioritize self-hosting.

# License

This project is licensed under the Apache 2.0 License - see the [LICENSE.txt](LICENSE.txt) file for details
