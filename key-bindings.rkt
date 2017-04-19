; Copyright 2017 Nick Collins
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

; Similar to "#lang racket"
(module key-bindings racket

  (require "misc.rkt")

  (provide (all-defined-out))

  ; This file contains all the keybindings for zinal. Currently, zinal only accepts keyboard input
  ; (except for compilation, which can only be done by using the menu). This is pretty sad but I
  ; wanted to have a "keyboard-first" philosophy, so mouse and menu input is not really a priority.
  ; Each binding is a list of key combinations of that will invoke the command. The syntax for a key
  ; binding is described at the page for racket keymap% ( https://goo.gl/PlQbcl ). So for example, if
  ; you want either ctrl+shift+x or left arrow to invoke a command, the key-binding value would be
  ; '("c:X", "left") .
  ;
  ; Regarding key-binding conflicts:
  ; The ui presentation and behavior is mostly controlled in ents.rkt . The db tree is parsed in
  ; ents.rkt into an ent tree, and each node in the ent tree is responsible for a portion of the
  ; displayed ui tree. The ent layer is also responsible for interpreting input - each ent attaches
  ; an event handler to each ui node it creates, and when a command is issued it is dispatched to the
  ; event handler of the currently selected ui node. Therefore, for a particular ent, and a particular
  ; ui node created by that ent, there can't be two distinct commands that share a key combination.
  ; Otherwise, a key binding can be associated with several different commands. Currently, the ent
  ; logic is not very well organized, and it's not documented which commands might conflict with each
  ; other. If you want to change the keys bound to a command, your best bet is to not create a
  ; collision that doesn't already exist in the default bindings. ents.rkt checks for collisions
  ; when the ui tree is built and displayed, so if you do cause a collision, zinal will probably crash
  ; during or shortly after startup with a message identifying the conflicting key binding.

  ; Global bindings - these commands can be invoked anywhere

  ; commands for switching to or creating a different module/interface
  (define zinal:key-bindings:CHANGE-MODULE '("e"))
  (define zinal:key-bindings:CREATE-NEW-MODULE '("E"))
  (define zinal:key-bindings:CHANGE-INTERFACE '("c:e"))
  (define zinal:key-bindings:CREATE-NEW-INTERFACE '("c:E"))

  ; navigates to the next unassigned node in this module, wrapping around to the top if necessary
  (define zinal:key-bindings:GOTO-NEXT-UNASSIGNED '("t"))
  (define zinal:key-bindings:GOTO-PREV-UNASSIGNED '("T"))

  ; navigate around the ui tree. Note that the ui tree doesn't correspond exactly to the db tree:
  ; if parent is displayed horizontally (i.e., in a single line), then this moves to the previous
  ; sibling (if one exists). Otherwise, it moves to the parent
  (define zinal:key-bindings:MOVE-LEFT '("left" "h"))
  ; if the selected item has children, moves to the first child. otherwise, moves to the next
  ; sibling - if the item is the "oldest sibling", it will move to the youngest older aunt or cousin.
  ; In other words, this is a pre-order traversal
  (define zinal:key-bindings:MOVE-RIGHT '("right" "l"))
  ; Moves to the previous sibling of the first vertically displayed parent/ancestor
  ; (or to the parent). Basically, it moves up one line
  (define zinal:key-bindings:MOVE-UP '("up" "k"))
  ; Moves to the next sibling of the first vertically displayed parent/ancestor.
  ; Basically, it moves down one line
  (define zinal:key-bindings:MOVE-DOWN '("down" "j"))

  ; Local bindings - Some bindings can only be invoked at certain places in the ui-tree. See the
  ; documentation at the top of this file for more explanation.

  ; expands the selected ui list so that it (and all ancestors) are displayed vertically.
  (define zinal:key-bindings:EXPAND '("s:left" "H"))
  ; collapses the selected ui list so that it is displayed horizontally (i.e. in a single line)
  (define zinal:key-bindings:COLLAPSE '("s:right" "L"))

  ; In zinal, some definition nodes are displayed in a "short" manner when viewed from their parent.
  ; I.e., if the list you're viewing includes some function definitions, for each definition you'll
  ; only see a brief, one line "define <name> = λ: <parms>", rather than the whole function body. When
  ; the short definition display is selected, this command navigates to a new view that shows you
  ; the complete definition, including its body.
  (define zinal:key-bindings:VIEW-DEFINITION '("enter"))
  ; When viewing a complete definition in its own view, use this command to return to viewing the
  ; definition's short display in the context of its parent
  (define zinal:key-bindings:RETURN-TO-PARENT '("enter" "backspace"))

  ; commands for inserting items into lists. Note that different types of lists may support some of
  ; these but not others, and the exact behavior may differ slightly in different cases.
  ; If the selected item is a list, inserts a new element at the beginning of the list. Otherwise,
  ; inserts a new element at the beginning of the parent list
  (define zinal:key-bindings:INSERT-FIRST '("I"))
  ; If the selected item is a list, inserts a new element at the end of the list. Otherwise,
  ; inserts a new element at the end of the parent list
  (define zinal:key-bindings:INSERT-LAST '("A"))
  ; inserts a new element before this one in the parent list
  (define zinal:key-bindings:INSERT-BEFORE '("i"))
  ; inserts a new element after this one in the parent list
  (define zinal:key-bindings:INSERT-AFTER '("a"))
  ; inserts a new element after this one in the parent list - unlike the previous commands, which
  ; tend to prompt the user for the type of the new item, this one just makes the new item unassigned
  ; without prompting
  (define zinal:key-bindings:INSERT-UNASSIGNED-AFTER '("o"))

  ; If possible, deletes or unassigns the selected item, depending on context. Typically, an
  ; unassigned ode is completely deleted, whereas other nodes are turned into unassigned nodes, but
  ; the exact behavior depends on context. A message may appear if the item cannot be deleted or
  ; unassigned, depending on context
  (define zinal:key-bindings:DELETE/UNASSIGN '("d"))

  ; If possible, replaces the current item with something else, prompting the user as necessary.
  (define zinal:key-bindings:REPLACE '("s"))

  ; When a reference or referable is selected, navigates to the next corresponding occurrence (whether
  ; reference or referable occurrence) in this module (wrapping around if necessary). For next, the
  ; search order is a pre-order traversal of the tree, and prev is the exact opposite order of next.
  (define zinal:key-bindings:GOTO-NEXT '("*"))
  (define zinal:key-bindings:GOTO-PREV '("#"))

  ; Hyper-specific bindings - these bindings are only applicable in very particular cases.

  ; Changes whether or not the current definition is public. Some types of definitions, which are
  ; direct children of a module, can be made public and thus visible outside the module. This toggles
  ; publicity
  (define zinal:key-bindings:MODIFY-ACCESS '("m"))

  ; Changes whether or not the current module is main. Currently, this can only be used when the
  ; module's "header" is selected - it can be a bit finicky, so you may have to press left or right
  ; a few times to get the correct node selected in order to issue this command.
  (define zinal:key-bindings:TOGGLE-MAIN '("m"))

  ; Changes whether or not the current legacy override is an augment.
  (define zinal:key-bindings:TOGGLE-AUGMENT '("a"))

  ; When the first optional param (or last required param) is selected, changes it to be required
  ; (or optional)
  (define zinal:key-bindings:MAKE-REQUIRED '("r"))
  (define zinal:key-bindings:MAKE-OPTIONAL '("o"))

)
