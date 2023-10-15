# surround

An Emacs package for inserting, changing, and, deleting surrounding pairs of quotes, braces,
etc.

This provides some of the functionality of vim-surround and evil-surround.  After using
evil-surround for a couple of years, I needed something similar when returning to native Emacs
keybindings.  There are a few similar packages, but none were quite as convenient as I wanted
and some do not seem to be maintained.

## Usage

This provides four major features:

- Surrounding the region or current symbol in pairs of characters like quotes or parentheses.

- Marking text in pairs of characters.  The term "marking" is used to mean setting the point
  and mark and activating the region.

- Killing text in pairs of characters without having to mark it first.

- Changing one pair of characters for another, such as changing from double quotes to single
  quotes.

### Pairs

The commands provided all accept a single pair character, either the left or right (aka open
and close).  The mode has a list of left and right pairs, and if you enter either it will
lookup the matching character.  For example, if you run the surround command and enter `(`, the
region will be surrounded with `(` and `)`.

These pairs are stored in the variable `surround-pairs` which you can change.  The default list
contains the following open and close pairs:

    ( )  { }  [ ]  < >

If you enter a character that is not in the list, that character will be used for both the left
and right.  So, using the surround command with `*` would surround the region with two
asterisks.

(This means the list really only needs to contain characters where the left and right are
different, unless you want a shortcut, discussed later.)

### Inner, Outer, and, Auto

Marking and killing come in two flavors, "inner" and "outer", which you may recognize from vi
or evil.  Inner means text within the pairs, but *not* the surrounding pairs themselves.  Outer
means the pairs are also included.

    (testing)
     -------     <-- inner
    ---------    <-- outer

There are also functions defined as "auto" which work as "inner" most of the time, but work as
"outer" if you enter a closing character.  For example, the kill command `k` normally kills
text within pairs, but if you enter `)` it will kill the parentheses also.

This is very handy as it allows the same command to be used for both inner and outer.
Unfortunately this can't be used for pairs where the left and right are the same, like quotes.
For these characters, the auto functions will always work as "inner".

### Key Bindings

The mode does not bind any keys.  It provides some commands which can be bound to keys, but the
recommended usage is to bind a single key to the provided keymap.  The rest of the
documentation will assume you are using this keymap.

I personally bind the keymap to "C-." so it is easy to access, and an example of this will be
shown in the installation section.

#### The Standard Commands

The keymap defines 7 standard keys for the different commands:

- s - surround the region or current symbol
- k - kill in pairs auto
- K - kill in pairs outer
- i - mark in pairs auto
- o - mark in pairs outer
- d - delete pair characters
- c - change pair characters

#### Shortcut Commands

The keymap also binds each of the characters from `surround-pairs`, both left and right, to a
mark command.  Left characters, including characters where left and right are the same (like
quotes), will be bound to "mark auto".  Right characters will be bound to "mark outer".  So
pressing `C-. (` will mark inside parentheses and `C-. )` will mark the parentheses also.

To provide shortcuts for quotes, they have been added to `surround-pairs` also, so `C-. "` will
mark inside quotes.

The keymap is generated from `surround-pairs`, so if you change the pairs list, you may need to
restart Emacs for shortcuts to be available for your new characters.  (If you are more familiar
with Emacs, you can use the provided function to rebuild the keymap instead.)

## Installation

The package is available on MELPA, so you can install it with `package-install`.  If you are
using `use-package`, put this into your init.el.  This snippet binds "C-." - be sure to change
that if you want a different key:

    (use-package surround
      :bind-keymap ("C-." . surround-keymap))

## Examples

The easiest way to explain the package is probably to see how it works with examples.  Starting
with the text below with the cursor on the `H`:

    Hello
    ^

To put quotes around the word, press `C-. s "`

    "Hello"
     ^

To change the quotes to parentheses: `C-. c " (`

    (Hello)
     ^
    
There are a couple of options for marking the text within the parentheses:

- `C-. i (` - Pass an opening parenthesis to the `i` command.
- `C-. (` - Use the opening parenthesis shortcut.

Both of these will mark within the parentheses, but not the parentheses themselves.

    (Hello)
     -----^

To mark the parentheses also, there are three options:

- `C-. i )` - Pass a closing parenthesis to the `i` command, which is really auto, not inner.
- `C-. o (` or `C-. o )` - Use the `o` command which is always outer, so you can pass either open
  or close.
- `C-. )` - Use the closing parenthesis shortcut.

The cursor will be after the closing parenthesis:

    (Hello)
    -------^

To delete the parentheses, deselect and move the cursor anywhere back into the parentheses,
such as to the first `l`:

    (Hello)
       ^

Now press `C-. d (` or `C-. d )`.  Open vs closed doesn't matter with this command - there is
no inner vs outer.

    Hello
      ^

Surround in curly braces using `C-. s {`

    {Hello}
      ^

To kill the text between the braces, leaving the braces, you would use `C-. k {`.  To kill the
braces and the text together, you'd use `C-. K {` or `C-. k }`  The `K` command is always outer.
The `k` command is an auto command, so passing a closing character will be a kill outer
command.

