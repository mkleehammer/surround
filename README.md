# surround

An Emacs package for inserting, changing, and, deleting surrounding pairs of quotes, braces,
etc.

This provides some of the functionality of vim-surround and evil-surround.  After using
evil-surround for a couple of years, I needed something similar when returning to native Emacs
keybindings.  There are a few similar packages, but none were quite as convenient as I wanted
and some do not seem to be maintained.

## Overview

### Operations

There are five operations supported:

- Surrounding the region or current symbol in pairs of characters like quotes or parentheses.

- Deleting surrounding pairs but leaving the text inside.  That is, undoing a surround.

- Marking or selecting text within pairs of characters.  This can include the pairs or just be
  the text within.

- Killing text in pairs of characters without having to mark it first.  Like marking, this can

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
asterisks.  (This means the list really only needs to contain characters where the left and
right are different, unless you want a shortcut, discussed later.)

### Inner, Outer, and, Auto

Marking and killing come in two flavors, "inner" and "outer", which you may recognize from vi
or evil.  Inner means text within the pairs, but *not* the surrounding pairs themselves.  Outer
means the pairs are also included.

    (testing)
     -------     <-- inner
    ---------    <-- outer

There are also functions defined as "auto" which work as "inner" most of the time, but work as
"outer" if you enter a closing character.  For example, killing using `k (` kills text within
parentheses, but `k )` it will kill the parentheses also.

This allows the same command to be used for both inner and outer, but Unfortunately can't be
used for pairs where the left and right are the same, like quotes.  For these characters, the
auto functions will always work as "inner".

### Key Bindings

This package does not bind any keys.  It provides some commands which can be bound to keys, but
the recommended usage is to bind a single key to the provided keymap.  The rest of the
documentation will assume you are using this keymap.

Below, I'll assume you've chosen to bind `M-'` to the surround keymap.  Another good choice is
`C-c s`.

#### The Standard Commands

The keymap defines 7 standard keys for the different commands:

- s - surround the region or current symbol
- k - kill auto
- K - kill outer
- i - mark auto
- o - mark outer
- d - delete pair characters
- c - change pair characters

#### Shortcut Commands

The keymap also binds each of the characters from `surround-pairs`, both left and right, to a
mark command.  Left characters, including characters where left and right are the same (like
quotes), will be bound to "mark auto".  Right characters will be bound to "mark outer".  So
pressing `M-' (` will mark inside parentheses and `M-' )` will mark the parentheses also.

To provide shortcuts for quotes, they have been added to `surround-pairs` also, so `M-' "` will
mark inside quotes.

The keymap is generated from `surround-pairs`, so if you change the pairs list, you may need to
restart Emacs for shortcuts to be available for your new characters.  (If you are more familiar
with Emacs, you can use the provided function to rebuild the keymap instead.)

## Examples

The easiest way to explain the package is probably to see how it works with examples.  Starting
with the text below with the cursor on the `H`:

    Hello
    ^

### Surrounding

To put quotes around the word, use the "surround" command bound to 's'.  The command asks for
the character to surround the text with, so enter a double quote: `M-' s "`.

    "Hello"
     ^

To change the quotes to parentheses, use the "change" command bound to 'c'.  It will ask for
the character to replace, so enter a quote.  It will then ask for the character replace the
quotes with, so enter a parenthesis: `M-' c " (`

    (Hello)
     ^

### Marking

To mark (select) the text within the parentheses, you have a couple of options.  You can use
the "inner" command bound to 'i' or the shortcut bound do '(':

- `M-' i (` - Pass an opening parenthesis to the `i` command.
- `M-' (` - Use the opening parenthesis shortcut.

Both of these will mark within the parentheses, but not the parentheses themselves.

    (Hello)
     -----^

To mark the parentheses also, there are three options:

- `M-' o (` or `M-' o )` - Use the "outer" command bound to 'o'.  When it asks for the pair
  character, you can enter either an opening or closing parenthesis.

- `M-' i )` - The "inner" command will behave like "outer" if you pass it a closing parenthesis.

- `M-' )` - Finally, the shortcut command bound to ')'.

The cursor will be after the closing parenthesis:

    (Hello)
    -------^

Now let's delete the parentheses but leave the text "Hello".  First, get back into the
parentheses and deselect.  Move the cursor to the first 'l':

    (Hello)
       ^

Use the "delete" command bound to 'd'.  It will ask for the character to delete and you can
either either an open or closing parenthesis: `M-' d (` or `M-' d )`.

    Hello
      ^

Surround in curly braces using `M-' s {`

    {Hello}
      ^

To kill the text between the braces, leaving the braces, you would use `M-' k {`.  To kill the
braces and the text together, you'd use `M-' K {` or `M-' k }`  The `K` command is always outer.
The `k` command is an auto command, so passing a closing character will be a kill outer
command.

## Installation

The package is available on MELPA, so you can install it with `package-install`.  If you are
using `use-package`, put this into your init.el.  This snippet binds "M-'" - be sure to change
that if you want a different key:

    (use-package surround
      :ensure t
      :bind-keymap ("M-'" . surround-keymap))
