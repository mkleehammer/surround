# surround

An Emacs package for inserting, changing, and, deleting surrounding pairs of quotes, braces,
etc.

This provides some of the functionality of vim-surround and evil-surround.  After using
evil-surround for a couple of years, I needed something similar when returning to native Emacs
keybindings.

## Usage

This provides four major features:

- Surrounding the region or current symbol in pairs of characters like quotes or parentheses.

- Marking text in pairs of characters.  The term "marking" is used too mean setting the point
  and mark and activating the region.

- Killing text in pairs of characters without having to mark it first.

- Changing one pair of characters for another, such as chaining from double quotes to single
  quotes.

### Pairs

The commands provided all accept a single pair character, either the left or right (aka open
and close).  The mode has a list of left and right pairs, and if you choose either it will
lookup the matching character.  For example, if you run the surround command and choose '(',
the region will be surrounded with '(' and ')'.

These pairs are stored in the variable `surround-pairs` which you can change.  The default list
contains the following open and close pairs:

    ( )  { }  [ ]  < >

It also has single quotes, double quotes, and grave accents (`).

If you enter a character that is not in the list, that character will be used for both the left
and right.  So, using the surround command with '*' would surround the region with two
asterisks.

### Inner, Outer, and, Auto

Marking and killing come in two flavors, "inner" and "outer", which you may recognize from vi
or evil.  Inner means text within the pairs, but *not* the surrounding pairs themselves.  Outer
means the pairs are also included.

    (testing)
     -------     <-- inner
    ---------    <-- outer

There are also functions defined as "auto" which work as "inner" *unless* you enter a closing
character.  For example, the mark auto command normally marks within pairs, but if you enter
')' it will mark the outer pair characters also.

This is very handy as it allows the same command to be used for both inner and outer.
Unfortunately this can't be used for pairs where the left and right are the same, like quotes.
For these characters, the auto functions will behave like inner.

### Key Bindings

The mode does not bind any keys.  It provides some commands, which can be bound to keys.  But
the recommended usage is to bind a single key to the provided keymap.  The rest of the
documentation will assume you are using this keymap, but the functions you can bind yourself
will also be described.

I personally bind the keymap to "C-." so it is easy to access, and an example of this will be
shown in the installation section.

#### The Standard Commands

The keymap defines 7 standard keys for the different commands:

- s - surround the region or current symbol
- k - kill auto (explained below)
- K - kill outer
- i - mark auto
- o - mark outer
- d - delete pair characters
- c - change pair characters

#### Shortcut Commands

The keymap also contains each of the defined pair characters and will mark using the character.
The marking is configured for auto, so pressing `C-. (` will mark inside parentheses and
`C-. )` will mark the parentheses also.

Shortcuts are created for all pairs in the `surround-pairs` list, so the different quotes have
been added to it.  This means `C-. "` will mark inside quotes, for example.

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
with the text below with the cursor on the 'H':

    Hello
    ^

To put quotes around the word, press `C-. s "`

    "Hello"
     ^

To change the quotes to parentheses: `C-. c " (`

    (Hello)
     ^
    
There are a couple of options for marking the text within the parentheses:

- `C-. i (` - Pass an opening parenthesis to the 'i' command.
- `C-. (` - Use the opening parenthesis shortcut.

    (Hello)
     -----^

To mark the parentheses also, there are three options:

- `C-. i )` - Pass a closing parenthesis to the 'i' command, which is really auto, not inner.
- `C-. o (` or `C-. o )` - Use the 'o' command which is always outer, so you can pass either open
  or close.
- `C-. )` - Use the closing parenthesis shortcut.

The cursor will be after the closing parenthesis:

    (Hello)
    -------^

To delete the parentheses, deselect and move the cursor back into the parentheses, such as the
first 'l':

    (Hello)
       ^

Now press `C-. d (` or `C-. d )`.  Open vs closed doesn't matter with this command since there is
no inner versus outer versions.

    Hello
      ^

Surround in curly braces using `C-. s {`

    {Hello}
      ^

To kill the text between the braces, leaving the braces, you would use `C-. k {`.  To kill the
braces and the text together, you'd use `C-. K {` or `C-. k }`  The 'K' command is always outer.
The 'k' command is an auto command, so passing a closing character will be a kill outer
command.

