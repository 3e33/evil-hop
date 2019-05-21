# Evil-Hop
## What is it?
An Emacs package / extension / plugin / addon / \<invent a new term here> for Emacs that provides [Easy Motion](https://github.com/easymotion/vim-easymotion)-like functionality.

## How do I use it?
1. Download the evil-hop.elc file to whatever folder you like to put your packages / extensions / etc... in.
If you don't have a folder for that, create one and add it to the `'load-path`
for example

    `(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/"))`

    Alternatively Git Clone this repo, or use a package manager that works with Github because this is a new package and is not on Melpa.

1. Add `(require 'evil-hop)` to your Emacs init file.

1. Bind a key to `'evil-hop-entry` for example, if you're using [General](https://github.com/noctuid/general.el):
    ````
    (general-define-key :states '(normal visual operator)
                    "<SPC>" 'evil-hop-entry)
    ````
    
    With this you can press \<SPC> (space key) followed by almost any motion command, and it should just work for most things.
    Not everything is supported yet. If you desperately need something that isn't working, throw me an Issue.
    
    Alternatively you can bind each key and function yourself, for example like this:
    
    ````
    (my-evil-leader :states '(normal visual operator)
      "w" #'(lambda () (interactive) (evil-hop-hop 'evil-forward-word-begin)))
    ````

## How do I customise it?
`'evil-hop-highlight-keys` sets the keys that build the jump combinations.

`'evil-hop-highlight` is a face that you can change to set how the highlighted areas appear.
    
## Why not use [Avy](https://github.com/abo-abo/avy)?
Evil-Hop took its inspiration from Easy Motion, and tries to do things the same way.
Although it's not a port, everything was written from scratch. Avy's user friendliness is
in my opinion not as good as Easy Motion, and its compatibility with Evil is sometimes wonky.

Evil-Hop was built with Evil and Easy-Motion in mind. So if you're coming to Emacs from Vim, Evil-Hop should be more comfortable.

## Does Evil-Hop work if I don't use Evil?
So far, yes. But that's by accident, not design. My primary goal is to extend Evil.

## Are all of the Easy-Motion features supported?
Not yet, but I think the most important ones are. If you have a feature request please add an Issue.


## Sounds good, I'm giving it a go!
: )