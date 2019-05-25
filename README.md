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
Although it's not a port, everything was written from scratch. Evil-Hop was built with Evil and Easy-Motion in mind.
So if you're coming to Emacs from Vim, Evil-Hop should be more comfortable.

Here is a more detailed comparison between Avy (left) and Evil-Hop (right):

Evil-Hop and EasyMotion both limit the number of key presses to the bare minimum, it's extremely rare
to have to go above 2 key presses. Also the closer to the cursor the less key presses required. As you can see
the jump locations are also as closer to what you would expect while running a given command than Avy, because
Evil-Hop builds its location list by actually running the original command, so there are far less surprises between
where you want to go and the locations provided.

Word start motion.

![1](https://user-images.githubusercontent.com/33631407/58362324-76035680-7ec8-11e9-96f7-d4ca48017f93.jpg)

Evil-Hop and EasyMotion both build its jump keys from the cursor starting point, not from the top of the window.

![2](https://user-images.githubusercontent.com/33631407/58362323-76035680-7ec8-11e9-8c57-8361224a18d4.jpg)

Evil-Hop tried to work with any motion command by default, here is Evil and Vim's `ge` command, something Avy
doesn't have unless you add it yourself.

![3](https://user-images.githubusercontent.com/33631407/58362322-76035680-7ec8-11e9-9e24-784d34ce3059.jpg)

Evil-Hop will try to work with any motion command, you don't need to extend it in any way. Here is motion for
`l`.

![4](https://user-images.githubusercontent.com/33631407/58362321-756ac000-7ec8-11e9-8957-bc10348f0f4d.jpg)

To "extend" Evil-Hop you only need to create a new motion.

Here is Evil-Hop running a binding. `"K" #'(lambda () (interactive) (evil-previous-line 3))`

![5](https://user-images.githubusercontent.com/33631407/58362320-756ac000-7ec8-11e9-9b2d-1986990e7fde.jpg)

## Does Evil-Hop work if I don't use Evil?
So far, yes. But that's by accident, not design. My primary goal is to extend Evil.

## Are all of the Easy-Motion features supported?
Not yet, but I think the most important ones are. If you have a feature request please add an Issue.


## Sounds good, I'm giving it a go!
: )