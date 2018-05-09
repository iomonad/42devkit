##  42devkit.el [![HitCount](http://hits.dwyl.io/iomonad/42devkit.svg)](http://hits.dwyl.io/iomonad/42devkit)

<a href="https://github.com/iomonad/iomonad.el"><img
  src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Normalize Logo"
  width="80" height="80" align="right"></a>

  > Development mode/toolkit for 42 school

**Installation**
#### Clone the repo
```bash
git clone https://github.com/iomonad/42devkit ~/.emacs.d
```
#### Load file in your config
```elisp
;; Add 42devkit
(load "~/.emacs.d/42devkit/42devkit")
(require '42devkit)
```
## Features
* Fully configurable `norminette` wrapper.
* 'Normify' current buffer (wip)
* Automatic best practices on `c-hook`

## Norminette
> `Norminette` is a static code analyser to check **C/C++** source norm.
### Bindings
* `C-x n` analyze the current buffer folder
* `C-x C-n` analyze the buffer *only*
