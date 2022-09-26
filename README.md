# Install
### Install Lisp
Install ```sbcl``` package

> Example for ArchLinux:
>```shell
>sudo pacman -S sbcl
>```
### Install quicklisp
```shell
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
* (quicklisp-quickstart:install)
* (quit)
```
# Run
```shell
sbcl --load snake.lisp
```
