# My Common Lisp Warehouse

個人学習用

- 私の環境:
    - OS: Manjaro Linux
    - SBCL 2.1.7
    - rlwrapを入れています

## Usage

ASDFの設定を済ませた後、例えば `greeting`を使いたい場合、

```
$ rlwrap sbcl

* (asdf:load-system :greeting)
...
T
* (greeing:hello)
Hello, world!!
NIL
* (greeting:hello "SBCL")
Hello, SBCL!!
NIL
```

l99に関しては、もう一手間必要になります。

```
* (asdf:load-system :l99)
* ; 例えばworking-with-listsパッケージを使用する場合
* (in-package :working-with-lists)
....
....

* (cl-user::quit)
```
