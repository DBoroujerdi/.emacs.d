# .emacs.d

Based on blog post:
https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/

## Setup

Checkout to the standard emacs config location

```
git clone git@github.com:DBoroujerdi/.emacs.d.git
```

Or start from an arbitrary location

`env HOME=/Users/dbo01/projects/personal emacs -q --load ~/projects/personal/.emacs.d/init.el`

## LSP - Required Language Servers

Ensure all language servers are available on `PATH`

### Typescript

```
yarn global add typescript-language-server
```

### Golang

```
go get golang.org/x/tools/gopls@latest
```
