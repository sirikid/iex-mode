

# iex-mode

This is a simple comint wrapper around [IEx](https://hexdocs.pm/iex/IEx.html).


# Installation

Just drop the file to the `load-path` or use one of the methods below.


## quelpa

    (quelpa
     '(iex-mode
       :fetcher git
       :url "https://git.sr.ht/~sokolov/iex-mode"))


# Setup


## use-package

    (use-package iex-mode
      :bind
      (:map elixir-mode-map
        ("C-c x i" . run-iex)
        ("C-c x p" . projectile-run-iex)))


# Usage

You can run `iex` in the directory of current buffer with `M-x
  run-iex`.

You can also run `iex -S mix` in the project root with `M-x
  projectile-run-iex`.

In both cases you can edit the exact command to run.
