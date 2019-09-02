# Emacs
![Screenshot](screenshot.png)

My personal Emacs configuration with contribution from friends.

Hopefully others might find this helpful. I don't believe in "starter-packs" such as
[oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh/), [prelude](http://batsov.com/prelude/) or
[Spacemacs](http://spacemacs.org/). I feel like the "one size fits all" approach leads to bloat and
means that users are not familiar with either the tool or the options the package configured for
them.

I mostly assembled bits and pieces over time as the need grew, but some sources served as
inspiration and deserve explicit credit:

* [emacs-fu](http://www.djcbsoftware.nl/dot-emacs.html)
* [magnars/.emacs.d](https://github.com/magnars/.emacs.d)
* [vmalloc/emacs](https://github.com/vmalloc/emacs)

Notes:
* Emacs 26.1 is required, older version will not work.
* For spell checking to work an ispell compatible program and dictionary need to be installed,
  e.g. `aspell-en`.
* By default `cmark` is used to markdown export/preview so it needs to be installed.

## Personal customizations

Users wishing to customize their configurations can use these files in `~/.emacs.site.d`:

1. `init.el` - loaded before any code is run but after `use-package` is available
1. `config.el` - loaded after configuration has finished

For example if one would like to change the theme and font they could do so by placing the following
in `~/.emacs.site.d/init.el`:

```elisp
(setq my/theme 'doom-solarized-light
      my/font-family "Consolas")
```

This directory can also be a git repository. For an example see https://github.com/drrlvn/.emacs.site.d.
