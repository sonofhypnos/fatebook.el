# fatebook.el

## Installation 

### Doom Emacs

For users of [Doom Emacs](https://github.com/hlissner/doom-emacs):

1. Add the package to your `packages.el`. Open it and add the following:

```elisp
(package! fatebook
  :recipe (:host github
           :repo "sonofhypnos/fatebook.el"))
```

2. Run the `doom sync` command to update your package set.

3. After restarting Emacs or running `M-x doom/reload`, the package will be available for use.

### Spacemacs

For [Spacemacs](http://spacemacs.org) users:

1. Open your `.spacemacs` or `.spacemacs.d/init.el` and navigate to the `dotspacemacs-additional-packages` list.

2. Add the package with its repository details:

```elisp
(fatebook :location (recipe
                     :fetcher github
                     :repo "sonofhypnos/fatebook.el"))
```

3. Reload the configuration (`SPC f e R` or `M-m f e R`).

### Manual Installation

1. Clone the repository:

```bash
git clone https://github.com/sonofhypnos/fatebook.el.git
```

2. Add the directory to your `load-path`. In your `.emacs` or `init.el`, add:

```elisp
(add-to-list 'load-path "/path/to/fatebook.el")
```

Replace `"/path/to/fatebook.el"` with the actual path to the directory.

3. Load the package:

```elisp
(require 'fatebook)
```


## Storing your api-keys

TLDR: put the following line into `~/.authinfo.gpg` (or `~/.authinfo` if you ). Replace $your-api-key with your api key and leave the remaining fields the same.
``` sh
machine fatebook.io login defaultUser password $your-api-key
```

For the longer explanation see this section I stole from [(ghub)Getting Started](https://magit.vc/manual/ghub/Storing-a-Token.html)

> Please also see (auth)Top for all the gory details about Auth-Source.

> The variable auth-sources controls how and where Auth-Source keeps its secrets. The default value is a list of three files: ("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"), but to avoid confusion you should make sure that only one of these files exists and then you should also adjust the value of the variable to only ever use that file, for example:

> (setq auth-sources '("~/.authinfo"))

> In ~/.authinfo secrets are stored in plain text. If you don’t want that, then you should use the encrypted ~/.authinfo.gpg instead:

> (setq auth-sources '("~/.authinfo.gpg"))

> Auth-Source also supports storing secrets in various external key-chains. See (auth)Top for more information. 

> The default Auth-Source backends only support storing three values per entry; the "machine", the "login" and the "password".

If you want fatebook.el to be able to find your api key use `fatebook.io` for machine and `defaultUser` for user.




``` sh
machine fatebook.io login defaultUser password $your-api-key
```

replace $your-api-key with your api key and leave the remaining fields the same.


