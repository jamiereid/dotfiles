Your dotfiles are how you personalize your system. These are mine.

If you're interested in the philosophy behind why projects like these are awesome, you might want to [read Zach's post on the subject](http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/).

## install
Pull down the repository and then create symlinks using [GNU Stow](https://alexpearce.me/2016/02/managing-dotfiles-with-stow/).

```sh
git clone git@github.com:jamiereid/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
stow fish tmux  # and whatever else you'd like to use
```

## thanks
A lot of my config is taken from many different repos. The current layout and deployment is based on [alexpearce's dotfiles](https://github.com/alexpearce/dotfiles).
