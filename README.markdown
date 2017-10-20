Your dotfiles are how you personalize your system. These are mine.

If you're interested in the philosophy behind why projects like these are
awesome, you might want to [read Zach's post on the
subject](http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/).

## install

Run this:

```sh
git clone https://github.com/jamiereid/dotfiles.git ~/.dotfiles
cd .dotfiles
./dots put
```

This will symlink the appropriate files in `.dotfiles` to your home directory.
Everything is configured and tweaked within `~/.dotfiles`.

#### additional installs

##### fzf
From [junegunn's github page](https://github.com/junegunn/fzf#installation)

```sh
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
```

##### ag (the_silver_surfer)
[The Silver Surfer](https://geoff.greer.fm/ag/)

```sh
  apt-get install silversearcher-ag
  pacman -Syyu the_silver_searcher
  brew install the_silver_searcher
```

#### zsh

You'll want to have zsh installed and setup for your user. Install it, then run the following command.

``` bash
$ chsh -s $(which zsh)
```

Afterward, log out and log back in for the change to take effect. The first time you open a zsh shell, zsh antigen (a zsh package manager) will download the packages I use. Don't interrupt this process.

## thanks
A lot of my config is taken from many different repos. The current layout and deployment is based on [blaenk's dotfiles](https://github.com/blaenk/dots).
