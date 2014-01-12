# lord

Lord is a unified command line interface to several online radios.

By default, lord uses [mpd] as backend.
When mpd is available, lord can be run as daemon (default) or in foreground (with `--no-daemon` option).
When mpd is unavailable (not running), lord will fallback to use [mplayer]. Mplayer is run in foreground to allow user controlling playback with mplayer keybindings.

Supported radios:

- [8tracks.com]
- [cmd.fm]
- [radioreddit.com]
- [douban.fm]  Only in China?
- [jing.fm] Only in China?

Lord is on [hackage](http://hackage.haskell.org/package/lord), you can install with

    cabal install lord

## Commands

```
lord -h
lord status
lord toggle
lord kill

lord 8tracks listen [<mix_id> | <mix_url>] [--no-daemon]
lord 8tracks search <keywords>
lord 8tracks [featured | trending | newest]

lord cmd listen <genre> [--no-daemon]
lord cmd genres

lord douban listen [<channel_id> | <album_url> | <musician_url> | <musician_name>] [--no-daemon]
lord douban search <keywords>
lord douban [hot | trending]

lord jing listen <keywords> [--no-daemon]

lord reddit listen <genre> [--no-daemon]
lord reddit genres
```

### bash completion

> `optparse-applicative` has built-in support for bash completion of command line options and arguments.

With bash, add to your **.bashrc**:

    source <(lord --bash-completion-script `which lord`)

With zsh, add to your **.zshrc**:

    autoload bashcompinit
    bashcompinit
    source <(lord --bash-completion-script `which lord`)

NOTE: <code>lord --bash-completion-script \`which lord`</code> can generate the bash_completion file on the fly. However, you can also use the installed version. The bash_completion file is installed to somewhere like **~/.cabal/share/x86_64-linux-ghc-7.6.3/lord-2.20131220/bash_completion.d/lord**.

[8tracks.com]: http://8tracks.com
[cmd.fm]: http://cmd.fm
[radioreddit.com]: http://radioreddit.com
[douban.fm]: http://douban.fm
[jing.fm]: http://jing.fm
[MPD]: http://musicpd.org/
[mplayer]: http://www.mplayerhq.hu/
