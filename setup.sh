rm -rf ~/.gitignore ~/.gitconfig ~/.vimrc ~/.doom.d ~/.zshrc ~/.config/helix

ln gitignore ~/.gitignore
ln gitconfig ~/.gitconfig
ln vimrc ~/.vimrc
# ln -shf doom.d ~/.doom.d
ln zshrc ~/.zshrc
ln ripgreprc ~/.ripgreprc
ln starship.toml ~/.config/starship.toml
# TODO needs to be absolute pah
ln -s ./helix ~/.config/helix

cd doom.d
ln config.el ~/.config/doom/config.el
ln init.el ~/.config/doom/init.el
ln packages.el ~/.config/doom/packages.el
ln lib.el ~/.config/doom/lib.el
ln experimentals.el ~/.config/doom/experimentals.el
