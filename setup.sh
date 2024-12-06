rm -rf ~/.gitignore ~/.gitconfig ~/.vimrc ~/.doom.d ~/.zshrc

ln gitignore ~/.gitignore
ln gitconfig ~/.gitconfig
ln vimrc ~/.vimrc
# ln -shf doom.d ~/.doom.d
ln zshrc ~/.zshrc
ln ripgreprc ~/.ripgreprc
ln starship.toml ~/.config/starship.toml

cd doom.d
ln config.el ~/.config/doom/config.el
ln init.el ~/.config/doom/init.el
ln packages.el ~/.config/doom/packages.el
