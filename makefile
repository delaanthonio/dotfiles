
install: install.conf.yaml
	./install

init: install

# Install pyenv
	$ curl -L https://raw.githubusercontent.com/pyenv/pyenv-installer/master/bin/pyenv-installer | bash
