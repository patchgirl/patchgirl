buildAndInstall: fetch
	cd back/; make app; cd ../front/; make all; cd ../back/; make install;

fetch:
	clear; echo "fetching latest source"; git pull
