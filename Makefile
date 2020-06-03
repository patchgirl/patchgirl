buildAndInstall: fetch
	cd back/; make both; cd ../front/; make all; cd ../back/; make install;

fetch:
	clear; echo "fetching latest source"; git pull
