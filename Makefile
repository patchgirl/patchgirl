buildAndInstall:
	git pull; cd back/; make app; cd ../front/; make all; cd ../back/; make install;
