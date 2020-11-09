# we start by defining variables
!define APPNAME "PatchGirl - Runner"
!define COMPANYNAME "PatchGirl"

!define EXECUTABLE_NAME "patchgirl-runner-exe.exe"
!define ASSETS "assets"
!define ICON_NAME "icon.ico"

!define VERSIONMAJOR 1
!define VERSIONMINOR 0
!define VERSIONPATCH 0

# create a directory where our assets will lay (eg: image, executable, uninstaller, dependencies...)
InstallDir "$PROGRAMFILES\${COMPANYNAME}\${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONPATCH}" 

# Define the installer name
outFile "patchgirl-runner-installer.exe"

section "install"
	setOutPath $INSTDIR

    # copy the executable and its assets in the installation directory
    file /r "${ASSETS}\"

	# create a desktop shortcut
    createShortCut "$DESKTOP\${APPNAME}.lnk" "$INSTDIR\${EXECUTABLE_NAME}" "" "$INSTDIR\${ICON_NAME}"
sectionEnd