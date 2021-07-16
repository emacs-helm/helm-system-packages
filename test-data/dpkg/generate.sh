apt-mark showmanual > list-explicit.dpkg
apt-mark showauto > list-dependencies.dpkg
dpkg --get-selections > list-residuals.dpkg
apt-cache pkgnames > list-names.dpkg
apt-cache search . > list-descriptions.dpkg
apt-cache show emacs > emacs-info.dpkg
dpkg --listfiles emacs > emacs-files.dpkg
apt-cache depends emacs > emacs-depends.dpkg 
apt-cache rdepends emacs > emacs-rdepends.dpkg 
