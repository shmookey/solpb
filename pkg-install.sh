#!/bin/bash
# install.sh -- install script for use in solpb binary packages
# accepts an install prefix as an argument, otherwise installs to /usr/local


VERSION=$(cat VERSION)
DEST=${1:-/usr/local/share/solpb/$VERSION}

echo "Installing solpb to $DEST"

echo "#!/bin/bash" > uninstall.sh
echo "# solpb uninstall script" >> uninstall.sh
echo "set -x" >> uninstall.sh
echo "rm -rf $DEST" >> uninstall.sh
chmod +x uninstall.sh

mkdir -p $DEST
cp -r * $DEST
rm $DEST/install.sh

if [ -z "$HOMEBREW_CELLAR" ]
then
  ln -s $DEST/bin/solpb /usr/local/bin/solpb
  echo "rm /usr/local/bin/solpb" >> uninstall.sh
  echo "Created symlink: /usr/local/bin/solpb"
else
  echo "Homebrew install detected."
fi

cp uninstall.sh $DEST/uninstall.sh

echo "Uninstaller saved at $DEST/uninstall.sh"

echo "Done."

