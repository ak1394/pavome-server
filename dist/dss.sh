wget http://static.macosforge.org/dss/downloads/DarwinStreamingSrvr6.0.3-Source.tar
wget http://dss.macosforge.org/trac/raw-attachment/ticket/6/dss-6.0.3.patch
wget http://dss.macosforge.org/trac/raw-attachment/ticket/6/dss-hh-20081021-1.patch 
wget http://dss.macosforge.org/trac/raw-attachment/ticket/6/darwin-streaming-server
wget http://dss.macosforge.org/trac/raw-attachment/ticket/6/Install

sudo addgroup --system qtss
sudo adduser --system --no-create-home --ingroup qtss qtss

tar -xvf DarwinStreamingSrvr6.0.3-Source.tar
mv DarwinStreamingSrvr6.0.3-Source DarwinStreamingSrvr6.0.3-Source.orig
patch -p0 < dss-6.0.3.patch
mv DarwinStreamingSrvr6.0.3-Source.orig DarwinStreamingSrvr6.0.3-Source
patch -p0 < dss-hh-20081021-1.patch
mv DarwinStreamingSrvr6.0.3-Source/Install  DarwinStreamingSrvr6.0.3-Source/Install.orig
cp Install DarwinStreamingSrvr6.0.3-Source/Install
chmod a+x DarwinStreamingSrvr6.0.3-Source/Install
cd DarwinStreamingSrvr6.0.3-Source

./Buildit
./Install
