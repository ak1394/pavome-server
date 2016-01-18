tar xzvf faac-1.28.tar.gz
cd faac-1.28
./configure --prefix=/opt/pavo
make install
cd ..

tar xzvf faad2-2.7.tar.gz
cd faad2-2.7
./configure --prefix=/opt/pavo
make install
cd ..

tar xzvf openjpeg_v1_3.tar.gz
cd OpenJPEG_v1_3
make
make install
cd -

tar xzvf lame-398-2.tar.gz
cd lame-398-2
./configure --prefix=/opt/pavo
make install
cd -

tar xzvf opencore-amr-0.1.2.tar.gz
cd opencore-amr-0.1.2
./configure --prefix=/opt/pavo
make install
cd -

tar xjvf x264-snapshot-20091025-2245.tar.bz2
cd x264-snapshot-20091025-2245
./configure --prefix=/opt/pavo --disable-asm
make
make install
cd -

cd libavfilter
sh checkout.sh
cd ffmpeg
./configure --enable-gpl --enable-nonfree --enable-pthreads --enable-libfaac --enable-libfaad --enable-libx264 --enable-libopencore-amrnb --enable-libopencore-amrwb --enable-version3 --enable-libopenjpeg --enable-avfilter --extra-cflags=-I/opt/pavo/include --extra-ldflags=-L/opt/pavo/lib --prefix=/opt/pavo
make install
cd ../..

apt-get install libpng12-dev libjpeg62-dev
tar xzvf GraphicsMagick-1.3.7.tar.gz
cd GraphicsMagick-1.3.7
./configure --prefix=/opt/pavo
make install
cd -

tar xzvf gpac-0.4.5.tar.gz
cd gpac
chmod 755 configure
./configure --extra-cflags=-I/opt/pavo/include --extra-ldflags=-L/opt/pavo/lib --prefix=/opt/pavo
make
make install
cd -
